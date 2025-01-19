-- | This module provides 'PerformEventT', the standard implementation of
-- 'PerformEvent'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PerformEvent.Base
  ( PerformEventT (..)
  , FireCommand (..)
  , hostPerformEventT
  ) where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.Requester.Base
import Reflex.Requester.Class

import Control.Lens
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Semigroup as S

-- | A function that fires events for the given 'EventTrigger's and then runs
-- any followup actions provided via 'PerformEvent'.  The given 'ReadPhase'
-- action will be run once for the initial trigger execution as well as once for
-- each followup.
newtype FireCommand t m = FireCommand { runFireCommand :: forall a. [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m [a] } --TODO: The handling of this ReadPhase seems wrong, or at least inelegant; how do we actually make the decision about what order frames run in?

-- | Provides a basic implementation of 'PerformEvent'.  Note that, despite the
-- name, 'PerformEventT' is not an instance of 'MonadTrans'.
newtype PerformEventT t m a = PerformEventT { unPerformEventT :: RequesterT t (HostFrame t) Identity (HostFrame t) a }

deriving instance ReflexHost t => Functor (PerformEventT t m)
deriving instance ReflexHost t => Applicative (PerformEventT t m)
deriving instance ReflexHost t => Monad (PerformEventT t m)
deriving instance ReflexHost t => MonadFix (PerformEventT t m)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (PerformEventT t m)
deriving instance (ReflexHost t, MonadException (HostFrame t)) => MonadException (PerformEventT t m)
deriving instance (ReflexHost t, Monoid a) => Monoid (PerformEventT t m a)
deriving instance (ReflexHost t, S.Semigroup a) => S.Semigroup (PerformEventT t m a)
deriving instance (ReflexHost t, MonadCatch (HostFrame t)) => MonadCatch (PerformEventT t m)
deriving instance (ReflexHost t, MonadThrow (HostFrame t)) => MonadThrow (PerformEventT t m)
deriving instance (ReflexHost t, MonadMask (HostFrame t)) => MonadMask (PerformEventT t m)

instance (PrimMonad (HostFrame t), ReflexHost t) => PrimMonad (PerformEventT t m) where
  type PrimState (PerformEventT t m) = PrimState (HostFrame t)
  primitive = PerformEventT . lift . primitive

instance (Monad (HostFrame t), ReflexHost t, Ref m ~ Ref IO) => PerformEvent t (PerformEventT t m) where
  type Performable (PerformEventT t m) = HostFrame t
  {-# INLINABLE performEvent_ #-}
  performEvent_ = PerformEventT . requesting_
  {-# INLINABLE performEvent #-}
  performEvent = PerformEventT . requestingIdentity

instance ReflexHost t => Adjustable t (PerformEventT t m) where
  runWithReplace outerA0 outerA' = PerformEventT $ runWithReplaceRequesterTWith f (coerce outerA0) (coerceEvent outerA')
    where f :: HostFrame t a -> Event t (HostFrame t b) -> RequesterT t (HostFrame t) Identity (HostFrame t) (a, Event t b)
          f a0 a' = do
            result0 <- lift a0
            result' <- requestingIdentity a'
            return (result0, result')
  traverseIntMapWithKeyWithAdjust f outerDm0 outerDm' = PerformEventT $ traverseIntMapWithKeyWithAdjustRequesterTWith (defaultAdjustIntBase traverseIntMapPatchWithKey) patchIntMapNewElementsMap mergeIntIncremental (\k v -> unPerformEventT $ f k v) (coerce outerDm0) (coerceEvent outerDm')
  traverseDMapWithKeyWithAdjust f outerDm0 outerDm' = PerformEventT $ traverseDMapWithKeyWithAdjustRequesterTWith (defaultAdjustBase traversePatchDMapWithKey) mapPatchDMap weakenPatchDMapWith patchMapNewElementsMap mergeMapIncremental (\k v -> unPerformEventT $ f k v) (coerce outerDm0) (coerceEvent outerDm')
  traverseDMapWithKeyWithAdjustWithMove f outerDm0 outerDm' = PerformEventT $ traverseDMapWithKeyWithAdjustRequesterTWith (defaultAdjustBase traversePatchDMapWithMoveWithKey) mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElementsMap mergeMapIncrementalWithMove (\k v -> unPerformEventT $ f k v) (coerce outerDm0) (coerceEvent outerDm')

defaultAdjustBase :: forall t v v2 k' p. (Monad (HostFrame t), Reflex t)
  => ((forall a. k' a -> v a -> HostFrame t (v2 a)) -> p k' v -> HostFrame t (p k' v2))
  -> (forall a. k' a -> v a -> HostFrame t (v2 a))
  -> DMap k' v
  -> Event t (p k' v)
  -> RequesterT t (HostFrame t) Identity (HostFrame t) (DMap k' v2, Event t (p k' v2))
defaultAdjustBase traversePatchWithKey f' dm0 dm' = do
  result0 <- lift $ DMap.traverseWithKey f' dm0
  result' <- requestingIdentity $ ffor dm' $ traversePatchWithKey f'
  return (result0, result')

defaultAdjustIntBase :: forall t v v2 p. (Monad (HostFrame t), Reflex t)
  => ((IntMap.Key -> v -> HostFrame t v2) -> p v -> HostFrame t (p v2))
  -> (IntMap.Key -> v -> HostFrame t v2)
  -> IntMap v
  -> Event t (p v)
  -> RequesterT t (HostFrame t) Identity (HostFrame t) (IntMap v2, Event t (p v2))
defaultAdjustIntBase traversePatchWithKey f' dm0 dm' = do
  result0 <- lift $ IntMap.traverseWithKey f' dm0
  result' <- requestingIdentity $ ffor dm' $ traversePatchWithKey f'
  return (result0, result')

instance ReflexHost t => MonadReflexCreateTrigger t (PerformEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PerformEventT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PerformEventT $ lift $ newFanEventWithTrigger f

-- | Run a 'PerformEventT' action, returning a 'FireCommand' that allows the
-- caller to trigger 'Event's while ensuring that 'performEvent' actions are run
-- at the appropriate time.
{-# INLINABLE hostPerformEventT #-}
hostPerformEventT :: forall t m a.
                     ( MonadReflexHost t m
                     , MonadRef m
                     , Ref m ~ Ref IO
                     )
                  => PerformEventT t m a
                  -> m (a, FireCommand t m)
hostPerformEventT a = do
  (response, responseTrigger) <- newEventWithTriggerRef
  (result, eventToPerform) <- runHostFrame $ runRequesterT (unPerformEventT a) response
  eventToPerformHandle <- subscribeEvent eventToPerform
  return $ (,) result $ FireCommand $ \triggers (readPhase :: ReadPhase m a') -> do
    let go :: [DSum (EventTrigger t) Identity] -> m [a']
        go ts = do
          (result', mToPerform) <- fireEventsAndRead ts $ do
            mToPerform <- sequence =<< readEvent eventToPerformHandle
            result' <- readPhase
            return (result', mToPerform)
          case mToPerform of
            Nothing -> return [result']
            Just toPerform -> do
              responses <- runHostFrame $ traverseRequesterData (fmap Identity) toPerform
              mrt <- readRef responseTrigger
              let followupEventTriggers = case mrt of
                    Just rt -> [rt :=> Identity responses]
                    Nothing -> []
              (result':) <$> go followupEventTriggers
    go triggers

instance ReflexHost t => MonadSample t (PerformEventT t m) where
  {-# INLINABLE sample #-}
  sample = PerformEventT . lift . sample

instance (ReflexHost t, MonadHold t m) => MonadHold t (PerformEventT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = PerformEventT $ lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = PerformEventT $ lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = PerformEventT $ lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 v' = PerformEventT $ lift $ buildDynamic getV0 v'
  {-# INLINABLE headE #-}
  headE = PerformEventT . lift . headE
  {-# INLINABLE now #-}
  now = PerformEventT . lift $ now

instance (MonadRef (HostFrame t), ReflexHost t) => MonadRef (PerformEventT t m) where
  type Ref (PerformEventT t m) = Ref (HostFrame t)
  {-# INLINABLE newRef #-}
  newRef = PerformEventT . lift . newRef
  {-# INLINABLE readRef #-}
  readRef = PerformEventT . lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = PerformEventT . lift . writeRef r

instance (MonadAtomicRef (HostFrame t), ReflexHost t) => MonadAtomicRef (PerformEventT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = PerformEventT . lift . atomicModifyRef r
