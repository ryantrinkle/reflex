-- | This module defines 'PostBuildT', the standard implementation of
-- 'PostBuild'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PostBuild.Base
  ( PostBuildT (..)
  , runPostBuildT
  -- * Internal
  , mapIntMapWithAdjustImpl
  , mapDMapWithAdjustImpl
  ) where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Applicative (liftA2)
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import qualified Control.Monad.Trans.Control as TransControl
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Semigroup as S

-- | Provides a basic implementation of 'PostBuild'.
newtype PostBuildT t m a = PostBuildT { unPostBuildT :: ReaderT (Event t ()) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadTrans
    , MonadException
    , MonadAsyncException
    , MonadMask
    , MonadThrow
    , MonadCatch
    )

-- | Run a 'PostBuildT' action.  An 'Event' should be provided that fires
-- immediately after the action is finished running; no other 'Event's should
-- fire first.
{-# INLINABLE runPostBuildT #-}
runPostBuildT :: PostBuildT t m a -> Event t () -> m a
runPostBuildT (PostBuildT a) = runReaderT a

-- TODO: Monoid and Semigroup can likely be derived once ReaderT has them.
instance (Monoid a, Applicative m) => Monoid (PostBuildT t m a) where
  mempty = pure mempty

instance (S.Semigroup a, Applicative m) => S.Semigroup (PostBuildT t m a) where
  (<>) = liftA2 (S.<>)

instance PrimMonad m => PrimMonad (PostBuildT x m) where
  type PrimState (PostBuildT x m) = PrimState m
  primitive = lift . primitive

instance (Reflex t, Monad m) => PostBuild t (PostBuildT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = PostBuildT ask

instance MonadSample t m => MonadSample t (PostBuildT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostBuildT t m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE
  {-# INLINABLE now #-}
  now = lift now

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = lift . performEvent

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (PostBuildT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (PostBuildT t m) where
  type Ref (PostBuildT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostBuildT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, PerformEvent t m) => Adjustable t (PostBuildT t m) where
  runWithReplace a0 a' = do
    postBuild <- getPostBuild
    lift $ do
      rec result@(_, result') <- runWithReplace (runPostBuildT a0 postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') a'
          let voidResult' = fmapCheap (\_ -> ()) result'
      return result
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust = mapIntMapWithAdjustImpl traverseIntMapWithKeyWithAdjust
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = mapDMapWithAdjustImpl traverseDMapWithKeyWithAdjust mapPatchDMap
  traverseDMapWithKeyWithAdjustWithMove = mapDMapWithAdjustImpl traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove

{-# INLINABLE mapIntMapWithAdjustImpl #-}
mapIntMapWithAdjustImpl :: forall t m v v' p. (Reflex t, MonadFix m, MonadHold t m, Functor p)
  => (   (IntMap.Key -> (Event t (), v) -> m v')
      -> IntMap (Event t (), v)
      -> Event t (p (Event t (), v))
      -> m (IntMap v', Event t (p v'))
     )
  -> (IntMap.Key -> v -> PostBuildT t m v')
  -> IntMap v
  -> Event t (p v)
  -> PostBuildT t m (IntMap v', Event t (p v'))
mapIntMapWithAdjustImpl base f dm0 dm' = do
  postBuild <- getPostBuild
  let loweredDm0 = fmap ((,) postBuild) dm0
      f' :: IntMap.Key -> (Event t (), v) -> m v'
      f' k (e, v) = do
        runPostBuildT (f k v) e
  lift $ do
    rec (result0, result') <- base f' loweredDm0 loweredDm'
        cohortDone <- numberOccurrencesFrom_ 1 result'
        numberedDm' <- numberOccurrencesFrom 1 dm'
        let postBuild' = fanInt $ fmapCheap (`IntMap.singleton` ()) cohortDone
            loweredDm' = flip pushAlways numberedDm' $ \(n, p) -> do
              return $ fmap ((,) (selectInt postBuild' n)) p
    return (result0, result')

{-# INLINABLE mapDMapWithAdjustImpl #-}
mapDMapWithAdjustImpl :: forall t m k v v' p. (Reflex t, MonadFix m, MonadHold t m)
  => (   (forall a. k a -> Compose ((,) (Bool, Event t ())) v a -> m (v' a))
      -> DMap k (Compose ((,) (Bool, Event t ())) v)
      -> Event t (p k (Compose ((,) (Bool, Event t ())) v))
      -> m (DMap k v', Event t (p k v'))
     )
  -> ((forall a. v a -> Compose ((,) (Bool, Event t ())) v a) -> p k v -> p k (Compose ((,) (Bool, Event t ())) v))
  -> (forall a. k a -> v a -> PostBuildT t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> PostBuildT t m (DMap k v', Event t (p k v'))
mapDMapWithAdjustImpl base mapPatch f dm0 dm' = do
  postBuild <- getPostBuild
  let loweredDm0 = DMap.map (Compose . (,) (False, postBuild)) dm0
      f' :: forall a. k a -> Compose ((,) (Bool, Event t ())) v a -> m (v' a)
      f' k (Compose ((shouldHeadE, e), v)) = do
        eOnce <- if shouldHeadE
          then headE e --TODO: Avoid doing this headE so many times; once per loweredDm' firing ought to be OK, but it's not totally trivial to do because result' might be firing at the same time, and we don't want *that* to be the postBuild occurrence
          else return e
        runPostBuildT (f k v) eOnce
  lift $ do
    rec (result0, result') <- base f' loweredDm0 loweredDm'
        let voidResult' = fmapCheap (\_ -> ()) result'
        let loweredDm' = ffor dm' $ mapPatch (Compose . (,) (True, voidResult'))
    return (result0, result')

--------------------------------------------------------------------------------
-- Deprecated functionality
--------------------------------------------------------------------------------

-- | Deprecated
instance TransControl.MonadTransControl (PostBuildT t) where
  type StT (PostBuildT t) a = TransControl.StT (ReaderT (Event t ())) a
  {-# INLINABLE liftWith #-}
  liftWith = TransControl.defaultLiftWith PostBuildT unPostBuildT
  {-# INLINABLE restoreT #-}
  restoreT = TransControl.defaultRestoreT PostBuildT
