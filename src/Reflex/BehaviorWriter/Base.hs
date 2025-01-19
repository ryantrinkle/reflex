{-|
Module: Reflex.BehaviorWriter.Base
Description: Implementation of BehaviorWriter
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.BehaviorWriter.Base
  ( BehaviorWriterT (..)
  , runBehaviorWriterT
  , withBehaviorWriterT
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Some (Some)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.BehaviorWriter.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Query.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

-- | A basic implementation of 'BehaviorWriter'.
newtype BehaviorWriterT t w m a = BehaviorWriterT { unBehaviorWriterT :: StateT [Behavior t w] m a }
  -- The list is kept in reverse order
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MFunctor
    , MonadIO
    , MonadFix
    , MonadAsyncException
    , MonadException
    , MonadCatch
    , MonadThrow
    , MonadMask
    )

-- | Run a 'BehaviorWriterT' action.  The behavior writer output will be provided
-- along with the result of the action.
runBehaviorWriterT :: (Monad m, Reflex t, Monoid w) => BehaviorWriterT t w m a -> m (a, Behavior t w)
runBehaviorWriterT (BehaviorWriterT a) = do
  (result, ws) <- runStateT a []
  return (result, mconcat $ reverse ws)

-- | Map a function over the output of a 'BehaviorWriterT'.
withBehaviorWriterT :: (Monoid w, Monoid w', Reflex t, MonadHold t m)
                   => (w -> w')
                   -> BehaviorWriterT t w m a
                   -> BehaviorWriterT t w' m a
withBehaviorWriterT f dw = do
  (r, d) <- lift $ do
    (r, d) <- runBehaviorWriterT dw
    let d' = fmap f d
    return (r, d')
  tellBehavior d
  return r

deriving instance MonadHold t m => MonadHold t (BehaviorWriterT t w m)
deriving instance MonadSample t m => MonadSample t (BehaviorWriterT t w m)

instance MonadRef m => MonadRef (BehaviorWriterT t w m) where
  type Ref (BehaviorWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (BehaviorWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (BehaviorWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, Monoid w, Reflex t) => BehaviorWriter t w (BehaviorWriterT t w m) where
  tellBehavior w = BehaviorWriterT $ modify (w :)

instance MonadReader r m => MonadReader r (BehaviorWriterT t w m) where
  ask = lift ask
  local f (BehaviorWriterT a) = BehaviorWriterT $ mapStateT (local f) a
  reader = lift . reader

instance PerformEvent t m => PerformEvent t (BehaviorWriterT t w m) where
  type Performable (BehaviorWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (BehaviorWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (BehaviorWriterT t w m) where
  getPostBuild = lift getPostBuild

instance MonadState s m => MonadState s (BehaviorWriterT t w m) where
  get = lift get
  put = lift . put

instance Requester t m => Requester t (BehaviorWriterT t w m) where
  type Request (BehaviorWriterT t w m) = Request m
  type Response (BehaviorWriterT t w m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance (MonadQuery t q m, Monad m) => MonadQuery t q (BehaviorWriterT t w m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance (Adjustable t m, Monoid w, MonadHold t m, Reflex t) => Adjustable t (BehaviorWriterT t w m) where
  runWithReplace a0 a' = do
    (result0, result') <- lift $ runWithReplace (runBehaviorWriterT a0) $ runBehaviorWriterT <$> a'
    tellBehavior . join =<< hold (snd result0) (snd <$> result')
    return (fst result0, fst <$> result')
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustImpl traverseIntMapWithKeyWithAdjust
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustImpl traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove weakenPatchDMapWithMoveWith

traverseIntMapWithKeyWithAdjustImpl
  :: forall t w v' p p' v m.
     ( PatchTarget (p' (Behavior t w)) ~ IntMap (Behavior t w)
     , Patch (p' (Behavior t w))
     , Monoid w
     , Reflex t
     , MonadHold t m
     , Functor p
     , p ~ p'
     )
  => (   (IntMap.Key -> v -> m (v', Behavior t w))
      -> IntMap v
      -> Event t (p v)
      -> m (IntMap (v', Behavior t w), Event t (p (v', Behavior t w)))
     )
  -> (IntMap.Key -> v -> BehaviorWriterT t w m v')
  -> IntMap v
  -> Event t (p v)
  -> BehaviorWriterT t w m (IntMap v', Event t (p v'))
traverseIntMapWithKeyWithAdjustImpl base f (dm0 :: IntMap v) dm' = do
  (result0, result') <- lift $ base (\k v -> runBehaviorWriterT $ f k v) dm0 dm'
  let liftedResult0 = fmap fst result0
      liftedResult' = fmap (fmap fst) result'
      liftedWritten0 :: IntMap (Behavior t w)
      liftedWritten0 = fmap snd result0
      liftedWritten' = fmap (fmap snd) result'
  i <- holdIncremental liftedWritten0 liftedWritten'
  tellBehavior $ pull $ do
    m <- sample $ currentIncremental i
    mconcat . IntMap.elems <$> traverse sample m
  return (liftedResult0, liftedResult')

newtype BehaviorWriterTLoweredResult t w v a = BehaviorWriterTLoweredResult (v a, Behavior t w)

traverseDMapWithKeyWithAdjustImpl
  :: forall t w k v' p p' v m.
     ( PatchTarget (p' (Some k) (Behavior t w)) ~ Map (Some k) (Behavior t w)
     , Patch (p' (Some k) (Behavior t w))
     , Monoid w
     , Reflex t
     , MonadHold t m
     )
  => (   (forall a. k a -> v a -> m (BehaviorWriterTLoweredResult t w v' a))
      -> DMap k v
      -> Event t (p k v)
      -> m (DMap k (BehaviorWriterTLoweredResult t w v'), Event t (p k (BehaviorWriterTLoweredResult t w v')))
     )
  -> ((forall a. BehaviorWriterTLoweredResult t w v' a -> v' a) -> p k (BehaviorWriterTLoweredResult t w v') -> p k v')
  -> ((forall a. BehaviorWriterTLoweredResult t w v' a -> Behavior t w) -> p k (BehaviorWriterTLoweredResult t w v') -> p' (Some k) (Behavior t w))
  -> (forall a. k a -> v a -> BehaviorWriterT t w m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> BehaviorWriterT t w m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustImpl base mapPatch weakenPatchWith f (dm0 :: DMap k v) dm' = do
  (result0, result') <- lift $ base (\k v -> fmap BehaviorWriterTLoweredResult $ runBehaviorWriterT $ f k v) dm0 dm'
  let getValue (BehaviorWriterTLoweredResult (v, _)) = v
      getWritten (BehaviorWriterTLoweredResult (_, w)) = w
      liftedResult0 = DMap.map getValue result0
      liftedResult' = ffor result' $ mapPatch getValue
      liftedWritten0 :: Map (Some k) (Behavior t w)
      liftedWritten0 = weakenDMapWith getWritten result0
      liftedWritten' = ffor result' $ weakenPatchWith getWritten
  i <- holdIncremental liftedWritten0 liftedWritten'
  tellBehavior $ pull $ do
    m <- sample $ currentIncremental i
    mconcat . Map.elems <$> traverse sample m
  return (liftedResult0, liftedResult')
