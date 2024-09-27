-- | This module defines 'TriggerEventT', the standard implementation of
-- 'TriggerEvent'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.TriggerEvent.Base
  ( TriggerEventT (..)
  , runTriggerEventT
  , askEvents
  , TriggerInvocation (..)
  , EventTriggerRef (..)
  ) where

import Control.Concurrent
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Coerce
import Data.Dependent.Sum
import Data.IORef
import qualified Data.Semigroup as S

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
#endif

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

-- | A value with which to fire an 'Event', as well as a callback to invoke
-- after its propagation has completed.
data TriggerInvocation a = TriggerInvocation a (IO ())

-- | A reference to an 'EventTrigger' suitable for firing with 'TriggerEventT'.
newtype EventTriggerRef t a = EventTriggerRef { unEventTriggerRef :: IORef (Maybe (EventTrigger t a)) }

-- | A basic implementation of 'TriggerEvent'.
newtype TriggerEventT t m a = TriggerEventT { unTriggerEventT :: ReaderT (Chan [DSum (EventTriggerRef t) TriggerInvocation]) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException, MonadCatch, MonadThrow, MonadMask)

-- | Run a 'TriggerEventT' action.  The argument should be a 'Chan' into which
-- 'TriggerInvocation's can be passed; it is expected that some other thread
-- will be responsible for popping values out of the 'Chan' and firing their
-- 'EventTrigger's.
runTriggerEventT :: TriggerEventT t m a -> Chan [DSum (EventTriggerRef t) TriggerInvocation] -> m a
runTriggerEventT = runReaderT . unTriggerEventT

instance MonadTrans (TriggerEventT t) where
  {-# INLINABLE lift #-}
  lift = TriggerEventT . lift

instance PrimMonad m => PrimMonad (TriggerEventT t m) where
  type PrimState (TriggerEventT t m) = PrimState m
  {-# INLINABLE primitive #-}
  primitive = lift . primitive

instance PerformEvent t m => PerformEvent t (TriggerEventT t m) where
  type Performable (TriggerEventT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (TriggerEventT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TriggerEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (TriggerEventT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = do
    events <- askEvents
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return . (,) eResult $ \a cb ->
      writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = do
    events <- askEvents
    lift . newEventWithTrigger $ \t ->
      f $ \a cb -> do
        reResultTrigger <- newRef $ Just t
        writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]

instance MonadRef m => MonadRef (TriggerEventT t m) where
  type Ref (TriggerEventT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (TriggerEventT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (TriggerEventT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (TriggerEventT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE
  {-# INLINABLE now #-}
  now = lift now

instance Adjustable t m => Adjustable t (TriggerEventT t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace (TriggerEventT a0) a' = TriggerEventT $ runWithReplace a0 (coerceEvent a')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = TriggerEventT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = TriggerEventT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = TriggerEventT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

-- TODO: Monoid and Semigroup can likely be derived once ReaderT has them.
instance (Monoid a, Applicative m) => Monoid (TriggerEventT t m a) where
  mempty = pure mempty
  mappend = (<>)

instance (S.Semigroup a, Applicative m) => S.Semigroup (TriggerEventT t m a) where
  (<>) = liftA2 (S.<>)


-- | Retrieve the current 'Chan'; event trigger invocations pushed into it will
-- be fired.
askEvents :: Monad m => TriggerEventT t m (Chan [DSum (EventTriggerRef t) TriggerInvocation])
askEvents = TriggerEventT ask
