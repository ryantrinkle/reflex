{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- | This module provides the interface for hosting 'Reflex' engines.  This
-- should only be necessary if you're writing a binding or some other library
-- that provides a core event loop.
module Reflex.Host.Class
  ( ReflexHost (..)
  , MonadSubscribeEvent (..)
  , MonadReadEvent (..)
  , MonadReflexCreateTrigger (..)
  , MonadReflexHost (..)
  , fireEvents
  , newEventWithTriggerRef
  , fireEventRef
  , fireEventRefAndRead
  ) where

import Reflex.Class

import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Ref
import Control.Monad.Trans
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer (WriterT)
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare
import Data.Kind (Type)

-- | Framework implementation support class for the reflex implementation
-- represented by @t@.
class ( Reflex t
      , MonadReflexCreateTrigger t (HostFrame t)
      , MonadSample t (HostFrame t)
      , MonadHold t (HostFrame t)
      , MonadFix (HostFrame t)
      , MonadSubscribeEvent t (HostFrame t)
      ) => ReflexHost t where
  type EventTrigger t :: Type -> Type
  type EventHandle t :: Type -> Type
  type HostFrame t :: Type -> Type

-- | Monad in which Events can be 'subscribed'.  This forces all underlying
-- event sources to be initialized, so that the event will fire whenever it
-- ought to.  Events must be subscribed before they are read using readEvent
class (Reflex t, Monad m) => MonadSubscribeEvent t m | m -> t where
  -- | Subscribe to an event and set it up if needed.
  --
  -- This function will create a new 'EventHandle' from an 'Event'. This handle
  -- may then be used via 'readEvent' in the read callback of
  -- 'fireEventsAndRead'.
  --
  -- If the event wasn't subscribed to before (either manually or through a
  -- dependent event or behavior) then this function will cause the event and
  -- all dependencies of this event to be set up.  For example, if the event was
  -- created by 'newEventWithTrigger', then it's callback will be executed.
  --
  -- It's safe to call this function multiple times.
  subscribeEvent :: Event t a -> m (EventHandle t a)

-- | Monad that allows to read events' values.
class (ReflexHost t, Applicative m, Monad m) => MonadReadEvent t m | m -> t where
  -- | Read the value of an 'Event' from an 'EventHandle' (created by calling
  -- 'subscribeEvent').
  --
  -- After event propagation is done, all events can be in two states: either
  -- they are firing with some value or they are not firing.  In the former
  -- case, this function returns @Just act@, where @act@ in an action to read
  -- the current value of the event. In the latter case, the function returns
  -- @Nothing@.
  --
  -- This function is normally used in the calllback for 'fireEventsAndRead'.
  readEvent :: EventHandle t a -> m (Maybe (m a))

-- | A monad where new events feed from external sources can be created.
class (Applicative m, Monad m) => MonadReflexCreateTrigger t m | m -> t where
  -- | Creates a root 'Event' (one that is not based on any other event).
  --
  -- When a subscriber first subscribes to an event (building another event that
  -- depends on the subscription) the given callback function is run and passed
  -- a trigger. The callback function can then set up the event source in IO.
  -- After this is done, the callback function must return an accompanying
  -- teardown action.
  --
  -- Any time between setup and teardown the trigger can be used to fire the
  -- event, by passing it to 'fireEventsAndRead'.
  --
  -- Note: An event may be set up multiple times. So after the teardown action
  -- is executed, the event may still be set up again in the future.
  newEventWithTrigger :: (EventTrigger t a -> IO (IO ())) -> m (Event t a)
  newFanEventWithTrigger :: GCompare k => (forall a. k a -> EventTrigger t a -> IO (IO ())) -> m (EventSelector t k)

-- | 'MonadReflexHost' designates monads that can run reflex frames.
class ( ReflexHost t
      , MonadReflexCreateTrigger t m
      , MonadSubscribeEvent t m
      , MonadReadEvent t (ReadPhase m)
      , MonadSample t (ReadPhase m)
      , MonadHold t (ReadPhase m)
      ) => MonadReflexHost t m | m -> t where
  type ReadPhase m :: Type -> Type
  -- | Propagate some events firings and read the values of events afterwards.
  --
  -- This function will create a new frame to fire the given events. It will
  -- then update all dependent events and behaviors. After that is done, the
  -- given callback is executed which allows to read the final values of events
  -- and check whether they have fired in this frame or not.
  --
  -- All events that are given are fired at the same time.
  --
  -- This function is typically used in the main loop of a reflex framework
  -- implementation.  The main loop waits for external events to happen (such as
  -- keyboard input or a mouse click) and then fires the corresponding events
  -- using this function. The read callback can be used to read output events
  -- and perform a corresponding response action to the external event.
  fireEventsAndRead :: [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m a

  -- | Run a frame without any events firing.
  --
  -- This function should be used when you want to use 'sample' and 'hold' when
  -- no events are currently firing.  Using this function in that case can
  -- improve performance, since the implementation can assume that no events are
  -- firing when 'sample' or 'hold' are called.
  --
  -- This function is commonly used to set up the basic event network when the
  -- application starts up.
  runHostFrame :: HostFrame t a -> m a

-- | Like 'fireEventsAndRead', but without reading any events.
fireEvents :: MonadReflexHost t m => [DSum (EventTrigger t) Identity] -> m ()
fireEvents dm = fireEventsAndRead dm $ return ()
{-# INLINE fireEvents #-}

-- | Create a new event and store its trigger in an 'IORef' while it's active.
--
-- An event is only active between the set up (when it's first subscribed to)
-- and the teardown phases (when noboby is subscribing the event anymore).  This
-- function returns an Event and an 'IORef'. As long as the event is active, the
-- 'IORef' will contain 'Just' the event trigger to trigger this event. When the
-- event is not active, the 'IORef' will contain 'Nothing'. This allows event
-- sources to be more efficient, since they don't need to produce events when
-- nobody is listening.
newEventWithTriggerRef :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO) => m (Event t a, Ref m (Maybe (EventTrigger t a)))
newEventWithTriggerRef = do
  rt <- newRef Nothing
  e <- newEventWithTrigger $ \t -> do
    writeRef rt $ Just t
    return $ writeRef rt Nothing
  return (e, rt)
{-# INLINE newEventWithTriggerRef #-}

-- | Fire the given trigger if it is not 'Nothing'.
fireEventRef :: (MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO) => Ref m (Maybe (EventTrigger t a)) -> a -> m ()
fireEventRef mtRef input = do
  mt <- readRef mtRef
  case mt of
    Nothing -> return ()
    Just trigger -> fireEvents [trigger :=> Identity input]

-- | Fire the given trigger if it is not 'Nothing', and read from the given
-- 'EventHandle'.
fireEventRefAndRead :: (MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO) => Ref m (Maybe (EventTrigger t a)) -> a -> EventHandle t b -> m (Maybe b)
fireEventRefAndRead mtRef input e = do
  mt <- readRef mtRef
  case mt of
    Nothing -> return Nothing -- Since we aren't firing the input, the output can't fire
    Just trigger -> fireEventsAndRead [trigger :=> Identity input] $ do
      mGetValue <- readEvent e
      case mGetValue of
        Nothing -> return Nothing
        Just getValue -> fmap Just getValue


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ReaderT r m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ReaderT r m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (ReaderT r m) where
  type ReadPhase (ReaderT r m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance (MonadReflexCreateTrigger t m, Monoid w) => MonadReflexCreateTrigger t (WriterT w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance (MonadSubscribeEvent t m, Monoid w) => MonadSubscribeEvent t (WriterT w m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadReflexHost t m, Monoid w) => MonadReflexHost t (WriterT w m) where
  type ReadPhase (WriterT w m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (StateT r m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (StateT s m) where
  type ReadPhase (StateT s m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame


instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (Strict.StateT s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (Strict.StateT r m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (Strict.StateT s m) where
  type ReadPhase (Strict.StateT s m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame



instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ContT r m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ContT r m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (ContT r m) where
  type ReadPhase (ContT r m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ExceptT e m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ExceptT r m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (ExceptT e m) where
  type ReadPhase (ExceptT e m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance (MonadReflexCreateTrigger t m, Monoid w) => MonadReflexCreateTrigger t (RWST r w s m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance (MonadSubscribeEvent t m, Monoid w) => MonadSubscribeEvent t (RWST r w s m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadReflexHost t m, Monoid w) => MonadReflexHost t (RWST r w s m) where
  type ReadPhase (RWST r w s m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame
