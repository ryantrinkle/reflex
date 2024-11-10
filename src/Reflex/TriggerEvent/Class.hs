-- | This module defines 'TriggerEvent', which describes actions that may create
-- new 'Event's that can be triggered from 'IO'.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.TriggerEvent.Class
  ( TriggerEvent (..)
  , newEventWithLazySTMTrigger
  ) where

import Reflex.Class

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe (MaybeT)

--TODO: Shouldn't have IO hard-coded
-- | 'TriggerEvent' represents actions that can create 'Event's that can be
-- triggered by 'IO' actions.
class Monad m => TriggerEvent t m | m -> t where
  -- | Create a triggerable 'Event'.  Whenever the resulting function is called,
  -- the resulting 'Event' will fire at some point in the future.  Note that
  -- this may not be synchronous.
  newTriggerEvent :: m (Event t a, a -> IO ())
  -- | Like 'newTriggerEvent', but the callback itself takes another callback,
  -- to be invoked once the requested 'Event' occurrence has finished firing.
  -- This allows synchronous operation.
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  -- | Like 'newTriggerEventWithOnComplete', but with setup and teardown.  This
  -- relatively complex type signature allows any external listeners to be
  -- subscribed lazily and then removed whenever the returned 'Event' is no
  -- longer being listened to.  Note that the setup/teardown may happen multiple
  -- times, and there is no guarantee that the teardown will be executed
  -- promptly, or even at all, in the case of program termination.
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

instance TriggerEvent t m => TriggerEvent t (ReaderT r m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance TriggerEvent t m => TriggerEvent t (StateT s m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance TriggerEvent t m => TriggerEvent t (Strict.StateT s m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance TriggerEvent t m => TriggerEvent t (MaybeT m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

-- | Create an 'Event' which triggers each time a blocking 'STM' action
-- completes. This can be used to listen on a broadcast 'TChan' without leaking
-- memory by duplicating it only when the event is being listened to. Note that
-- the setup/teardown may happen multiple times, and there is no guarantee that
-- the teardown will be executed promptly, or even at all, in the case of
-- program termination.
newEventWithLazySTMTrigger
  :: TriggerEvent t m
  => STM s
  -- ^ Setup action returning state token, e.g. @/dupTChan eventBroadcastChan/@
  -> (s -> STM ())
  -- ^ Teardown action
  -> (s -> STM a)
  -- ^ Action to block on retrieving the next event value, e.g. 'readTChan'
  -> m (Event t a)
newEventWithLazySTMTrigger setup teardown getNextValue =
  newEventWithLazyTriggerWithOnComplete $ \fire -> do
    doneVar <- newEmptyTMVarIO
    stateToken <- atomically setup
    let waitDone = Nothing <$ readTMVar doneVar
        waitNext = Just <$> getNextValue stateToken
        loop = atomically (waitDone <|> waitNext) >>= \case
          Nothing -> return ()
          Just a -> do
            fire a $ return ()
            loop
    void $ forkIO loop
    return $ atomically $ do
      putTMVar doneVar ()
      teardown stateToken
