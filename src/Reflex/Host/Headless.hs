{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Host.Headless where

import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef, Ref, readRef)
import Data.Dependent.Sum (DSum (..), (==>))
import Data.Foldable (for_, asum)
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import Reflex
import Reflex.Host.Class

type MonadHeadlessApp t m =
  ( Reflex t
  , Adjustable t m
  , MonadCatch m
  , MonadFix (Performable m)
  , MonadFix m
  , MonadHold t (Performable m)
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadMask m
  , MonadRef (HostFrame t)
  , MonadSample t (Performable m)
  , MonadSample t m
  , MonadThrow m
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad (HostFrame t)
  , Ref (HostFrame t) ~ IORef
  , Ref m ~ IORef
  , ReflexHost t
  , TriggerEvent t m
  )

-- | Run a headless FRP network. Inside the action, you will most probably use
-- the capabilities provided by the 'TriggerEvent' and 'PerformEvent' type
-- classes to interface the FRP network with the outside world. Useful for
-- testing. Each headless network runs on its own spider timeline.
runHeadlessApp
  :: forall a
  .  (forall t m. MonadHeadlessApp t m => m (Event t a))
  -- ^ The action to be run in the headless FRP network. The FRP network is
  -- closed at the first occurrence of the resulting 'Event'.
  -> IO a
runHeadlessApp guest =
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline.
  withSpiderTimeline $ runSpiderHostForTimeline $ do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newChan
    -- Run the "guest" application, providing the appropriate context. We'll
    -- pure the result of the action, and a 'FireCommand' that will be used to
    -- trigger events.
    (result, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventT events $  -- Allows the guest app to create new
                                          -- events and triggers and write
                                          -- those triggers to a channel from
                                          -- which they will be read and
                                          -- processed.
            guest

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent result

    -- When there is a subscriber to the post-build event, fire the event.
    initialShutdownEventFirings :: Maybe [Maybe a] <- for mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ sequence =<< readEvent shutdown
    let shutdownImmediately = case initialShutdownEventFirings of
          -- We didn't even fire postBuild because it wasn't subscribed
          Nothing -> Nothing
          -- Take the first Just, if there is one. Ideally, we should cut off
          -- the event loop as soon as the firing happens, but Performable
          -- doesn't currently give us an easy way to do that
          Just firings -> asum firings

    case shutdownImmediately of
      Just exitResult -> pure exitResult
      -- The main application loop. We wait for new events and fire those that
      -- have subscribers. If we detect a shutdown request, the application
      -- terminates.
      Nothing -> fix $ \loop -> do
        -- Read the next event (blocking).
        ers <- liftIO $ readChan events
        shutdownEventFirings :: [Maybe a] <- do
          -- Fire events that have subscribers.
          fireEventTriggerRefs fc ers $
            -- Check if the shutdown 'Event' is firing.
            sequence =<< readEvent shutdown
        let -- If the shutdown event fires multiple times, take the first one.
            -- Ideally, we should cut off the event loop as soon as this fires,
            -- but Performable doesn't currently give us an easy way to do that.
            shutdownNow = asum shutdownEventFirings
        case shutdownNow of
          Just exitResult -> pure exitResult
          Nothing -> loop
  where
    -- Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: forall b m t
      .  MonadIO m
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m b
      -> m [b]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          pure $ fmap (==> a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      pure a
