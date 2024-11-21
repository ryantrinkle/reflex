{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}



-- |
-- Module:
--   Reflex.Test.Monad.Host
-- Description:
--   This module contains a monad for testing reflex networks

module Reflex.Test.Monad.Host
  ( TestGuestT
  , TestGuestConstraints
  , ReflexTriggerRef

  , MonadReflexTest(..)
  , AppState(..)
  , ReflexTestT(..)
  , runReflexTestT
  , ReflexTestApp(..)
  , runReflexTestApp
  )
where



import Prelude


import Control.Concurrent.Chan
import Control.Monad.IO.Class

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Kind

import Reflex
import Reflex.Class ()
import Reflex.Host.Class


type TestGuestT t (m :: Type -> Type)
  = TriggerEventT t (PostBuildT t (PerformEventT t m))

type TestGuestConstraints t (m :: Type -> Type)
  = ( MonadReflexHost t m
    , MonadHold t m
    , MonadSample t m
    , Ref m ~ Ref IO
    , MonadRef m
    , MonadRef (HostFrame t)
    , Ref (HostFrame t) ~ Ref IO
    , MonadIO (HostFrame t)
    , PrimMonad (HostFrame t)
    , MonadIO m
    , MonadFix m
    )

-- | since we work with this type directly a lot, it helps to wrap it around a type synonym
type ReflexTriggerRef t (m :: Type -> Type) a = Ref m (Maybe (EventTrigger t a))

-- |
class MonadReflexTest t m | m -> t  where
  -- | since event subscriptions also happen within the monad, input triggers created via 'newEventWithTriggerRef' may be stuck in the 'Nothing' state as there are no listeners yet
  -- therefore it's necessary to pass in IORefs to the EventTriggers, thus the name of this type
  -- in practice, this will likely be a record containing many trigger refs and the monad user must deref them all
  type InputTriggerRefs m :: Type
  -- | in practice, this will likely be a record containing events and behaviors for the monad user to build a 'ReadPhase' that is passed into 'fireQueuedEventsAndRead'
  type OutputEvents m :: Type
  -- | the inner monad that reflex is running in
  -- likely 'SpiderHost Global'
  type InnerMonad m :: Type -> Type
  -- | see comments for 'InputTriggerRefs'
  inputTriggerRefs :: m (InputTriggerRefs m)
  -- | all queued triggers will fire simultaneous on the next execution of 'fireQueuedEventsAndRead'
  queueEventTrigger :: DSum (EventTrigger t) Identity -> m ()
  -- | same as 'queueEventTrigger' except works with trigger refs
  -- if the trigger ref derefs to 'Nothing', the event does not get queued
  queueEventTriggerRef :: Ref (InnerMonad m) (Maybe (EventTrigger t a)) -> a -> m ()
  -- | see comments for 'OutputEvents'
  outputs :: m (OutputEvents m)
  -- | fire all queued events and run a ReadPhase to produce results from the execution frames
  -- readphase takes place in the inner monad
  fireQueuedEventsAndRead :: ReadPhase (InnerMonad m) a -> m [a]

-- m is 'InnerMonad' from above
data AppState t m = AppState
    { _appState_queuedEvents :: [DSum (EventTrigger t) Identity] -- ^ events to fire in next 'FireCommand'
    -- ^ 'FireCommand' to fire events and run next frame
    , _appState_fire         :: FireCommand t m -- ^ 'FireCommand' to fire events and run next frame
    }

-- | implementation of 'MonadReflexTest'
newtype ReflexTestT t intref out m a = ReflexTestT { unReflexTestM :: ReaderT (intref, out) (StateT (AppState t m) m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFix
    , MonadReader (intref, out)
    , MonadState (AppState t m))

deriving instance MonadSample t m => MonadSample t (ReflexTestT t intref out m)
deriving instance MonadHold t m => MonadHold t (ReflexTestT t intref out m)
deriving instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ReflexTestT t intref out m)

instance MonadTrans (ReflexTestT t intref out) where
  lift = ReflexTestT . lift . lift

instance (MonadSubscribeEvent t m) => MonadSubscribeEvent t (ReflexTestT t intref out m) where
  subscribeEvent = lift . subscribeEvent

instance (MonadRef m) => MonadReflexTest t (ReflexTestT t intref out m) where
  type InputTriggerRefs (ReflexTestT t intref out m) = intref
  type OutputEvents (ReflexTestT t intref out m) = out
  type InnerMonad (ReflexTestT t intref out m) = m
  inputTriggerRefs = do
    (intref,_) <- ask
    return intref
  queueEventTrigger evt = do
    as <- get
    put $ as { _appState_queuedEvents = evt : _appState_queuedEvents as }
  queueEventTriggerRef ref a = do
    mpulse <- lift $ readRef ref
    case mpulse of
      Nothing    -> return ()
      Just pulse -> do
        as <- get
        put $ as { _appState_queuedEvents = (pulse :=> Identity a) : _appState_queuedEvents as }
  outputs = do
    (_,out) <- ask
    return out
  fireQueuedEventsAndRead rp = do
    as <- get
    put $ as { _appState_queuedEvents = [] }
    lift $ (runFireCommand $ _appState_fire as) (_appState_queuedEvents as) rp

runReflexTestT
  :: forall intref inev out t m a
   . (TestGuestConstraints t m)
  => (inev, intref) -- ^ make sure intref match inev, i.e. return values of newEventWithTriggerRef
  -> (inev -> TestGuestT t m out) -- ^ network to test
  -> ReflexTestT t intref out m a -- ^ test monad to run
  -> m ()
runReflexTestT (input, inputTRefs) app rtm = do
  (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef

  events                           <- liftIO newChan
  (output, fc@(FireCommand fire))  <- do
    hostPerformEventT
      $ flip runPostBuildT    postBuild
      $ flip runTriggerEventT events
      $ app input

  -- handle post build
  -- TODO consider adding some way to test 'PostBuild' results
  mPostBuildTrigger <- readRef postBuildTriggerRef
  _                 <- case mPostBuildTrigger of
    Nothing -> return [()] -- no subscribers
    Just postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

  -- TODO maybe find a way to handle trigger events
  -- one solution is to implement non-blocking variant of TriggerEventT
  -- and then pass as part of AppState such that each call to readPhase will fire any trigger events
  -- another option is just to start a thread and output warnings anytime triggerEvs are created
  --triggerEvs <- liftIO $ readChan events

  -- run the test monad
  _ <- flip runStateT (AppState [] fc)
    $ flip runReaderT (inputTRefs, output)
      $ unReflexTestM rtm

  return ()



-- | class to help bind network and types to a 'ReflexTestT'
-- see test/Reflex/Test/Monad/HostSpec.hs for usage example
class ReflexTestApp app t m | app -> t m where
  data AppInputTriggerRefs app :: Type
  data AppInputEvents app :: Type
  data AppOutput app :: Type
  getApp :: AppInputEvents app -> TestGuestT t m (AppOutput app)
  makeInputs :: m (AppInputEvents app, AppInputTriggerRefs app)

runReflexTestApp
  :: (ReflexTestApp app t m, TestGuestConstraints t m)
  => ReflexTestT t (AppInputTriggerRefs app) (AppOutput app) m ()
  -> m ()
runReflexTestApp rtm = do
  i <- makeInputs
  runReflexTestT i getApp rtm
