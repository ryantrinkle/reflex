{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}


import Prelude

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

import Control.Monad.IO.Class (liftIO)
import Data.Kind

import Reflex
import Reflex.Host.Class
import Reflex.Test.Monad.Host


-- | a very basic test network, simple passes on the input event to its observed outputs
basic_network :: forall t m. (ReflexHost t)
  => (Event t Int -> TestGuestT t m (Event t Int))
basic_network ev = return ev

-- | test 'basic_network'
test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $ do
  ins <- newEventWithTriggerRef
  runReflexTestT ins basic_network $ do

    -- get our app's output events and subscribe to them
    oh                               <- subscribeEvent =<< outputs
    -- get our input trigger ref, dereference it, queue it and fire it
    intref                           <- inputTriggerRefs

    -- example of how to manually dereferences our input trigger
    {-
    mh :: Maybe (EventTrigger T Int) <- liftIO $ readRef intref
    case mh of
      Just h  -> queueEventTrigger $ (h :=> Identity 0)
      Nothing -> error "no subscribers to h"
    -}

    -- simpler version that uses 'queueEventTriggerRef'
    queueEventTriggerRef intref 123

    -- fire the events and read from our output handle
    a <- fireQueuedEventsAndRead $ sequence =<< readEvent oh

    -- validate results
    liftIO $ a @?= [Just 123]

data BasicNetworkTest1 t (m :: Type -> Type)

instance (TestGuestConstraints t m) => ReflexTestApp (BasicNetworkTest1 t m) t m where
  data AppInputTriggerRefs (BasicNetworkTest1 t m) =
    BasicNetworkTest1_InputTriggerRefs { _basicNetworkTest1_InputTriggerRefs_intEvTRef :: ReflexTriggerRef t m Int }
  data AppInputEvents (BasicNetworkTest1 t m) =
    BasicNetworkTest1_InputEvents { _basicNetworkTest1_InputEvents_intEv :: Event t Int }
  data AppOutput (BasicNetworkTest1 t m) =
    BasicNetworkTest1_Output { _basicNetworkTest1_Output_intEv :: Event t Int }
  getApp ev = basic_network (_basicNetworkTest1_InputEvents_intEv ev) >>= return . BasicNetworkTest1_Output
  makeInputs = do
    (inev, intref) <- newEventWithTriggerRef
    return (BasicNetworkTest1_InputEvents inev, BasicNetworkTest1_InputTriggerRefs intref)

test_basic_viaReflexTestApp :: Test
test_basic_viaReflexTestApp = TestLabel "basic_viaReflexTestApp" $ TestCase $ runSpiderHost $
  runReflexTestApp @ (BasicNetworkTest1 (SpiderTimeline Global) (SpiderHost Global)) $ do
    -- get our app's output events and subscribe to them
    BasicNetworkTest1_Output {..} <- outputs
    oh                            <- subscribeEvent _basicNetworkTest1_Output_intEv

    -- get our input trigger ref
    BasicNetworkTest1_InputTriggerRefs{..}                           <- inputTriggerRefs

    -- fire it
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_intEvTRef 123
    -- fire the events and read from our output handle
    a1 <- fireQueuedEventsAndRead $ sequence =<< readEvent oh

    -- validate results
    liftIO $ a1 @?= [Just 123]

    -- try a different value
    queueEventTriggerRef _basicNetworkTest1_InputTriggerRefs_intEvTRef 238
    a2 <- fireQueuedEventsAndRead $ sequence =<< readEvent oh
    liftIO $ a2 @?= [Just 238]


spec :: Spec
spec = do
  fromHUnitTest test_basic
  fromHUnitTest test_basic_viaReflexTestApp


main = hspec spec
