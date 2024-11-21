{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

import Prelude

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

import Reflex
import Reflex.Test.SimpleHost

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Functor
import qualified Data.List as L
import Data.These


-- | network that ensures postbuild event was triggered
postbuild_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t () () -> TestGuestT t m (AppOut t Bool ()))
postbuild_network AppIn {..} = do
  pbev            <- getPostBuild
  didPBTriggerBeh <- hold False (pbev $> True)
  return AppOut { _appOut_behavior = didPBTriggerBeh, _appOut_event = never }

test_postbuild :: Test
test_postbuild = TestLabel "postbuild" $ TestCase $ runSpiderHost $ do
  appFrame <- getAppFrame postbuild_network ()
  -- tick the appFrame once which will give us the output behavior value of the previous frame (which triggered the postbuild event)
  out      <- tickAppFrame appFrame (Just (That ()))
  liftIO $ out @?= [(True, Nothing)]


-- | Basic network that flips returns a behavior that is the input behavior flipped
-- and an event that is the input behavior's value added to the input events value.
basic_network
  :: forall t m
   . (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t Int Int -> TestGuestT t m (AppOut t Int Int))
basic_network AppIn {..} = return AppOut
  { _appOut_behavior = fmap (* (-1)) _appIn_behavior
  , _appOut_event    = fmap (\(b, e) -> e + b)
                         $ attach _appIn_behavior _appIn_event
  }

test_basic :: Test
test_basic = TestLabel "basic" $ TestCase $ runSpiderHost $ do
  let b  = 10 :: Int
      es = [1 .. 10] :: [Int]
  appFrame <- getAppFrame basic_network b
  forM_ es $ \e -> do
    out <- tickAppFrame appFrame (Just (That e))
    liftIO $ L.last out @?= (-b, Just (b + e))

spec :: Spec
spec = do
  fromHUnitTest test_basic
  fromHUnitTest test_postbuild

main = hspec spec
