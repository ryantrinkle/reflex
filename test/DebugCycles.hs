{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Foldable
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Exception
import System.Timeout
import Data.Maybe (isJust)
import Data.Functor.Misc
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.These
import Data.Align
import Reflex
import Reflex.EventWriter.Base
import Reflex.Test.SimpleHost
import Test.Hspec
import Reflex.Spider.Internal (EventLoopException)
import Data.Witherable (Filterable)

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif

type Widget t m = (MonadHold t m, Reflex t, MonadFix m)

connectDyn :: Widget t m => Event t () -> (Dynamic t a, Dynamic t a) -> m (Dynamic t a)
connectDyn e (d, d') = do
  dd <- holdDyn d (d' <$ e)
  return $ join dd

dynLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
dynLoop (e1, e2) = do
  -- "heightBagRemove: Height 2 not present in bag HeightBag {_heightBag_size = 2, _heightBag_contents = fromList [(0,1)]}"
  rec
    d <- count e1
    d' <- connectDyn e2 (d, liftA2 (+) d d')
  return $ updated d'

connectOnCoincidence :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectOnCoincidence click e = do
  d <- holdDyn never (e <$ click)
  return $ coincidence (updated d)

coincidenceLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
coincidenceLoop (e1, e2) = do
-- "heightBagRemove: Height 1 not present in bag HeightBag {_heightBag_size = 1, _heightBag_contents = fromList [(0,0)]}"
-- (simpler version of dynLoop)
  rec
    e' <- connectOnCoincidence e2 (updated d)
    d <- count (align e' e1)
  return $ updated d

addHeight :: Reflex t =>  Event t a -> Event t a
addHeight e = leftmost [e4, e4] where
  e1 = leftmost [e, e]
  e2 = leftmost [e1, e1]
  e3 = leftmost [e2, e2]
  e4 = leftmost [e3, e3]

-- Take an existing test and build it inside a
buildLoop :: Widget t m => (forall t m. Widget t m => (Event t Int, Event t ()) -> m (Event t Int)) -> (Event t Int, Event t ()) -> m (Event t Int)
buildLoop test (e1, e2) = switchHold never buildLoop
  where buildLoop = pushAlways (const $ test (e1, e2)) e2

connectButtonPromptly :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectButtonPromptly click e = do
  d <- holdDyn never (e <$ click)
  return (switchDyn d)

connectButton :: Widget t m => Event t () -> Event t a -> m (Event t a)
connectButton click e = do
  d <- hold never (e <$ click)
  return (switch d)

switchLoop01 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop01 (e1, e2) = do
  rec
    e' <- connectButton e2 (updated d)
    d <- count (align e' e1)
  return $ updated d

mergeLoop :: forall t m. (Adjustable t m, Widget t m) => (Event t Int, Event t ()) -> m (Event t Int)
mergeLoop (e1, e2) = do
  rec
    (_, e) <- runEventWriterT $
      runWithReplace w (leftmost [w <$ e1])
  return (sum <$> e)
  where
    w = do
      c <- count e1
      tellEvent (updated ((pure <$> c) :: Dynamic t [Int]))

switchLoop02 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop02 (e1, e2) = do
  rec
    e' <- connectButton e2 (updated d)
    d <- count (leftmost [e', e1])
  return $ updated d

switchLoop03 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
switchLoop03 (e1, e2) = do
  rec
    e' <- connectButton e2 (addHeight $ updated d)
    d <- count (align e' e1)
  return $ updated d

staticLoop01 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
staticLoop01 (e1, e2) = do
  rec
    d <- foldDyn (+) (0 :: Int) (1 <$ align e1 (updated d))
  return $ updated d

staticLoop02 :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
staticLoop02 (e1, e2) = do
  rec
    d <- foldDyn (+) (0 :: Int) (leftmost [e1, updated d])
  return $ updated d

buildStaticLoop :: Widget t m => (Event t Int, Event t ()) -> m (Event t Int)
buildStaticLoop (e1, e2) = switchHold never buildLoop
  where buildLoop = pushAlways (const $ staticLoop01 (e1, e2)) e2

splitThese :: Filterable f => f (These a b) -> (f a, f b)
splitThese f = (mapMaybe (preview here) f,  mapMaybe (preview there) f)

main :: IO ()
main = hspec $ do
  describe "DebugCycles" $ do
    it "throws EventLoopException on switchLoop01" $ do
      check switchLoop01
    it "throws EventLoopException on switchLoop02" $ do
      check switchLoop02
    it "throws EventLoopException on switchLoop03" $ do
      check switchLoop03
    it "throws EventLoopException on buildSwitchLoop" $ do
      check $ buildLoop switchLoop01
    xit "throws EventLoopException on mergeLoop" $ do
      check mergeLoop
    xit "throws EventLoopException on staticLoop01" $ do
      check staticLoop01
    xit "throws EventLoopException on staticLoop02" $ do
      check staticLoop02
    xit "throws EventLoopException on buildStaticLoop" $ do
      check buildStaticLoop
    xit "throws EventLoopException on coincidenceLoop" $ do
      check coincidenceLoop
    xit "throws EventLoopException on dynLoop" $ do
      check dynLoop
    xit "throws EventLoopException on buildCoincidenceLoop" $ do
      check $ buildLoop coincidenceLoop
 where
   milliseconds = (*1000)
   occs = [  This 1, This 2, That (), This 3, That (), This 1 ]
   check test = do
     let action = timeout (milliseconds 50) $ do
                    runApp' (test . splitThese) (Just <$> occs)
     action `shouldThrow` (const True :: Selector EventLoopException)
