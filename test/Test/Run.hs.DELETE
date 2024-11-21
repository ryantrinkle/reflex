{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Test.Run where

import Control.Monad
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.These

import Reflex
import Reflex.Host.Class

data AppIn t b e = AppIn
  { _appIn_behavior :: Behavior t b
  , _appIn_event :: Event t e
  }

data AppOut t b e = AppOut
  { _appOut_behavior :: Behavior t b
  , _appOut_event :: Event t e
  }

runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
       -> bIn
       -> [Maybe (These bIn eIn)]
       -> IO [[(bOut, Maybe eOut)]]
runApp app b0 input = runSpiderHost $ do
  (appInHoldE, pulseHoldTriggerRef) <- newEventWithTriggerRef
  (appInE, pulseEventTriggerRef) <- newEventWithTriggerRef
  appInB <- hold b0 appInHoldE
  (out, FireCommand fire) <- hostPerformEventT $ app $ AppIn
    { _appIn_event = appInE
    , _appIn_behavior = appInB
    }
  hnd <- subscribeEvent (_appOut_event out)
  mpulseB <- readRef pulseHoldTriggerRef
  mpulseE <- readRef pulseEventTriggerRef
  let readPhase = do
        b <- sample (_appOut_behavior out)
        frames <- sequence =<< readEvent hnd
        return (b, frames)
  forM input $ \case
    Nothing ->
      fire [] $ readPhase
    Just i -> case i of
      This b' -> case mpulseB of
        Nothing -> error "tried to fire in-behavior but ref was empty"
        Just pulseB -> fire [ pulseB :=> Identity b' ] $ readPhase
      That e' -> case mpulseE of
        Nothing -> error "tried to fire in-event but ref was empty"
        Just pulseE -> fire [ pulseE :=> Identity e' ] $ readPhase
      These b' e' -> case mpulseB of
        Nothing -> error "tried to fire in-behavior but ref was empty"
        Just pulseB -> case mpulseE of
          Nothing -> error "tried to fire in-event but ref was empty"
          Just pulseE -> fire [ pulseB :=> Identity b', pulseE :=> Identity e' ] $ readPhase

runApp' :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Event t eOut))
        -> [Maybe eIn]
        -> IO [[Maybe eOut]]
runApp' app input = do
  let app' = fmap (AppOut (pure ())) . app
  map (map snd) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

runAppB :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Behavior t bOut))
        -> [Maybe eIn]
        -> IO [[bOut]]
runAppB app input = do
  let app' = fmap (flip AppOut never) . app
  map (map fst) <$> runApp (app' . _appIn_event) () (map (fmap That) input)
