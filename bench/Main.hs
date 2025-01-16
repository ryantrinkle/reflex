{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- The instance for NFData (TVar a) is an orphan, but necessary here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Criterion.Main
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Misc
import Data.IORef
import Data.Maybe (fromJust)
import Reflex
import Reflex.Host.Class

main :: IO ()
main = defaultMain
  [ bgroup "micro" micros ]

instance NFData (TVar a) where
  rnf x = seq x ()

newtype WHNF a = WHNF a
instance NFData (WHNF a) where
  rnf (WHNF a) = seq a ()

withSetup :: NFData b => String -> SpiderHost Global a -> (a -> SpiderHost Global b) -> Benchmark
withSetup name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . nfIO $ runSpiderHost (action a)

withSetupWHNF :: String -> SpiderHost Global a -> (a -> SpiderHost Global b) -> Benchmark
withSetupWHNF name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . whnfIO $ runSpiderHost (action a)

micros :: [Benchmark]
micros =
  [ bench "newIORef" $ whnfIO $ void $ newIORef ()
  , env (newIORef (42 :: Int)) (bench "readIORef" . whnfIO . readIORef)
  , bench "newTVar" $ whnfIO $ void $ newTVarIO ()
  , env (newTVarIO (42 :: Int)) (bench "readTVar" . whnfIO . readTVarIO)
  , bench "newEventWithTrigger" $ whnfIO . void $ runSpiderHost $ newEventWithTrigger $
      \trigger -> return () <$ evaluate trigger
  , bench "newEventWithTriggerRef" $ whnfIO . void $ runSpiderHost newEventWithTriggerRef
  , withSetupWHNF "subscribeEvent" newEventWithTriggerRef $ subscribeEvent . fst
  , withSetupWHNF "subscribeSwitch"
    (join $ hold <$> fmap fst newEventWithTriggerRef <*> fmap fst newEventWithTriggerRef)
    (subscribeEvent . switch)
  , withSetupWHNF "subscribeMerge(1)" (setupMerge 1) $ \(ev,_) -> subscribeEvent ev
  , withSetupWHNF "subscribeMerge(100)" (setupMerge 100) (subscribeEvent . fst)
  , withSetupWHNF "subscribeMerge(10000)" (setupMerge 10000) (subscribeEvent . fst)
  , bench "runHostFrame" $ whnfIO $ runSpiderHost $ runHostFrame $ return ()
  , withSetupWHNF "fireEventsAndRead(single/single)"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> fireAndRead trigger (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly"
    (newEventWithTriggerRef >>= subscribePair)
    (\(_, trigger) -> do
        key <- fromJust <$> liftIO (readIORef trigger)
        fireEvents [key :=> Identity (42 :: Int)])
  , withSetupWHNF "fireEventsAndRead(head/merge1)"
    (setupMerge 1 >>= subscribePair)
    (\(subd, t:_) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:_) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge10000)"
      (setupMerge 10000 >>= subscribePair)
      (\(subd, t:_) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(_, t:_) -> do
        key <- fromJust <$> liftIO (readIORef t)
        fireEvents [key :=> Identity (42 :: Int)])
  , withSetupWHNF "hold" newEventWithTriggerRef $ \(ev, _) -> hold (42 :: Int) ev
  , withSetupWHNF "sample" (newEventWithTriggerRef >>= hold (42 :: Int) . fst) sample
  ]

setupMerge :: Int
           -> SpiderHost Global ( Event (SpiderTimeline Global) (DMap (Const2 Int a) Identity)
                                , [IORef (Maybe (EventTrigger Spider a))]
                                )
setupMerge num = do
  (evs, triggers) <- unzip <$> replicateM num newEventWithTriggerRef
  let !m = DMap.fromList [Const2 i :=> v | (i,v) <- zip [0..] evs]
  pure (merge m, triggers)

subscribePair :: (Event (SpiderTimeline Global) a, b) -> SpiderHost Global (EventHandle (SpiderTimeline Global) a, b)
subscribePair (ev, b) = (,b) <$> subscribeEvent ev

fireAndRead :: IORef (Maybe (EventTrigger (SpiderTimeline Global) a)) -> a -> EventHandle (SpiderTimeline Global) b
            -> SpiderHost Global (Maybe b)
fireAndRead trigger val subd = do
  key <- fromJust <$> liftIO (readIORef trigger)
  fireEventsAndRead [key :=> Identity val] $ readEvent subd >>= sequence
