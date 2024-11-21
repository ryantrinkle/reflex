{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Fix
import Data.Maybe
import qualified Data.Map as Map

import Reflex
import Reflex.EventWriter.Base
import Reflex.Network
import Reflex.Patch.MapWithMove
import Reflex.Test.SimpleHost

main :: IO ()
main = do
  let actions =  [ Increment, Update0th, Increment, Swap, Increment, Increment ]
  os <- runAppB testPatchMapWithMove $ map Just actions
  -- If the final counter value in the adjusted widgets corresponds to the number of times it has
  -- been incremented, we know that the networks haven't broken.
  let expectedCount = length $ ffilter (== Increment) actions
  -- let !True = last (last os) == [expectedCount,expectedCount] -- TODO re-enable this test after issue #369 has been resolved
  return ()

data PatchMapTestAction
  = Increment
  | Swap
  | Update0th
  deriving Eq

-- See https://github.com/reflex-frp/reflex/issues/369 for the bug that this is testing.
testPatchMapWithMove
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t PatchMapTestAction
  -> m (Behavior t [Int])
testPatchMapWithMove pulse = do
  let pulseAction = ffor pulse $ \case
        Increment -> Nothing
        Swap -> patchMapWithMove $ Map.fromList
          [ (0, NodeInfo (From_Move 1) (Just 1))
          , (1, NodeInfo (From_Move 0) (Just 0))
          ]
        Update0th -> patchMapWithMove $ Map.fromList
          [ (0, NodeInfo (From_Insert 'z') Nothing) ]
  (_, result) <- runBehaviorWriterT $ mdo
    counter <- foldDyn (+) 1 $ fmapMaybe (\e -> if isNothing e then Just 1 else Nothing) pulseAction
    _ <- mapMapWithAdjustWithMove
      (\_ _ -> networkHold
        (tellBehavior $ constant [])
        ((\t -> tellBehavior $ constant [t]) <$> updated counter))
      (Map.fromList $ zip [0..] "ab")
      (fmapMaybe id pulseAction)
    return ()
  return result
