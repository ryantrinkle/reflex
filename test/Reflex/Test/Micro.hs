{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Reflex.Test.Micro (testCases) where

import Reflex
import Reflex.TestPlan

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Foldable
import Data.Functor.Misc
import qualified Data.Map as Map
import Data.Monoid

import Prelude

pushDyn :: (Reflex t, MonadHold t m) => (a -> PushM t b) -> Dynamic t a -> m (Dynamic t b)
pushDyn f d = buildDynamic (sample (current d) >>= f) (pushAlways f (updated d))

foldedDyn :: (Reflex t, MonadHold t m) => (a -> a -> a) -> Dynamic t a -> m (Dynamic t a)
foldedDyn f d = fmap join $ flip buildDynamic never $ do
 a <- sample (current d)
 foldDyn f a (updated d)

scannedDyn :: (Reflex t, MonadHold t m) => Dynamic t a -> m (Dynamic t [a])
scannedDyn = fmap (fmap reverse) . foldedDyn (<>) . fmap pure

scanInnerDyns :: (Reflex t, MonadHold t m) => Dynamic t (Dynamic t a) -> m (Dynamic t [a])
scanInnerDyns d = do
  scans <- scannedDyn d
  return (join (fmap distributeListOverDynPure scans))



{-# ANN testCases "HLint: ignore Functor law" #-}
testCases :: [(String, TestCase)]
testCases =
  [ testB "hold"  $ hold "0" =<< events1

  , testB "count" $ do
      b <- current <$> (count =<< events2)
      return $ (+ (0::Int)) <$> b

  , testB "pull-1"  $ do
      b <- hold "0" =<< events1
      return (pull $ sample $ pull $ sample b)

  , testB "pull-2" $ do
      b1 <- behavior1
      return (pull $ liftA2 (<>) (sample b1) (sample b1))

  , testB "pull-3" $ do
      b1 <- behavior1
      b2 <- behavior2
      return (pull $ liftA2 (<>) (sample b1) (sample b2))

  , testB "pull-4" $ do
      es <- planList ["a", "b", "c"]
      e <- plan [(0, ())]
      b <- hold (constant "") $
        pushAlways (const $ hold "z" es) e
      return (join b)

  , testE "id" $ do
      events2

  , testE "fmap-id" $ do
      e <- events2
      return $ fmap id e

  , testE "tag-1" $ do
      b1 <- behavior1
      e <- events2
      return (tag b1 e)

  , testE "tag-2" $ do
      b1 <- behavior1
      e <- events2
      return (tag (map toUpper <$>  b1) e)

  , testE "attach-1" $ do
      b1 <- behavior1
      e <- events2
      return (attachWith (++) (map toUpper <$> b1) e)

  , testE "leftmost" $ liftA2 leftmost2 events1 events2

  , testE "appendEvents-1" $ liftA2 mappend events1 events2

  , testE "appendEvents-2" $ liftA2 mappend events3 events2

  , testE "merge-1" $ do
      e <- events1
      return $ leftmost ["x" <$ e, "y" <$ e]

  , testE "merge-2" $ do
      e <- events1
      let m = mergeMap $ Map.fromList [(1::Int, "y" <$ e), (2, "z" <$ e)]
      let ee = flip pushAlways e $ const $ return m
      return $ coincidence ee

  , testE "headE-1" $ do
      e <- events1
      headE $ leftmost [e, e]

  , testE "headE-2" $ do
      e <- events1
      b <- hold never (e <$ e)
      headE $ switch b

  , testE "switch-1" $ do
      e <- events1
      b <- hold never (e <$ e)
      return $ switch b

  , testE "switch-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) (e <$ e)

  , testE "switch-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
          switch <$> hold (leftmost ["x" <$ e, "y" <$ e, "z" <$ e]) never

  , testE "switch-4" $ do
      e <- events1
      switch <$> hold (deep e) (e <$ e)

  , testE "switch-5" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $
        return $ leftmost ["x" <$ e, "y" <$ e, "z" <$ e]

  , testE "switch-6" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold ("x" <$ e) (e <$ e)

  , testE "switchHoldPromptly-1" $ do
      e <- events1
      let e' = e <$ e
      switchHoldPromptly never $ e <$ e'

  , testE "switchHoldPromptly-2" $ do
      e <- events1
      switchHoldPromptly never $ deep (e <$ e)

  , testE "switchHoldPromptly-3" $ do
      e <- events1
      switchHoldPromptly never $ (e <$ deep e)

  , testE "switchHoldPromptly-4" $ do
      e <- events1
      switchHoldPromptly never $ (deep e <$ e)

  , testE "switch-5" $ do
      e <- events1
      switch <$> hold never (deep e <$ e)

  , testE "switchHoldPromptly-5" $ do
    e <- events1
    switchHoldPromptly never $ flip push e $
      const (Just <$> headE e)

  , testE "switchHoldPromptly-6" $ do
      e <- events1
      switchHoldPromptly never $ flip pushAlways e $
        const (switchHoldPromptly e never)

  , testE "coincidence-1" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return e

  , testE "coincidence-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (deep e)

  , testE "coincidence-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (coincidence (e <$ e))

  , testE "coincidence-4" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const (headE e)

  , testE "coincidence-5" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = deep e
        return (coincidence (e' <$ e'))

  , testE "coincidence-6" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = coincidence (e <$ e)
        return $ deep e'

  , testE "coincidence-7" $ do
      e <- events1
      return $ coincidence (deep e <$ e)

  , testE "coincidence-8" $ do
      e1 <- events1
      e2 <- events2
      pure . coincidence . fmap (const e1) $ e2

  , testB "holdWhileFiring" $ do
      e <- events1
      eo <- headE e
      bb <- hold (constant "x") $ pushAlways (const $ hold "a" eo) eo
      return $ pull $ sample =<< sample bb


  , testB "foldDynWhileFiring"  $ do
    e <- events1
    d <- foldDyn (:) [] $
      pushAlways (\a -> foldDyn (:) [a] e) e

    return $ current (join (fmap distributeListOverDynPure d))

  , testE "joinDyn" $ do
      e <- events1
      bb <- hold "b" e
      bd <- hold never . fmap (const e) =<< headE e

      eOuter <- pushAlways sample . fmap (const bb) <$> headE e
      let eInner = switch bd
      return $ leftmost [eOuter, eInner]

  , testB "foldDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      return (current d)

  , testB "mapDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      return $ current $ fmap (map toUpper) d

  , testB "combineDyn"  $ do
      d1 <- foldDyn (++) "0" =<< events1
      d2 <- fmap (fmap (map toUpper)) $ foldDyn (++) "0" =<< events2

      return $ current $ zipDynWith (<>) d1 d2

  , testB "buildDynamicStrictness"  $ do
      rec
        d'' <- pushDyn return d'
        d' <- pushDyn return d
        d <- holdDyn "0" =<< events1

      _ <- sample (current d'')
      return (current d'')

  , testB "factorDyn"  $ do
      d <- holdDyn (Left "a") =<< eithers

      eithers' <- eitherDyn d
      let unFactor = either id id
      return $ current (join (fmap unFactor eithers'))

  , testB "pushDynDeep"  $ do
      _ <- events1
      _ <- events2

      d1 <- holdDyn "d1" =<< events1
      d2 <- holdDyn "d2" =<< events2

      d <- flip pushDyn d1 $ \a ->
        flip pushDyn d2 $ \b ->
          flip pushDyn d1 $ \c ->
            return (a <> b <> c)

      d' <- pushDyn scanInnerDyns d >>= scanInnerDyns
      return $ current d'

  , testE "fan-1" $ do
      e <- fmap toMap <$> events1
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE "fan-2" $ do
      e <- fmap toMap <$> events3
      let es = select (fanMap e) . Const2 <$> values

      return (mergeList es)

  , testE "fan-3" $ do
      f <- fanMap . fmap toMap <$> events3
      return $  select f (Const2 'c')

  , testE "fan-4" $ do
      e <- fmap toMap <$> events1
      return $ toUpper <$> select (fanMap e) (Const2 'a')

  , testE "fan-5" $ do
      e <- fmap toMap <$> events2
      return $ toUpper <$> select (fanMap e) (Const2 'c')

  , testE "fan-6" $ do
      f <- fanMap . fmap toMap <$> events1
      return $ toList <$> mergeList [ select f (Const2 'b'), select f (Const2 'b'), select f (Const2 'e'), select f (Const2 'e') ]

  , testE "difference" $ do
      e1 <- events1
      e2 <- events2
      return $ e1 `difference ` e2

  , testE "lazy-hold" $ do
      let lazyHold :: forall t m. (Reflex t, MonadHold t m, MonadFix m) => m (Event t ())
          lazyHold = do
            rec !b <- hold never e
                let e = never <$ switch b
            return $ void e
      lazyHold
  , testE "now-1" $ do
      e1 <- events1
      switchHoldPromptly never . pushAlways (\a -> fmap (a <$) now) $ e1
  , testE "now-2" $ do
      e1 <- events1
      let e = pushAlways (\a -> if a == "a" then now else return never) e1
      x <- accumDyn (<>) never e 
      return . coincidence $ updated x
  ] where

    events1, events2, events3 ::  TestPlan t m => m (Event t String)
    events1 = plan [(1, "a"), (2, "b"), (5, "c"), (7, "d"), (8, "e")]
    events2 = plan [(1, "e"), (3, "d"), (4, "c"), (6, "b"), (7, "a")]
    events3 = liftA2 mappend events1 events2

    eithers ::  TestPlan t m => m (Event t (Either String String))
    eithers = plan [(1, Left "e"), (3, Left "d"), (4, Right "c"), (6, Right "b"), (7, Left "a")]


    values = "abcde"
    toMap str = Map.fromList $ map (\c -> (c, c)) str

    behavior1, behavior2 :: forall t m. TestPlan t m => m (Behavior t String)
    behavior1 =  hold "1" =<< events1
    behavior2 =  hold "2" =<< events2

    deep e = leftmost [e, e]
    leftmost2 e1 e2 = leftmost [e1, e2]
