{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- There are two expected orphan instances in this module:
--   * MonadSample (Pure t) ((->) t)
--   * MonadHold (Pure t) ((->) t)
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module: Reflex.Pure
-- Description:
--   This module provides a pure implementation of Reflex, which is intended to
--   serve as a reference for the semantics of the Reflex class.  All
--   implementations of Reflex should produce the same results as this
--   implementation, although performance and laziness/strictness may differ.
module Reflex.Pure
  ( Pure
  , Behavior (..)
  , Event (..)
  , Dynamic (..)
  , Incremental (..)
  ) where

import Control.Monad
import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare)
import qualified Data.Dependent.Map as DMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.MemoTrie
import Data.Monoid
import Data.Type.Coercion
import Reflex.Class
import Data.Kind (Type)

-- | A completely pure-functional 'Reflex' timeline, identifying moments in time
-- with the type @/t/@.
data Pure (t :: Type)

-- | The 'Enum' instance of @/t/@ must be dense: for all @/x :: t/@, there must not exist
-- any @/y :: t/@ such that @/'pred' x < y < x/@. The 'HasTrie' instance will be used
-- exclusively to memoize functions of @/t/@, not for any of its other capabilities.
instance (Enum t, HasTrie t, Ord t) => Reflex (Pure t) where

  newtype Behavior (Pure t) a = Behavior { unBehavior :: t -> a }
  newtype Event (Pure t) a = Event { unEvent :: t -> Maybe a }
  newtype Dynamic (Pure t) a = Dynamic { unDynamic :: t -> (a, Maybe a) }
  newtype Incremental (Pure t) p = Incremental { unIncremental :: t -> (PatchTarget p, Maybe p) }

  type PushM (Pure t) = (->) t
  type PullM (Pure t) = (->) t

  never :: Event (Pure t) a
  never = Event $ \_ -> Nothing

  constant :: a -> Behavior (Pure t) a
  constant x = Behavior $ \_ -> x

  push :: (a -> PushM (Pure t) (Maybe b)) -> Event (Pure t) a -> Event (Pure t) b
  push f e = Event $ memo $ \t -> unEvent e t >>= \o -> f o t

  pushCheap :: (a -> PushM (Pure t) (Maybe b)) -> Event (Pure t) a -> Event (Pure t) b
  pushCheap = push

  pull :: PullM (Pure t) a -> Behavior (Pure t) a
  pull = Behavior . memo

  -- [UNUSED_CONSTRAINT]: The following type signature for merge will produce a
  -- warning because the GCompare instance is not used; however, removing the
  -- GCompare instance produces a different warning, due to that constraint
  -- being present in the original class definition.

  --mergeG :: GCompare k => (forall a. q a -> Event (Pure t) (v a))
  --   -> DMap k q -> Event (Pure t) (DMap k v)
  mergeG nt events = Event $ memo $ \t ->
    let currentOccurrences = DMap.mapMaybeWithKey (\_ q -> case nt q of Event a -> a t) events
    in if DMap.null currentOccurrences
       then Nothing
       else Just currentOccurrences

--  The instance signature doeesn't compile, leave commented for documentation
--  fanG :: GCompare k => Event (Pure t) (DMap k v) -> EventSelectorG (Pure t) k v
  fanG e = EventSelectorG $ \k -> Event $ \t -> unEvent e t >>= DMap.lookup k

  switch :: Behavior (Pure t) (Event (Pure t) a) -> Event (Pure t) a
  switch b = Event $ memo $ \t -> unEvent (unBehavior b t) t

  coincidence :: Event (Pure t) (Event (Pure t) a) -> Event (Pure t) a
  coincidence e = Event $ memo $ \t -> unEvent e t >>= \o -> unEvent o t

  current :: Dynamic (Pure t) a -> Behavior (Pure t) a
  current d = Behavior $ \t -> fst $ unDynamic d t

  updated :: Dynamic (Pure t) a -> Event (Pure t) a
  updated d = Event $ \t -> snd $ unDynamic d t

  unsafeBuildDynamic :: PullM (Pure t) a -> Event (Pure t) a -> Dynamic (Pure t) a
  unsafeBuildDynamic readV0 v' = Dynamic $ \t -> (readV0 t, unEvent v' t)

  -- See UNUSED_CONSTRAINT, above.

  --unsafeBuildIncremental :: Patch p => PullM (Pure t) a -> Event (Pure t) (p
  --a) -> Incremental (Pure t) p a
  unsafeBuildIncremental readV0 p = Incremental $ \t -> (readV0 t, unEvent p t)

  mergeIncrementalG = mergeIncrementalImpl
  mergeIncrementalWithMoveG = mergeIncrementalImpl

  currentIncremental i = Behavior $ \t -> fst $ unIncremental i t

  updatedIncremental i = Event $ \t -> snd $ unIncremental i t

  incrementalToDynamic i = Dynamic $ \t ->
    let (old, mPatch) = unIncremental i t
        e = case mPatch of
          Nothing -> Nothing
          Just patch -> apply patch old
    in (old, e)
  behaviorCoercion Coercion = Coercion
  eventCoercion Coercion = Coercion
  dynamicCoercion Coercion = Coercion
  incrementalCoercion Coercion Coercion = Coercion

  fanInt e = EventSelectorInt $ \k -> Event $ \t -> unEvent e t >>= IntMap.lookup k

  mergeIntIncremental = mergeIntIncrementalImpl

mergeIncrementalImpl :: (PatchTarget p ~ DMap k q, GCompare k)
  => (forall a. q a -> Event (Pure t) (v a))
  -> Incremental (Pure t) p -> Event (Pure t) (DMap k v)
mergeIncrementalImpl nt i = Event $ \t ->
  let results = DMap.mapMaybeWithKey (\_ q -> case nt q of Event e -> e t) $ fst $ unIncremental i t
  in if DMap.null results
     then Nothing
     else Just results

mergeIntIncrementalImpl :: (PatchTarget p ~ IntMap (Event (Pure t) a)) => Incremental (Pure t) p -> Event (Pure t) (IntMap a)
mergeIntIncrementalImpl i = Event $ \t ->
  let results = IntMap.mapMaybeWithKey (\_ (Event e) -> e t) $ fst $ unIncremental i t
  in if IntMap.null results
     then Nothing
     else Just results

instance Functor (Dynamic (Pure t)) where
  fmap f d = Dynamic $ \t -> let (cur, upd) = unDynamic d t
                             in (f cur, fmap f upd)

instance Applicative (Dynamic (Pure t)) where
  pure a = Dynamic $ \_ -> (a, Nothing)
  (<*>) = ap

instance Monad (Dynamic (Pure t)) where
  return = pure
  (x :: Dynamic (Pure t) a) >>= (f :: a -> Dynamic (Pure t) b) = Dynamic $ \t ->
    let (curX :: a, updX :: Maybe a) = unDynamic x t
        (cur :: b, updOuter :: Maybe b) = unDynamic (f curX) t
        (updInner :: Maybe b, updBoth :: Maybe b) = case updX of
          Nothing -> (Nothing, Nothing)
          Just nextX -> let (c, u) = unDynamic (f nextX) t
                        in (Just c, u)
    in (cur, getFirst $ mconcat $ map First [updBoth, updOuter, updInner])

instance MonadSample (Pure t) ((->) t) where

  sample :: Behavior (Pure t) a -> (t -> a)
  sample = unBehavior

instance (Enum t, HasTrie t, Ord t) => MonadHold (Pure t) ((->) t) where

  hold :: a -> Event (Pure t) a -> t -> Behavior (Pure t) a
  hold initialValue e initialTime = Behavior f
    where f = memo $ \sampleTime ->
            -- Really, the sampleTime should never be prior to the initialTime,
            -- because that would mean the Behavior is being sampled before
            -- being created.
            if sampleTime <= initialTime
            then initialValue
            else let lastTime = pred sampleTime
                 in fromMaybe (f lastTime) $ unEvent e lastTime

  holdDyn v0 = buildDynamic (return v0)

  buildDynamic :: (t -> a) -> Event (Pure t) a -> t -> Dynamic (Pure t) a
  buildDynamic initialValue e initialTime =
    let Behavior f = hold (initialValue initialTime) e initialTime
    in Dynamic $ \t -> (f t, unEvent e t)

  holdIncremental :: Patch p => PatchTarget p -> Event (Pure t) p -> t -> Incremental (Pure t) p
  holdIncremental initialValue e initialTime = Incremental $ \t -> (f t, unEvent e t)
    where f = memo $ \sampleTime ->
            -- Really, the sampleTime should never be prior to the initialTime,
            -- because that would mean the Behavior is being sampled before
            -- being created.
            if sampleTime <= initialTime
            then initialValue
            else let lastTime = pred sampleTime
                     lastValue = f lastTime
                 in case unEvent e lastTime of
                   Nothing -> lastValue
                   Just x -> fromMaybe lastValue $ apply x lastValue

  headE = slowHeadE
  now t = Event $ guard . (t ==)
