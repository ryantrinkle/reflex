{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- |
-- Module:
--   Reflex.Class
-- Description:
--   This module contains the Reflex interface, as well as a variety of
--   convenience functions for working with 'Event's, 'Behavior's, and other
--   signals.
module Reflex.Class
  ( module Data.Patch
    -- * Primitives
  , Reflex (..)
  , mergeInt
  , coerceBehavior
  , coerceEvent
  , coerceDynamic
  , coerceIncremental
  , MonadSample (..)
  , MonadHold (..)
    -- ** 'fan' related types
  , EventSelector (..)
  , EventSelectorG (..)
  , EventSelectorInt (..)
    -- * Convenience functions
  , constDyn
  , pushAlways
    -- ** Combining 'Event's
  , leftmost
  , merge
  , mergeIncremental
  , mergeIncrementalWithMove
  , mergeMap
  , mergeIntMap
  , mergeMapIncremental
  , mergeMapIncrementalWithMove
  , mergeIntMapIncremental
  , coincidencePatchMap
  , coincidencePatchMapWithMove
  , coincidencePatchIntMap
  , mergeList
  , mergeWith
  , difference
  , alignEventWithMaybe
    -- ** Breaking up 'Event's
  , splitE
  , fan
  , fanEither
  , fanThese
  , fanMap
  , dmapToThese
  , EitherTag (..)
  , eitherToDSum
  , dsumToEither
  , factorEvent
  , filterEventKey
    -- ** Collapsing 'Event . Event'
  , switchHold
  , switchHoldPromptly
  , switchHoldPromptOnly
  , switchHoldPromptOnlyIncremental
    -- ** Using 'Event's to sample 'Behavior's
  , tag
  , tagMaybe
  , attach
  , attachWith
  , attachWithMaybe
    -- ** Blocking an 'Event' based on a 'Behavior'
  , gate
    -- ** Combining 'Dynamic's
  , distributeDMapOverDynPure
  , distributeDMapOverDynPureG
  , distributeListOverDyn
  , distributeListOverDynWith
  , zipDyn
  , zipDynWith
    -- ** Accumulating state
  , Accumulator (..)
  , accumDyn
  , accumMDyn
  , accumMaybeDyn
  , accumMaybeMDyn
  , mapAccumDyn
  , mapAccumMDyn
  , mapAccumMaybeDyn
  , mapAccumMaybeMDyn
  , accumB
  , accumMB
  , accumMaybeB
  , accumMaybeMB
  , mapAccumB
  , mapAccumMB
  , mapAccumMaybeB
  , mapAccumMaybeMB
  , mapAccum_
  , mapAccumM_
  , mapAccumMaybe_
  , mapAccumMaybeM_
  , accumIncremental
  , accumMIncremental
  , accumMaybeIncremental
  , accumMaybeMIncremental
  , mapAccumIncremental
  , mapAccumMIncremental
  , mapAccumMaybeIncremental
  , mapAccumMaybeMIncremental
  , zipListWithEvent
  , numberOccurrences
  , numberOccurrencesFrom
  , numberOccurrencesFrom_
  , (<@>)
  , (<@)
  , tailE
  , headTailE
  , takeWhileE
  , takeWhileJustE
  , dropWhileE
  , takeDropWhileJustE
  , switcher
    -- * Debugging functions
  , traceEvent
  , traceEventWith
    -- * Unsafe functions
  , unsafeDynamic
  , unsafeMapIncremental
    -- * 'Filterable' convenience functions
  , FunctorMaybe -- fmapMaybe is purposely not exported from deprecated 'FunctorMaybe' and the new alias is exported instead
  , mapMaybe -- Re-exported for convenience
  , fmapMaybe
  , fforMaybe
  , ffilter
  , filterLeft
  , filterRight
    -- * Miscellaneous convenience functions
  , ffor
  , ffor2
  , ffor3
    -- * Deprecated functions
  , switchPromptly
  , switchPromptOnly
  -- * "Cheap" functions
  , fmapMaybeCheap
  , mapMaybeCheap
  , fmapCheap
  , fforCheap
  , fforMaybeCheap
  , pushAlwaysCheap
  , tagCheap
  , mergeWithCheap
  , mergeWithCheap'
    -- * Slow, but general, implementations
  , slowHeadE
  ) where

#ifdef MIN_VERSION_semialign
import Prelude hiding (zip, zipWith)

#if MIN_VERSION_these(0,8,0)
import Data.These.Combinators (justThese)
#endif
#if MIN_VERSION_semialign(1,1,0)
import Data.Zip (Zip (..), Unzip (..))
#endif
#endif

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.Writer (WriterT)
import Data.Align
import Data.Bifunctor
import Data.Coerce
import Data.Default
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.Functor.Product
import Data.GADT.Compare (GEq (..), GCompare (..))
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Misc
import Data.Functor.Plus
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Semigroup (Semigroup (..))
import Data.Some (Some(Some))
import Data.String
import Data.These
import Data.Type.Coercion
import Data.Type.Equality ((:~:) (..))
import Reflex.FunctorMaybe (FunctorMaybe)
import qualified Reflex.FunctorMaybe
import Data.Patch
import qualified Data.Patch.MapWithMove as PatchMapWithMove
import Witherable (Filterable(..))
import qualified Witherable as W

import Debug.Trace (trace)

-- | The 'Reflex' class contains all the primitive functionality needed for
-- Functional Reactive Programming (FRP).  The @/t/@ type parameter indicates
-- which "timeline" is in use.  Timelines are fully-independent FRP contexts,
-- and the type of the timeline determines the FRP engine to be used.  For most
-- purposes, the 'Reflex.Spider' implementation is recommended.
class ( MonadHold t (PushM t)
      , MonadSample t (PullM t)
      , MonadFix (PushM t)
      , Functor (Dynamic t)
      , Applicative (Dynamic t) -- Necessary for GHC <= 7.8
      , Monad (Dynamic t)
      ) => Reflex t where
  -- | A container for a value that can change over time.  'Behavior's can be
  -- sampled at will, but it is not possible to be notified when they change
  data Behavior t :: Type -> Type
  -- | A stream of occurrences.  During any given frame, an 'Event' is either
  -- occurring or not occurring; if it is occurring, it will contain a value of
  -- the given type (its "occurrence type")
  data Event t :: Type -> Type
  -- | A container for a value that can change over time and allows
  -- notifications on changes.  Basically a combination of a 'Behavior' and an
  -- 'Event', with a rule that the 'Behavior' will change if and only if the
  -- 'Event' fires.
  data Dynamic t :: Type -> Type
  -- | An 'Incremental' is a more general form of  a 'Dynamic'.
  -- Instead of always fully replacing the value, only parts of it can be patched.
  -- This is only needed for performance critical code via `mergeIncremental` to make small
  -- changes to large values.
  data Incremental t :: Type -> Type
  -- | A monad for doing complex push-based calculations efficiently
  type PushM t :: Type -> Type
  -- | A monad for doing complex pull-based calculations efficiently
  type PullM t :: Type -> Type
  -- | An 'Event' with no occurrences
  never :: Event t a
  -- | Create a 'Behavior' that always has the given value
  constant :: a -> Behavior t a --TODO: Refactor to use 'pure' from Applicative instead; however, we need to make sure that encouraging Applicative-style use of 'Behavior's doesn't have a negative performance impact
  -- | Create an 'Event' from another 'Event'; the provided function can sample
  -- 'Behavior's and hold 'Event's, and use the results to produce a occurring
  -- (Just) or non-occurring (Nothing) result
  push :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b
  -- | Like 'push' but intended for functions that the implementation can consider cheap to compute for performance considerations. WARNING: The function passed to 'pushCheap' may be run multiple times without any caching.
  pushCheap :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b
  -- | Create a 'Behavior' by reading from other 'Behavior's; the result will be
  -- recomputed whenever any of the read 'Behavior's changes
  pull :: PullM t a -> Behavior t a
  -- | Merge a collection of events; the resulting 'Event' will only occur if at
  -- least one input event is occurring, and will contain all of the input keys
  -- that are occurring simultaneously

   --TODO: Generalize to get rid of DMap use --TODO: Provide a type-level guarantee that the result is not empty
  mergeG :: GCompare k => (forall a. q a -> Event t (v a))
         -> DMap k q -> Event t (DMap k v)

  -- | Efficiently fan-out an event to many destinations.  You should save the
  -- result in a @let@-binding, and then repeatedly 'selectG' on the result to
  -- create child events
  fanG :: GCompare k => Event t (DMap k v) -> EventSelectorG t k v

  -- | Create an 'Event' that will occur whenever the currently-selected input
  -- 'Event' occurs
  switch :: Behavior t (Event t a) -> Event t a
  -- | Create an 'Event' that will occur whenever the input event is occurring -- and its occurrence value, another 'Event', is also occurring.
  --   You maybe looking for '@switchHold@ @never@' instead.
  coincidence :: Event t (Event t a) -> Event t a
  -- | Extract the 'Behavior' of a 'Dynamic'.
  current :: Dynamic t a -> Behavior t a
  -- | Extract the 'Event' of the 'Dynamic'.
  updated :: Dynamic t a -> Event t a
  -- | Create a new 'Dynamic'.  The given 'PullM' must always return the most
  -- recent firing of the given 'Event', if any.
  unsafeBuildDynamic :: PullM t a -> Event t a -> Dynamic t a
  -- | Create a new 'Incremental'.  The given "PullM"'s value must always change
  -- in the same way that the accumulated application of patches would change
  -- that value.
  unsafeBuildIncremental :: Patch p => PullM t (PatchTarget p) -> Event t p -> Incremental t p
  -- | Create a merge whose parents can change over time
  mergeIncrementalG :: GCompare k
    => (forall a. q a -> Event t (v a))
    -> Incremental t (PatchDMap k q)
    -> Event t (DMap k v)
  -- | Experimental: Create a merge whose parents can change over time; changing the key of an Event is more efficient than with mergeIncremental
  mergeIncrementalWithMoveG :: GCompare k
    => (forall a. q a -> Event t (v a))
    -> Incremental t (PatchDMapWithMove k q) -> Event t (DMap k v)
  -- | Extract the 'Behavior' component of an 'Incremental'
  currentIncremental :: Patch p => Incremental t p -> Behavior t (PatchTarget p)
  -- | Extract the 'Event' component of an 'Incremental'
  updatedIncremental :: Patch p => Incremental t p -> Event t p
  -- | Convert an 'Incremental' to a 'Dynamic'
  incrementalToDynamic :: Patch p => Incremental t p -> Dynamic t (PatchTarget p)
  -- | Construct a 'Coercion' for a 'Behavior' given an 'Coercion' for its
  -- occurrence type
  behaviorCoercion :: Coercion a b -> Coercion (Behavior t a) (Behavior t b)
  -- | Construct a 'Coercion' for an 'Event' given an 'Coercion' for its
  -- occurrence type
  eventCoercion :: Coercion a b -> Coercion (Event t a) (Event t b)
  -- | Construct a 'Coercion' for a 'Dynamic' given an 'Coercion' for its
  -- occurrence type
  dynamicCoercion :: Coercion a b -> Coercion (Dynamic t a) (Dynamic t b)
  -- | Construct a 'Coercion' for an 'Incremental' given 'Coercion's for its
  -- patch target and patch types.
  incrementalCoercion
    :: Coercion (PatchTarget a) (PatchTarget b) -> Coercion a b -> Coercion (Incremental t a) (Incremental t b)
  mergeIntIncremental :: Incremental t (PatchIntMap (Event t a)) -> Event t (IntMap a)
  fanInt :: Event t (IntMap a) -> EventSelectorInt t a

-- | Efficiently fan-out an event to many destinations. You should save the
-- result in a @let@-binding, and then repeatedly 'select' on the result to
-- create child events
fan :: forall t k. (Reflex t, GCompare k)
    => Event t (DMap k Identity) -> EventSelector t k
    --TODO: Can we help enforce the partial application discipline here?  The combinator is worthless without it
fan e = EventSelector (fixup (selectG (fanG e) :: k a -> Event t (Identity a)) :: forall a. k a -> Event t a)
  where
    fixup :: forall a. (k a -> Event t (Identity a)) -> k a -> Event t a
    fixup = case eventCoercion Coercion :: Coercion (Event t (Identity a)) (Event t a) of
              Coercion -> coerce

--TODO: Specialize this so that we can take advantage of knowing that there's no changing going on
-- | Constructs a single 'Event' out of a map of events. The output event may fire with multiple
-- keys simultaneously.
mergeInt :: Reflex t => IntMap (Event t a) -> Event t (IntMap a)
mergeInt m = mergeIntIncremental $ unsafeBuildIncremental (return m) never

-- | Coerce a 'Behavior' between representationally-equivalent value types
coerceBehavior :: (Reflex t, Coercible a b) => Behavior t a -> Behavior t b
coerceBehavior = coerceWith $ behaviorCoercion Coercion

-- | Coerce an 'Event' between representationally-equivalent occurrence types
coerceEvent :: (Reflex t, Coercible a b) => Event t a -> Event t b
coerceEvent = coerceWith $ eventCoercion Coercion

-- | Coerce a 'Dynamic' between representationally-equivalent value types
coerceDynamic :: (Reflex t, Coercible a b) => Dynamic t a -> Dynamic t b
coerceDynamic = coerceWith $ dynamicCoercion Coercion

-- | Coerce an 'Incremental' between representationally-equivalent value types
coerceIncremental
  :: (Reflex t, Coercible a b, Coercible (PatchTarget a) (PatchTarget b))
  => Incremental t a -> Incremental t b
coerceIncremental = coerceWith $ incrementalCoercion Coercion Coercion

-- | Construct a 'Dynamic' from a 'Behavior' and an 'Event'.  The 'Behavior'
-- __must__ change when and only when the 'Event' fires, such that the
-- 'Behavior''s value is always equal to the most recent firing of the 'Event';
-- if this is not the case, the resulting 'Dynamic' will behave
-- nondeterministically.
unsafeDynamic :: Reflex t => Behavior t a -> Event t a -> Dynamic t a
unsafeDynamic = unsafeBuildDynamic . sample

-- | Construct a 'Dynamic' value that never changes
constDyn :: Reflex t => a -> Dynamic t a
constDyn = pure

instance (Reflex t, Default a) => Default (Dynamic t a) where
  def = pure def

-- | 'MonadSample' designates monads that can read the current value of a
-- 'Behavior'.  This includes both 'PullM' and 'PushM'.
class (Applicative m, Monad m) => MonadSample t m | m -> t where
  -- | Get the current value in the 'Behavior'
  sample :: Behavior t a -> m a

-- | 'MonadHold' designates monads that can create new 'Behavior's based on
-- 'Event's; usually this will be 'PushM' or a monad based on it.  'MonadHold'
-- is required to create any stateful computations with Reflex.
class MonadSample t m => MonadHold t m where
  -- | Create a new 'Behavior' whose value will initially be equal to the given
  -- value and will be updated whenever the given 'Event' occurs.  The update
  -- takes effect immediately after the 'Event' occurs; if the occurrence that
  -- sets the 'Behavior' (or one that is simultaneous with it) is used to sample
  -- the 'Behavior', it will see the __old__ value of the 'Behavior', not the new
  -- one.
  hold :: a -> Event t a -> m (Behavior t a)
  default hold :: (m ~ f m', MonadTrans f, MonadHold t m') => a -> Event t a -> m (Behavior t a)
  hold v0 = lift . hold v0
  -- | Create a 'Dynamic' value using the given initial value that changes every
  -- time the 'Event' occurs.
  holdDyn :: a -> Event t a -> m (Dynamic t a)
  default holdDyn :: (m ~ f m', MonadTrans f, MonadHold t m') => a -> Event t a -> m (Dynamic t a)
  holdDyn v0 = lift . holdDyn v0
  -- | Create an 'Incremental' value using the given initial value that changes
  -- every time the 'Event' occurs.
  holdIncremental :: Patch p => PatchTarget p -> Event t p -> m (Incremental t p)
  default holdIncremental :: (Patch p, m ~ f m', MonadTrans f, MonadHold t m') => PatchTarget p -> Event t p -> m (Incremental t p)
  holdIncremental v0 = lift . holdIncremental v0
  buildDynamic :: PushM t a -> Event t a -> m (Dynamic t a)
  {-
  default buildDynamic :: (m ~ f m', MonadTrans f, MonadHold t m') => PullM t a -> Event t a -> m (Dynamic t a)
  buildDynamic getV0 = lift . buildDynamic getV0
  -}
  -- | Create a new 'Event' that only occurs only once, on the first occurrence of
  -- the supplied 'Event'.
  headE :: Event t a -> m (Event t a)
  -- | An event which only occurs at the current moment in time, such that:
  --
  -- > coincidence (pushAlways (\a -> (a <$) <$> now) e) = e
  --
  now :: m (Event t ())
  default now :: (m ~ f m', MonadTrans f, MonadHold t m') => m (Event t ())
  now = lift now

-- | Accumulate an 'Incremental' with the supplied initial value and the firings of the provided 'Event',
-- using the combining function to produce a patch.
accumIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> p)
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p)
accumIncremental f = accumMaybeIncremental $ \v o -> Just $ f v o

-- | Similar to 'accumIncremental' but the combining function runs in 'PushM'
accumMIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> PushM t p)
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p)
accumMIncremental f = accumMaybeMIncremental $ \v o -> Just <$> f v o

-- | Similar to 'accumIncremental' but allows filtering of updates (by dropping updates when the
-- combining function produces @Nothing@)
accumMaybeIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> Maybe p)
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p)
accumMaybeIncremental f = accumMaybeMIncremental $ \v o -> return $ f v o

-- | Similar to 'accumMaybeMIncremental' but the combining function runs in 'PushM'
accumMaybeMIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> PushM t (Maybe p))
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p)
accumMaybeMIncremental f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ currentIncremental d'
            f v o
      d' <- holdIncremental z e'
  return d'

-- | Accumulate an 'Incremental' by folding occurrences of an 'Event'
-- with a function that both accumulates and produces a value to fire
-- as an 'Event'. Returns both the accumulated value and the constructed
-- 'Event'.
mapAccumIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> (p, c))
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p, Event t c)
mapAccumIncremental f = mapAccumMaybeIncremental $ \v o -> bimap Just Just $ f v o

-- | Like 'mapAccumIncremental' but the combining function runs in 'PushM'
mapAccumMIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> PushM t (p, c))
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p, Event t c)
mapAccumMIncremental f = mapAccumMaybeMIncremental $ \v o -> bimap Just Just <$> f v o

-- | Accumulate an 'Incremental' by folding occurrences of an 'Event' with
-- a function that both optionally accumulates and optionally produces
-- a value to fire as a separate output 'Event'.
-- Note that because 'Nothing's are discarded in both cases, the output
-- 'Event' may fire even though the output 'Incremental' has not changed, and
-- the output 'Incremental' may update even when the output 'Event' is not firing.
mapAccumMaybeIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> (Maybe p, Maybe c))
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p, Event t c)
mapAccumMaybeIncremental f = mapAccumMaybeMIncremental $ \v o -> return $ f v o

-- | Like 'mapAccumMaybeIncremental' but the combining function is a 'PushM' action
mapAccumMaybeMIncremental
  :: (Reflex t, Patch p, MonadHold t m, MonadFix m)
  => (PatchTarget p -> b -> PushM t (Maybe p, Maybe c))
  -> PatchTarget p
  -> Event t b
  -> m (Incremental t p, Event t c)
mapAccumMaybeMIncremental f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ currentIncremental d'
            result <- f v o
            return $ case result of
              (Nothing, Nothing) -> Nothing
              _ -> Just result
      d' <- holdIncremental z $ mapMaybe fst e'
  return (d', mapMaybe snd e')

-- | A somewhat slow implementation of 'headE'
slowHeadE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
slowHeadE e = do
  rec be <- hold e $ fmapCheap (const never) e'
      let e' = switch be
  return e'

-- | An 'EventSelector' allows you to efficiently 'select' an 'Event' based on a
-- key.  This is much more efficient than filtering for each key with
-- 'mapMaybe'.
newtype EventSelector t k = EventSelector
  { -- | Retrieve the 'Event' for the given key.  The type of the 'Event' is
    -- determined by the type of the key, so this can be used to fan-out
    -- 'Event's whose sub-'Event's have different types.
    --
    -- Using 'EventSelector's and the 'fan' primitive is far more efficient than
    -- (but equivalent to) using 'mapMaybe' to select only the relevant
    -- occurrences of an 'Event'.
    select :: forall a. k a -> Event t a
  }

newtype EventSelectorG t k v = EventSelectorG
  { -- | Retrieve the 'Event' for the given key.  The type of the 'Event' is
    -- determined by the type of the key, so this can be used to fan-out
    -- 'Event's whose sub-'Event's have different types.
    --
    -- Using 'EventSelector's and the 'fan' primitive is far more efficient than
    -- (but equivalent to) using 'mapMaybe' to select only the relevant
    -- occurrences of an 'Event'.
    selectG :: forall a. k a -> Event t (v a)
  }

-- | Efficiently select an 'Event' keyed on 'Int'. This is more efficient than manually
-- filtering by key.
newtype EventSelectorInt t a = EventSelectorInt { selectInt :: Int -> Event t a }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadSample t m => MonadSample t (ReaderT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ReaderT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance (MonadSample t m, Monoid r) => MonadSample t (WriterT r m) where
  sample = lift . sample

instance (MonadHold t m, Monoid r) => MonadHold t (WriterT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance MonadSample t m => MonadSample t (StateT s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (StateT s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance MonadSample t m => MonadSample t (ExceptT e m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ExceptT e m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance (MonadSample t m, Monoid w) => MonadSample t (RWST r w s m) where
  sample = lift . sample

instance (MonadHold t m, Monoid w) => MonadHold t (RWST r w s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance MonadSample t m => MonadSample t (ContT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ContT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

-- | Create an 'Event' from another 'Event'.  The provided function can sample
-- 'Behavior's and hold 'Event's.
pushAlways :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlways f = push (fmap Just . f)

-- | Flipped version of 'fmap'.
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

-- | Rotated version of 'liftA2'.
ffor2 :: Applicative f => f a -> f b -> (a -> b -> c) -> f c
ffor2 a b f = liftA2 f a b

-- | Rotated version of 'liftA3'.
ffor3 :: Applicative f => f a -> f b -> f c -> (a -> b -> c -> d) -> f d
ffor3 a b c f = liftA3 f a b c

instance Reflex t => Applicative (Behavior t) where
  pure = constant
  f <*> x = pull $ sample f `ap` sample x
  _ *> b = b
  a <* _ = a

instance Reflex t => Apply (Behavior t) where
  (<.>) = (<*>)

instance Reflex t => Bind (Behavior t) where
  (>>-) = (>>=)

instance (Reflex t, Fractional a) => Fractional (Behavior t a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
  recip = fmap recip

instance Reflex t => Functor (Behavior t) where
  fmap f = pull . fmap f . sample

instance (Reflex t, IsString a) => IsString (Behavior t a) where
  fromString = pure . fromString

instance Reflex t => Monad (Behavior t) where
  a >>= f = pull $ sample a >>= sample . f
#if !MIN_VERSION_base(4,13,0)
  fail = error "Monad (Behavior t) does not support fail"
#endif

instance (Reflex t, Monoid a) => Monoid (Behavior t a) where
  mempty = constant mempty
  mconcat = pull . fmap mconcat . mapM sample

instance (Reflex t, Num a) => Num (Behavior t a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  fromInteger = pure . fromInteger
  negate = fmap negate
  signum = fmap signum

instance (Num a, Reflex t) => Num (Dynamic t a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate
  (-) = liftA2 (-)

instance (Reflex t, Semigroup a) => Semigroup (Behavior t a) where
  a <> b = pull $ liftM2 (<>) (sample a) (sample b)
  sconcat = pull . fmap sconcat . mapM sample
  stimes n = fmap $ stimes n

-- | Alias for 'mapMaybe'
fmapMaybe :: Filterable f => (a -> Maybe b) -> f a -> f b
fmapMaybe = mapMaybe

-- | Flipped version of 'mapMaybe'.
fforMaybe :: Filterable f => f a -> (a -> Maybe b) -> f b
fforMaybe = flip mapMaybe

-- | Filter 'f a' using the provided predicate.
ffilter :: Filterable f => (a -> Bool) -> f a -> f a
ffilter = W.filter

-- | Filter 'Left's from 'f (Either a b)' into 'a'.
filterLeft :: Filterable f => f (Either a b) -> f a
filterLeft = mapMaybe (either Just (const Nothing))

-- | Filter 'Right's from 'f (Either a b)' into 'b'.
filterRight :: Filterable f => f (Either a b) -> f b
filterRight = mapMaybe (either (const Nothing) Just)

-- | Left-biased event union (prefers left event on simultaneous
-- occurrence).
instance Reflex t => Alt (Event t) where
  ev1 <!> ev2 = leftmost [ev1, ev2]

-- | 'Event' intersection. Only occurs when both events are co-incident.
instance Reflex t => Apply (Event t) where
  evf <.> evx = mapMaybe f (align evf evx) where
    f (These g a) = Just (g a)
    f _           = Nothing


-- | 'Event' intersection (convenient interface to 'coincidence').
instance Reflex t => Bind (Event t) where
  evx >>- f = coincidence (f <$> evx)
  join = coincidence

instance Reflex t => Functor (Event t) where
  {-# INLINE fmap #-}
  fmap f = mapMaybe $ Just . f
  {-# INLINE (<$) #-}
  x <$ e = fmapCheap (const x) e

-- TODO Remove this instance
instance Reflex t => FunctorMaybe (Event t) where
  {-# INLINE fmapMaybe #-}
  fmapMaybe = mapMaybe

instance Reflex t => Filterable (Event t) where
  {-# INLINE mapMaybe #-}
  mapMaybe f = push $ return . f

-- | Never: @'zero' = 'never'@.
instance Reflex t => Plus (Event t) where
  zero = never

-- | Replace each occurrence value of the 'Event' with the value of the
-- 'Behavior' at the time of that occurrence.
tag :: Reflex t => Behavior t b -> Event t a -> Event t b
tag b = pushAlways $ \_ -> sample b

-- | Replace each occurrence value of the 'Event' with the value of the
-- 'Behavior' at that time; if it is 'Just', fire with the contained value; if
-- it is 'Nothing', drop the occurrence.
tagMaybe :: Reflex t => Behavior t (Maybe b) -> Event t a -> Event t b
tagMaybe b = push $ \_ -> sample b

-- | Create a new 'Event' that combines occurrences of supplied 'Event' with the
-- current value of the 'Behavior'.
attach :: Reflex t => Behavior t a -> Event t b -> Event t (a, b)
attach = attachWith (,)

-- | Create a new 'Event' that occurs when the supplied 'Event' occurs by
-- combining it with the current value of the 'Behavior'.
attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
attachWith f = attachWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining each occurrence with the current value of
-- the 'Behavior'. The occurrence is discarded if the combining function returns
-- Nothing
attachWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
attachWithMaybe f b e = flip push e $ \o -> (`f` o) <$> sample b

-- | Create a new 'Event' that occurs on all but the first occurrence of the
-- supplied 'Event'.
tailE :: (Reflex t, MonadHold t m) => Event t a -> m (Event t a)
tailE e = snd <$> headTailE e

-- | Create a tuple of two 'Event's with the first one occurring only the first
-- time the supplied 'Event' occurs and the second occurring on all but the first
-- occurrence.
headTailE :: (Reflex t, MonadHold t m) => Event t a -> m (Event t a, Event t a)
headTailE e = do
  eHead <- headE e
  be <- hold never $ fmap (const e) eHead
  return (eHead, switch be)

-- | Take the streak of occurrences starting at the current time for which the
-- event returns 'True'.
--
-- Starting at the current time, fire all the occurrences of the 'Event' for
-- which the given predicate returns 'True'.  When first 'False' is returned,
-- do not fire, and permanently stop firing, even if 'True' values would have
-- been encountered later.
takeWhileE
  :: forall t m a
  .  (Reflex t, MonadFix m, MonadHold t m)
  => (a -> Bool)
  -> Event t a
  -> m (Event t a)
takeWhileE f = takeWhileJustE $ \v -> guard (f v) $> v

-- | Take the streak of occurrences starting at the current time for which the
-- event returns 'Just b'.
--
-- Starting at the current time, fire all the occurrences of the 'Event' for
-- which the given predicate returns 'Just b'.  When first 'Nothing' is returned,
-- do not fire, and permanently stop firing, even if 'Just b' values would have
-- been encountered later.
takeWhileJustE
  :: forall t m a b
  .  (Reflex t, MonadFix m, MonadHold t m)
  => (a -> Maybe b)
  -> Event t a
  -> m (Event t b)
takeWhileJustE f e = do
  rec let (eBad, eTrue) = fanEither $ ffor e' $ \a -> case f a of
            Nothing -> Left never
            Just b  -> Right b
      eFirstBad <- headE eBad
      e' <- switchHold e eFirstBad
  return eTrue

-- | Drop the streak of occurrences starting at the current time for which the
-- event returns 'True'.
--
-- Starting at the current time, do not fire all the occurrences of the 'Event'
-- for which the given predicate returns 'True'.  When 'False' is first
-- returned, do fire, and permanently continue firing, even if 'True' values
-- would have been encountered later.
dropWhileE
  :: forall t m a
  .  (Reflex t, MonadFix m, MonadHold t m)
  => (a -> Bool)
  -> Event t a
  -> m (Event t a)
dropWhileE f e = snd <$> takeDropWhileJustE (\v -> guard (f v) $> v) e

-- | Both take and drop the streak of occurrences starting at the current time
-- for which the event returns 'Just b'.
--
-- For the left event, starting at the current time, fire all the occurrences
-- of the 'Event' for which the given function returns 'Just b'.  When
-- 'Nothing' is returned, do not fire, and permanently stop firing, even if
-- 'Just b' values would have been encountered later.
--
-- For the right event, do not fire until the first occurrence where the given
-- function returns 'Nothing', and fire that one and all subsequent
-- occurrences. Even if the function would have again returned 'Just b', keep
-- on firing.
takeDropWhileJustE
  :: forall t m a b
  . (Reflex t, MonadFix m, MonadHold t m)
  => (a -> Maybe b)
  -> Event t a
  -> m (Event t b, Event t a)
takeDropWhileJustE f e = do
  rec let (eBad, eGood) = fanEither $ ffor e' $ \a -> case f a of
            Nothing -> Left ()
            Just b  -> Right b
      eFirstBad <- headE eBad
      e' <- switchHold e (never <$ eFirstBad)
  eRest <- switchHoldPromptOnly never (e <$ eFirstBad)
  return (eGood, eRest)

-- | Split the supplied 'Event' into two individual 'Event's occurring at the
-- same time with the respective values from the tuple.
splitE :: Reflex t => Event t (a, b) -> (Event t a, Event t b)
splitE e = (fmap fst e, fmap snd e)

-- | Print the supplied 'String' and the value of the 'Event' on each
-- occurrence. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if the
-- 'Event' is actually used.
traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a
traceEvent s = traceEventWith $ \x -> s <> ": " <> show x

-- | Print the output of the supplied function on each occurrence of the
-- 'Event'. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if the
-- 'Event' is actually used.
traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceEventWith f = push $ \x -> trace (f x) $ return $ Just x

instance (Semigroup a, Reflex t) => Semigroup (Event t a) where
  (<>) = alignWith (mergeThese (<>))
  sconcat = fmap sconcat . mergeList . toList
  stimes n = fmap $ stimes n

instance (Semigroup a, Reflex t) => Monoid (Event t a) where
  mempty = never
  mappend = (<>)
  mconcat = fmap sconcat . mergeList

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs. If multiple occur at the same time they are folded from the left
-- with the given function.
{-# INLINE mergeWith #-}
mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWith = mergeWith' id

{-# INLINE mergeWith' #-}
mergeWith' :: Reflex t => (a -> b) -> (b -> b -> b) -> [Event t a] -> Event t b
mergeWith' f g es = fmap (Prelude.foldl1 g . fmap f)
                  . mergeInt
                  . IntMap.fromDistinctAscList
                  $ zip [0 :: Int ..] es

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs. If multiple occur at the same time the value is the value of the
-- leftmost event.
{-# INLINE leftmost #-}
leftmost :: Reflex t => [Event t a] -> Event t a
leftmost = mergeWith const

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs and has a list of the values of all 'Event's occurring at that
-- time.
mergeList :: Reflex t => [Event t a] -> Event t (NonEmpty a)
mergeList [] = never
mergeList es = mergeWithFoldCheap' id es

unsafeMapIncremental
  :: (Reflex t, Patch p, Patch p')
  => (PatchTarget p -> PatchTarget p')
  -> (p -> p')
  -> Incremental t p
  -> Incremental t p'
unsafeMapIncremental f g a = unsafeBuildIncremental (fmap f $ sample $ currentIncremental a) $ g <$> updatedIncremental a

-- | Create a new 'Event' combining the map of 'Event's into an 'Event' that
-- occurs if at least one of them occurs and has a map of values of all 'Event's
-- occurring at that time.
mergeMap :: (Reflex t, Ord k) => Map k (Event t a) -> Event t (Map k a)
mergeMap = fmap dmapToMap . merge . mapWithFunctorToDMap

-- | Like 'mergeMap' but for 'IntMap'.
mergeIntMap :: Reflex t => IntMap (Event t a) -> Event t (IntMap a)
mergeIntMap = fmap dmapToIntMap . merge . intMapWithFunctorToDMap

-- | Create a merge whose parents can change over time
mergeMapIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Event t a)) -> Event t (Map k a)
mergeMapIncremental = fmap dmapToMap . mergeIncremental . unsafeMapIncremental mapWithFunctorToDMap (const2PatchDMapWith id)

-- | Create a merge whose parents can change over time
mergeIntMapIncremental :: Reflex t => Incremental t (PatchIntMap (Event t a)) -> Event t (IntMap a)
mergeIntMapIncremental = fmap dmapToIntMap . mergeIncremental . unsafeMapIncremental intMapWithFunctorToDMap (const2IntPatchDMapWith id)

-- | Experimental: Create a merge whose parents can change over time; changing the key of an Event is more efficient than with mergeIncremental
mergeMapIncrementalWithMove :: (Reflex t, Ord k) => Incremental t (PatchMapWithMove k (Event t a)) -> Event t (Map k a)
mergeMapIncrementalWithMove = fmap dmapToMap . mergeIncrementalWithMove . unsafeMapIncremental mapWithFunctorToDMap (const2PatchDMapWithMoveWith id)

-- | Split the event into separate events for 'Left' and 'Right' values.
fanEither :: Reflex t => Event t (Either a b) -> (Event t a, Event t b)
fanEither e =
  let justLeft = either Just (const Nothing)
      justRight = either (const Nothing) Just
  in (mapMaybe justLeft e, mapMaybe justRight e)

-- | Split the event into separate events for 'This' and 'That' values,
-- allowing them to fire simultaneously when the input value is 'These'.
fanThese :: Reflex t => Event t (These a b) -> (Event t a, Event t b)
fanThese e =
  let this (This x) = Just x
      this (These x _) = Just x
      this _ = Nothing
      that (That y) = Just y
      that (These _ y) = Just y
      that _ = Nothing
  in (mapMaybe this e, mapMaybe that e)

-- | Split the event into an 'EventSelector' that allows efficient selection of
-- the individual 'Event's.
fanMap :: (Reflex t, Ord k) => Event t (Map k a) -> EventSelector t (Const2 k a)
fanMap = fan . fmap mapToDMap

-- | Switches to the new event whenever it receives one. Only the old event is
-- considered the moment a new one is switched in; the output event will fire at
-- that moment only if the old event does.
--
-- Because the simultaneous firing case is irrelevant, this function imposes
-- laxer "timing requirements" on the overall circuit, avoiding many potential
-- cyclic dependency / metastability failures. It's also more performant. Use
-- this rather than 'switchHoldPromptly' and 'switchHoldPromptOnly' unless you
-- are absolutely sure you need to act on the new event in the coincidental
-- case.
switchHold :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHold ea0 eea = switch <$> hold ea0 eea

-- | Switches to the new event whenever it receives one. Whenever a new event is
-- provided, if it is firing, its value will be the resulting event's value; if
-- it is not firing, but the old one is, the old one's value will be used.
--
-- 'switchHold', by always forwarding the old event the moment it is switched
-- out, avoids many potential cyclic dependency problems / metastability
-- problems. It's also more performant. Use it instead unless you are sure you
-- cannot.
switchHoldPromptly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHoldPromptly ea0 eea = do
  bea <- hold ea0 eea
  let eLag = switch bea
      eCoincidences = coincidence eea
  return $ leftmost [eCoincidences, eLag]

-- | switches to a new event whenever it receives one.  At the moment of
-- switching, the old event will be ignored if it fires, and the new one will be
-- used if it fires; this is the opposite of 'switch', which will use only the
-- old value.
--
-- 'switchHold', by always forwarding the old event the moment it is switched
-- out, avoids many potential cyclic dependency problems / metastability
-- problems. It's also more performant. Use it instead unless you are sure you
-- cannot.
switchHoldPromptOnly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHoldPromptOnly e0 e' = do
  eLag <- switch <$> hold e0 e'
  return $ coincidence $ leftmost [e', eLag <$ eLag]

-- | When the given outer event fires, condense the inner events into the contained patch.  Non-firing inner events will be replaced with deletions.
coincidencePatchMap :: (Reflex t, Ord k) => Event t (PatchMap k (Event t v)) -> Event t (PatchMap k v)
coincidencePatchMap e = fmapCheap PatchMap $ coincidence $ ffor e $ \(PatchMap m) -> mergeMap $ ffor m $ \case
  Nothing -> fmapCheap (const Nothing) e
  Just ev -> leftmost [fmapCheap Just ev, fmapCheap (const Nothing) e]

-- | See 'coincidencePatchMap'
coincidencePatchIntMap :: Reflex t => Event t (PatchIntMap (Event t v)) -> Event t (PatchIntMap v)
coincidencePatchIntMap e = fmapCheap PatchIntMap $ coincidence $ ffor e $ \(PatchIntMap m) -> mergeIntMap $ ffor m $ \case
  Nothing -> fmapCheap (const Nothing) e
  Just ev -> leftmost [fmapCheap Just ev, fmapCheap (const Nothing) e]

-- | See 'coincidencePatchMap'
coincidencePatchMapWithMove :: (Reflex t, Ord k) => Event t (PatchMapWithMove k (Event t v)) -> Event t (PatchMapWithMove k v)
coincidencePatchMapWithMove e = fmapCheap unsafePatchMapWithMove $ coincidence $ ffor e $ \p -> mergeMap $ ffor (unPatchMapWithMove p) $ \ni -> case PatchMapWithMove._nodeInfo_from ni of
  PatchMapWithMove.From_Delete -> fforCheap e $ \_ ->
    ni { PatchMapWithMove._nodeInfo_from = PatchMapWithMove.From_Delete }
  PatchMapWithMove.From_Move k -> fforCheap e $ \_ ->
    ni { PatchMapWithMove._nodeInfo_from = PatchMapWithMove.From_Move k }
  PatchMapWithMove.From_Insert ev -> leftmost
    [ fforCheap ev $ \v ->
        ni { PatchMapWithMove._nodeInfo_from = PatchMapWithMove.From_Insert v }
    , fforCheap e $ \_ ->
        ni { PatchMapWithMove._nodeInfo_from = PatchMapWithMove.From_Delete }
    ]

-- | Given a 'PatchTarget' of events (e.g., a 'Map' with 'Event' values) and an event of 'Patch'es
-- (e.g., a 'PatchMap' with 'Event' values), produce an 'Event' of the 'PatchTarget' type that
-- fires with the patched value.
switchHoldPromptOnlyIncremental
  :: forall t m p pt w
  .  ( Reflex t
     , MonadHold t m
     , Patch (p (Event t w))
     , PatchTarget (p (Event t w)) ~ pt (Event t w)
     , Patch (p w)
     , PatchTarget (p w) ~ pt w
     , Monoid (pt w)
     )
  => (Incremental t (p (Event t w)) -> Event t (pt w))
  -> (Event t (p (Event t w)) -> Event t (p w))
  -> pt (Event t w)
  -> Event t (p (Event t w))
  -> m (Event t (pt w))
switchHoldPromptOnlyIncremental mergePatchIncremental coincidencePatch e0 e' = do
  lag <- mergePatchIncremental <$> holdIncremental e0 e'
  pure $ ffor (align lag (coincidencePatch e')) $ \case
    This old -> old
    That new -> new `applyAlways` mempty
    These old new -> new `applyAlways` old

instance Reflex t => Align (Event t) where
  nil = never
#if MIN_VERSION_these(0, 8, 0)
instance Reflex t => Semialign (Event t) where
#endif
  align = alignEventWithMaybe Just

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_semialign(1,1,0)
instance Reflex t => Zip (Event t) where
#endif
  zip x y = mapMaybe justThese $ align x y
#endif

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_semialign(1,1,0)
instance Reflex t => Unzip (Event t) where
  unzip = splitE
#endif
#endif

-- | Create a new 'Event' that only occurs if the supplied 'Event' occurs and
-- the 'Behavior' is true at the time of occurrence.
gate :: Reflex t => Behavior t Bool -> Event t a -> Event t a
gate = attachWithMaybe $ \allow a -> if allow then Just a else Nothing

-- | Create a new behavior given a starting behavior and switch to the behavior
-- carried by the event when it fires.
switcher :: (Reflex t, MonadHold t m)
        => Behavior t a -> Event t (Behavior t a) -> m (Behavior t a)
switcher b eb = pull . (sample <=< sample) <$> hold b eb

instance (Reflex t, IsString a) => IsString (Dynamic t a) where
  fromString = pure . fromString

-- | Combine two 'Dynamic's.  The result will change whenever either (or both)
-- input 'Dynamic' changes.  Equivalent to @zipDynWith (,)@.
zipDyn :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t (a, b)
zipDyn = zipDynWith (,)

-- | Combine two 'Dynamic's with a combining function.  The result will change
-- whenever either (or both) input 'Dynamic' changes.
-- More efficient than 'liftA2'.
zipDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
zipDynWith f da db =
  let eab = align (updated da) (updated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This a -> do
            b <- sample $ current db
            return (a, b)
          That b -> do
            a <- sample $ current da
            return (a, b)
          These a b -> return (a, b)
        return $ Just $ f a b
  in unsafeBuildDynamic (f <$> sample (current da) <*> sample (current db)) ec

instance (Reflex t, Semigroup a) => Semigroup (Dynamic t a) where
  (<>) = zipDynWith (<>)
  stimes n = fmap $ stimes n

instance (Reflex t, Monoid a) => Monoid (Dynamic t a) where
  mconcat = distributeListOverDynWith mconcat
  mempty = constDyn mempty

-- | This function converts a 'DMap' whose elements are 'Dynamic's into a
-- 'Dynamic' 'DMap'.  Its implementation is more efficient than doing the same
-- through the use of multiple uses of 'zipDynWith' or 'Applicative' operators.
distributeDMapOverDynPure :: forall t k. (Reflex t, GCompare k) => DMap k (Dynamic t) -> Dynamic t (DMap k Identity)
distributeDMapOverDynPure = distributeDMapOverDynPureG coerceDynamic

-- | This function converts a 'DMap' whose elements are 'Dynamic's into a
-- 'Dynamic' 'DMap'.  Its implementation is more efficient than doing the same
-- through the use of multiple uses of 'zipDynWith' or 'Applicative' operators.
distributeDMapOverDynPureG
  :: forall t k q v. (Reflex t, GCompare k)
  => (forall a. q a -> Dynamic t (v a))
  -> DMap k q -> Dynamic t (DMap k v)
distributeDMapOverDynPureG nt dm = case DMap.toList dm of
  [] -> constDyn DMap.empty
  [k :=> v] -> DMap.singleton k <$> nt v
  _ ->
    let getInitial = DMap.traverseWithKey (\_ -> sample . current . nt) dm
        edmPre = mergeG getCompose $ DMap.map (Compose . updated . nt) dm
        result = unsafeBuildDynamic getInitial $ flip pushAlways edmPre $ \news -> do
          olds <- sample $ current result
          return $ DMap.unionWithKey (\_ _ new -> new) olds news
    in result

-- | Convert a list of 'Dynamic's into a 'Dynamic' list.
distributeListOverDyn :: Reflex t => [Dynamic t a] -> Dynamic t [a]
distributeListOverDyn = distributeListOverDynWith id

-- | Create a new 'Dynamic' by applying a combining function to a list of 'Dynamic's
distributeListOverDynWith :: Reflex t => ([a] -> b) -> [Dynamic t a] -> Dynamic t b
distributeListOverDynWith f =
  fmap (f . map fromDSum . DMap.toAscList) .
  distributeDMapOverDynPure .
  DMap.fromDistinctAscList .
  zipWith toDSum [0..]
  where
    toDSum :: Int -> Dynamic t a -> DSum (Const2 Int a) (Dynamic t)
    toDSum k v = Const2 k :=> v
    fromDSum :: DSum (Const2 Int a) Identity -> a
    fromDSum (Const2 _ :=> Identity v) = v

-- | Create a new 'Event' that occurs when the first supplied 'Event' occurs
-- unless the second supplied 'Event' occurs simultaneously.
difference :: Reflex t => Event t a -> Event t b -> Event t a
difference = alignEventWithMaybe $ \case
  This a -> Just a
  _ -> Nothing

-- | Zips two values by taking the union of their shapes and combining with the provided function.
-- 'Nothing' values are dropped.
alignEventWithMaybe :: Reflex t => (These a b -> Maybe c) -> Event t a -> Event t b -> Event t c
alignEventWithMaybe f ea eb = mapMaybe (f <=< dmapToThese) $
  merge $ DMap.fromList [LeftTag :=> ea, RightTag :=> eb]

-- | Produces an 'Event' that fires only when the input event fires with a 'DSum' key that
-- matches the provided key.
filterEventKey
  :: forall t m k v a.
     ( Reflex t
     , MonadFix m
     , MonadHold t m
     , GEq k
     )
  => k a
  -> Event t (DSum k v)
  -> m (Event t (v a))
filterEventKey k kv' = do
  let f :: DSum k v -> Maybe (v a)
      f (newK :=> newV) = case newK `geq` k of
        Just Refl -> Just newV
        Nothing -> Nothing
  takeWhileJustE f kv'

-- | "Factor" the input 'DSum' 'Event' to produce an 'Event' which
-- fires when the 'DSum' key changes and contains both the value of the
-- 'DSum' at switchover and an 'Event' of values produced by subsequent
-- firings of the input 'Event' that do not change the 'DSum' key.
factorEvent
  :: forall t m k v a.
     ( Reflex t
     , MonadFix m
     , MonadHold t m
     , GEq k
     )
  => k a
  -> Event t (DSum k v)
  -> m (Event t (v a), Event t (DSum k (Product v (Compose (Event t) v))))
factorEvent k0 kv' = do
  key :: Behavior t (Some k) <- hold (Some k0) $ fmapCheap (\(k :=> _) -> Some k) kv'
  let update = flip push kv' $ \(newKey :=> newVal) -> sample key >>= \case
        Some oldKey -> case newKey `geq` oldKey of
          Just Refl -> return Nothing
          Nothing -> do
            newInner <- filterEventKey newKey kv'
            return $ Just $ newKey :=> Pair newVal (Compose newInner)
  eInitial <- filterEventKey k0 kv'
  return (eInitial, update)

--------------------------------------------------------------------------------
-- Accumulator
--------------------------------------------------------------------------------

-- | An 'Accumulator' type can be built by accumulating occurrences of an
-- 'Event'.
#if __GLASGOW_HASKELL__ < 802
{-# WARNING accum "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumM "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumMaybe "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumMaybeM "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
#endif
class Reflex t => Accumulator t f | f -> t where
  accum :: (MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (f a)
  accum f = accumMaybe $ \v o -> Just $ f v o
  accumM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t a) -> a -> Event t b -> m (f a)
  accumM f = accumMaybeM $ \v o -> Just <$> f v o
  accumMaybe :: (MonadHold t m, MonadFix m) => (a -> b -> Maybe a) -> a -> Event t b -> m (f a)
  accumMaybe f = accumMaybeM $ \v o -> return $ f v o
  accumMaybeM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a)) -> a -> Event t b -> m (f a)
  mapAccum :: (MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccum f = mapAccumMaybe $ \v o -> bimap Just Just $ f v o
  mapAccumM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccumM f = mapAccumMaybeM $ \v o -> bimap Just Just <$> f v o
  mapAccumMaybe :: (MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccumMaybe f = mapAccumMaybeM $ \v o -> return $ f v o
  mapAccumMaybeM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event'
-- with the provided function. See 'foldDyn'.
accumDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> a)
  -> a
  -> Event t b
  -> m (Dynamic t a)
accumDyn f = accumMaybeDyn $ \v o -> Just $ f v o

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event'
-- with the provided 'PushM' action.
accumMDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t a)
  -> a
  -> Event t b
  -> m (Dynamic t a)
accumMDyn f = accumMaybeMDyn $ \v o -> Just <$> f v o

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event'
-- with the provided function, discarding 'Nothing' results.
accumMaybeDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> Maybe a)
  -> a
  -> Event t b
  -> m (Dynamic t a)
accumMaybeDyn f = accumMaybeMDyn $ \v o -> return $ f v o

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event'
-- with the provided 'PushM' action, discarding 'Nothing' results.
accumMaybeMDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t (Maybe a))
  -> a
  -> Event t b
  -> m (Dynamic t a)
accumMaybeMDyn f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ current d'
            f v o
      d' <- holdDyn z e'
  return d'

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event'
-- with a function that both accumulates and produces a value to fire
-- as an 'Event'. Returns both the accumulated value and an 'Event'.
mapAccumDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> (a, c))
  -> a
  -> Event t b
  -> m (Dynamic t a, Event t c)
mapAccumDyn f = mapAccumMaybeDyn $ \v o -> bimap Just Just $ f v o

-- | Similar to 'mapAccumDyn' except that the combining function is a
-- 'PushM' action.
mapAccumMDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t (a, c))
  -> a
  -> Event t b
  -> m (Dynamic t a, Event t c)
mapAccumMDyn f = mapAccumMaybeMDyn $ \v o -> bimap Just Just <$> f v o

-- | Accumulate a 'Dynamic' by folding occurrences of an 'Event' with
-- a function that both optionally accumulates and optionally produces
-- a value to fire as a separate output 'Event'.
-- Note that because 'Nothing's are discarded in both cases, the output
-- 'Event' may fire even though the output 'Dynamic' has not changed, and
-- the output 'Dynamic' may update even when the output 'Event' is not firing.
mapAccumMaybeDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> (Maybe a, Maybe c))
  -> a
  -> Event t b
  -> m (Dynamic t a, Event t c)
mapAccumMaybeDyn f = mapAccumMaybeMDyn $ \v o -> return $ f v o

-- | Like 'mapAccumMaybeDyn' except that the combining function is a
-- 'PushM' action.
mapAccumMaybeMDyn
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t (Maybe a, Maybe c))
  -> a
  -> Event t b
  -> m (Dynamic t a, Event t c)
mapAccumMaybeMDyn f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ current d'
            result <- f v o
            return $ case result of
              (Nothing, Nothing) -> Nothing
              _ -> Just result
      d' <- holdDyn z $ mapMaybe fst e'
  return (d', mapMaybe snd e')

-- | Accumulate a 'Behavior' by folding occurrences of an 'Event'
-- with the provided function.
{-# INLINE accumB #-}
accumB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> a)
  -> a
  -> Event t b
  -> m (Behavior t a)
accumB f = accumMaybeB $ \v o -> Just $ f v o

-- | Like 'accumB' except that the combining function is a 'PushM' action.
{-# INLINE accumMB #-}
accumMB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t a)
  -> a
  -> Event t b
  -> m (Behavior t a)
accumMB f = accumMaybeMB $ \v o -> Just <$> f v o

-- | Accumulate a 'Behavior' by folding occurrences of an 'Event'
-- with the provided function, discarding 'Nothing' results.
{-# INLINE accumMaybeB #-}
accumMaybeB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> Maybe a)
  -> a
  -> Event t b
  -> m (Behavior t a)
accumMaybeB f = accumMaybeMB $ \v o -> return $ f v o

-- | Like 'accumMaybeB' except that the combining function is a 'PushM' action.
{-# INLINE accumMaybeMB #-}
accumMaybeMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a)) -> a -> Event t b -> m (Behavior t a)
accumMaybeMB f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample d'
            f v o
      d' <- hold z e'
  return d'

-- | Accumulate a 'Behavior' by folding occurrences of an 'Event'
-- with a function that both accumulates and produces a value to fire
-- as an 'Event'. Returns both the accumulated value and an 'Event'.
{-# INLINE mapAccumB #-}
mapAccumB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> (a, c))
  -> a
  -> Event t b
  -> m (Behavior t a, Event t c)
mapAccumB f = mapAccumMaybeB $ \v o -> bimap Just Just $ f v o

-- | Like 'mapAccumB' except that the combining function is a 'PushM' action.
{-# INLINE mapAccumMB #-}
mapAccumMB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> PushM t (a, c))
  -> a
  -> Event t b
  -> m (Behavior t a, Event t c)
mapAccumMB f = mapAccumMaybeMB $ \v o -> bimap Just Just <$> f v o

-- | Accumulate a 'Behavior' by folding occurrences of an 'Event' with
-- a function that both optionally accumulates and optionally produces
-- a value to fire as a separate output 'Event'. 'Nothing's are discarded.
{-# INLINE mapAccumMaybeB #-}
mapAccumMaybeB
  :: (Reflex t, MonadHold t m, MonadFix m)
  => (a -> b -> (Maybe a, Maybe c))
  -> a
  -> Event t b
  -> m (Behavior t a, Event t c)
mapAccumMaybeB f = mapAccumMaybeMB $ \v o -> return $ f v o

-- | Like 'mapAccumMaybeB' except that the combining function is a 'PushM' action.
{-# INLINE mapAccumMaybeMB #-}
mapAccumMaybeMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Behavior t a, Event t c)
mapAccumMaybeMB f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample d'
            result <- f v o
            return $ case result of
              (Nothing, Nothing) -> Nothing
              _ -> Just result
      d' <- hold z $ mapMaybe fst e'
  return (d', mapMaybe snd e')

-- | Accumulate occurrences of an 'Event', producing an output occurrence each
-- time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccum_ #-}
mapAccum_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum_ f z e = do
  (_, result) <- mapAccumB f z e
  return result

-- | Accumulate occurrences of an 'Event', possibly producing an output
-- occurrence each time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccumMaybe_ #-}
mapAccumMaybe_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe_ f z e = do
  (_, result) <- mapAccumMaybeB f z e
  return result

-- | Accumulate occurrences of an 'Event', using a 'PushM' action and producing
-- an output occurrence each time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccumM_ #-}
mapAccumM_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (Event t c)
mapAccumM_ f z e = do
  (_, result) <- mapAccumMB f z e
  return result

-- | Accumulate occurrences of an 'Event', using a 'PushM' action and possibly
-- producing an output occurrence each time.  Discard the underlying
-- 'Accumulator'.
{-# INLINE mapAccumMaybeM_ #-}
mapAccumMaybeM_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybeM_ f z e = do
  (_, result) <- mapAccumMaybeMB f z e
  return result

instance Reflex t => Accumulator t (Dynamic t) where
  accumMaybeM = accumMaybeMDyn
  mapAccumMaybeM = mapAccumMaybeMDyn

instance Reflex t => Accumulator t (Behavior t) where
  accumMaybeM = accumMaybeMB
  mapAccumMaybeM = mapAccumMaybeMB

instance Reflex t => Accumulator t (Event t) where
  accumMaybeM f z e = updated <$> accumMaybeM f z e
  mapAccumMaybeM f z e = first updated <$> mapAccumMaybeM f z e

-- | Create a new 'Event' by combining each occurrence with the next value of the
-- list using the supplied function. If the list runs out of items, all
-- subsequent 'Event' occurrences will be ignored.
zipListWithEvent :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> c) -> [a] -> Event t b -> m (Event t c)
zipListWithEvent f l e = do
  let f' a b = case a of
        h:t -> (Just t, Just $ f h b)
        _ -> (Nothing, Nothing) --TODO: Unsubscribe the event?
  mapAccumMaybe_ f' l e

-- | Assign a number to each occurrence of the given 'Event', starting from 0
{-# INLINE numberOccurrences #-}
numberOccurrences :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Event t (b, a))
numberOccurrences = numberOccurrencesFrom 0

-- | Assign a number to each occurrence of the given 'Event'
{-# INLINE numberOccurrencesFrom #-}
numberOccurrencesFrom :: (Reflex t, MonadHold t m, MonadFix m, Num b) => b -> Event t a -> m (Event t (b, a))
numberOccurrencesFrom = mapAccum_ (\n a -> let !next = n + 1 in (next, (n, a)))

-- | Assign a number to each occurrence of the given 'Event'; discard the occurrences' values
{-# INLINE numberOccurrencesFrom_ #-}
numberOccurrencesFrom_ :: (Reflex t, MonadHold t m, MonadFix m, Num b) => b -> Event t a -> m (Event t b)
numberOccurrencesFrom_ = mapAccum_ (\n _ -> let !next = n + 1 in (next, n))

-- | This is used to sample the value of a 'Behavior' using an 'Event'.
--
-- The '<@>' operator is intended to be used in conjunction with
-- the 'Applicative' instance for 'Behavior'.
--
-- This is useful when we want to combine the values of one 'Event' and
-- the value of several 'Behavior's at the time the 'Event' is firing.
--
-- If we have:
--
-- > f  :: a -> b -> c -> d
-- > b1 :: Behavior t a
-- > b2 :: Behavior t b
-- > e  :: Event t c
--
-- then we can do:
--
-- > f <$> b1 <*> b2 <@> e :: Event t d
--
-- in order to apply the function 'f' to the relevant values.
--
-- The alternative would be something like:
--
-- > attachWith (\(x1, x2) y -> f x1 x2 y) ((,) <$> b1 <*> b2) e :: Event t d
--
-- or a variation involing a custom data type to hold the combination of
-- 'Behavior's even when that combination might only ever be used by 'f'.
--
-- A more suggestive example might be:
--
-- > handleMouse <$> bPlayerState <*> bMousePosition <@> eMouseClick :: Event t (GameState -> GameState)
--
(<@>) :: Reflex t => Behavior t (a -> b) -> Event t a -> Event t b
(<@>) b = push $ \x -> do
  f <- sample b
  return . Just . f $ x
infixl 4 <@>

-- | An version of '<@>' that does not use the value of the 'Event'.
--
-- Alternatively, it is 'tag' in operator form.
--
-- This is useful when we want to combine the values of several
-- 'Behavior's at particular points in time using an 'Applicative'
-- style syntax.
--
-- If we have:
--
-- > g  :: a -> b -> d
-- > b1 :: Behavior t a
-- > b2 :: Behavior t b
-- > e  :: Event t c
--
-- where 'e' is firing at the points in time of interest.
--
-- Then we can use '<@':
--
-- > g <$> b1 <*> b2 <@  e :: Event t d
--
-- to combine the values of 'b1' and 'b2' at each of those points of time,
-- with the function 'g' being used to combine the values.
--
-- This is the same as '<@>' except that the 'Event' is being used only
-- to act as a trigger.
(<@) :: (Reflex t) => Behavior t b -> Event t a -> Event t b
(<@) = tag
infixl 4 <@

------------------
-- Cheap Functions
------------------

-- | A "cheap" version of 'pushAlways'. See the performance note on 'pushCheap'.
{-# INLINE pushAlwaysCheap #-}
pushAlwaysCheap :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlwaysCheap f = pushCheap (fmap Just . f)

-- | A "cheap" version of 'mapMaybe'. See the performance note on 'pushCheap'.
{-# INLINE mapMaybeCheap #-}
mapMaybeCheap :: Reflex t => (a -> Maybe b) -> Event t a -> Event t b
mapMaybeCheap f = pushCheap $ return . f

-- | An alias for 'mapMaybeCheap'
{-# INLINE fmapMaybeCheap #-}
fmapMaybeCheap :: Reflex t => (a -> Maybe b) -> Event t a -> Event t b
fmapMaybeCheap = mapMaybeCheap


-- | A "cheap" version of 'fforMaybe'. See the performance note on 'pushCheap'.
{-# INLINE fforMaybeCheap #-}
fforMaybeCheap :: Reflex t => Event t a -> (a -> Maybe b) -> Event t b
fforMaybeCheap = flip mapMaybeCheap

-- | A "cheap" version of 'ffor'. See the performance note on 'pushCheap'.
{-# INLINE fforCheap #-}
fforCheap :: Reflex t => Event t a -> (a -> b) -> Event t b
fforCheap = flip fmapCheap

-- | A "cheap" version of 'fmap'. See the performance note on 'pushCheap'.
{-# INLINE fmapCheap #-}
fmapCheap :: Reflex t => (a -> b) -> Event t a -> Event t b
fmapCheap f = pushCheap $ return . Just . f

-- | A "cheap" version of 'tag'. See the performance note on 'pushCheap'.
{-# INLINE tagCheap #-}
tagCheap :: Reflex t => Behavior t b -> Event t a -> Event t b
tagCheap b = pushAlwaysCheap $ \_ -> sample b

-- | Merge a collection of events; the resulting 'Event' will only occur if at
-- least one input event is occurring, and will contain all of the input keys
-- that are occurring simultaneously
merge :: (Reflex t, GCompare k) => DMap k (Event t) -> Event t (DMap k Identity)
merge = mergeG coerceEvent
{-# INLINE merge #-}

-- | Create a merge whose parents can change over time
mergeIncremental :: (Reflex t, GCompare k)
  => Incremental t (PatchDMap k (Event t)) -> Event t (DMap k Identity)
mergeIncremental = mergeIncrementalG coerceEvent

-- | Experimental: Create a merge whose parents can change over time; changing the key of an Event is more efficient than with mergeIncremental
mergeIncrementalWithMove :: (Reflex t, GCompare k)
  => Incremental t (PatchDMapWithMove k (Event t)) -> Event t (DMap k Identity)
mergeIncrementalWithMove = mergeIncrementalWithMoveG coerceEvent

-- | A "cheap" version of 'mergeWithCheap'. See the performance note on 'pushCheap'.
{-# INLINE mergeWithCheap #-}
mergeWithCheap :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWithCheap = mergeWithCheap' id

-- | A "cheap" version of 'mergeWithCheap''. See the performance note on 'pushCheap'.
{-# INLINE mergeWithCheap' #-}
mergeWithCheap' :: Reflex t => (a -> b) -> (b -> b -> b) -> [Event t a] -> Event t b
mergeWithCheap' f g = mergeWithFoldCheap' $ foldl1 g . fmap f

-- | A "cheap" version of 'mergeWithFoldCheap''. See the performance note on 'pushCheap'.
{-# INLINE mergeWithFoldCheap' #-}
mergeWithFoldCheap' :: Reflex t => (NonEmpty a -> b) -> [Event t a] -> Event t b
mergeWithFoldCheap' f es =
  fmapCheap (f . (\(h : t) -> h :| t) . IntMap.elems)
  . mergeInt
  . IntMap.fromDistinctAscList
  $ zip [0 :: Int ..] es

--------------------------------------------------------------------------------
-- Deprecated functions
--------------------------------------------------------------------------------

{-# DEPRECATED switchPromptly "Use 'switchHoldPromptly' instead. The 'switchHold*' naming convention was chosen because those functions are more closely related to each other than they are to 'switch'. " #-}
-- | See 'switchHoldPromptly'
switchPromptly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchPromptly = switchHoldPromptly
{-# DEPRECATED switchPromptOnly "Use 'switchHoldPromptOnly' instead. The 'switchHold*' naming convention was chosen because those functions are more closely related to each other than they are to 'switch'. " #-}
-- | See 'switchHoldPromptOnly'
switchPromptOnly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchPromptOnly = switchHoldPromptOnly
