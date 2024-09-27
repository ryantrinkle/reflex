{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:
--   Reflex.Query.Class
-- Description:
--   A class that ties together queries to some data source and their results,
--   providing methods for requesting data from the source and accumulating
--   streamed results.
module Reflex.Query.Class
  ( Query (..)
  , QueryMorphism (..)
  , SelectedCount (..)
  , combineSelectedCounts

  , MonadQuery (..)
  , tellQueryDyn
  , queryDyn
  , subQuery
  , mapQuery
  , mapQueryResult
  ) where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Monad.Reader
import Data.Bits
import Data.Data
import Data.Ix
import Data.Kind (Type)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import Data.Semigroup.Commutative
import Data.Void
import Data.Monoid hiding ((<>))
import Foreign.Storable

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative
import Data.Semigroup (Semigroup(..))
#endif

import Reflex.Class

-- | A 'Query' can be thought of as a declaration of interest in some set of data.
-- A 'QueryResult' is the set of data associated with that interest set.
-- The @crop@ function provides a way to determine what part of a given 'QueryResult'
-- is relevant to a given 'Query'.
class (Monoid (QueryResult a), Semigroup (QueryResult a)) => Query a where
  type QueryResult a :: Type
  crop :: a -> QueryResult a -> QueryResult a

instance (Ord k, Query v) => Query (MonoidalMap k v) where
  type QueryResult (MonoidalMap k v) = MonoidalMap k (QueryResult v)
  crop q r = MonoidalMap.intersectionWith (flip crop) r q

-- | the result of two queries is both results.
instance (Query a, Query b) => Query (a, b) where
  type QueryResult (a, b) = (QueryResult a, QueryResult b)
  crop (x, x') (y, y') = (crop x y, crop x' y')

-- | Trivial queries have trivial results.
instance Query () where
  type QueryResult () = ()
  crop _ _ = ()

-- | The result of an absurd query is trivial; If you can ask the question, the
-- answer cannot tell you anything you didn't already know.
--
-- 'QueryResult Void = @Void@' seems like it would also work, but that has
-- problems of robustness. In some applications, an unasked question can still
-- be answered, so it is important that the result is inhabited even when the
-- question isn't. Applications that wish to prevent this can mandate that the
-- query result be paired with the query: then the whole response will be
-- uninhabited as desired.
instance Query Void where
  type QueryResult Void = ()
  crop = absurd

#if MIN_VERSION_base(4,12,0)
-- | We can lift queries into monoidal containers.
-- But beware of Applicatives whose monoid is different from (pure mempty, liftA2 mappend)
instance (Query q, Applicative f) => Query (Ap f q) where
  type QueryResult (Ap f q) = Ap f (QueryResult q)
  crop = liftA2 crop
#endif

-- | QueryMorphism's must be group homomorphisms when acting on the query type
-- and compatible with the query relationship when acting on the query result.
data QueryMorphism q q' = QueryMorphism
  { _queryMorphism_mapQuery :: q -> q'
  , _queryMorphism_mapQueryResult :: QueryResult q' -> QueryResult q
  }

instance Category QueryMorphism where
  id = QueryMorphism id id
  qm . qm' = QueryMorphism
    { _queryMorphism_mapQuery = mapQuery qm . mapQuery qm'
    , _queryMorphism_mapQueryResult = mapQueryResult qm' . mapQueryResult qm
    }

-- | Apply a 'QueryMorphism' to a 'Query'
mapQuery :: QueryMorphism q q' -> q -> q'
mapQuery = _queryMorphism_mapQuery

-- | Map a 'QueryMorphism' to a 'QueryResult'
mapQueryResult :: QueryMorphism q q' -> QueryResult q' -> QueryResult q
mapQueryResult = _queryMorphism_mapQueryResult

-- | This type can be used to track of the frequency of interest in a given 'Query'. See note on
-- 'combineSelectedCounts'
newtype SelectedCount = SelectedCount { unSelectedCount :: Int }
  deriving (Eq, Ord, Show, Read, Integral, Num, Bounded, Enum, Real, Ix, Bits, FiniteBits, Storable, Data)

instance Semigroup SelectedCount where
  SelectedCount a <> SelectedCount b = SelectedCount (a + b)

instance Monoid SelectedCount where
  mempty = SelectedCount 0
  mappend = (<>)

instance Group SelectedCount where
  negateG (SelectedCount a) = SelectedCount (negate a)

instance Commutative SelectedCount

-- | The Semigroup\/Monoid\/Group instances for a Query containing 'SelectedCount's should use
-- this function which returns Nothing if the result is 0. This allows the pruning of leaves
-- of the 'Query' that are no longer wanted.
combineSelectedCounts :: SelectedCount -> SelectedCount -> Maybe SelectedCount
combineSelectedCounts (SelectedCount i) (SelectedCount j) = if i == negate j then Nothing else Just $ SelectedCount (i + j)

-- | A class that allows sending of 'Query's and retrieval of 'QueryResult's. See 'queryDyn' for a commonly
-- used interface.
class (Group q, Commutative q, Query q, Monad m) => MonadQuery t q m | m -> q t where
  tellQueryIncremental :: Incremental t (AdditivePatch q) -> m ()
  askQueryResult :: m (Dynamic t (QueryResult q))
  queryIncremental :: Incremental t (AdditivePatch q) -> m (Dynamic t (QueryResult q))

instance MonadQuery t q m => MonadQuery t q (ReaderT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

-- | Produce and send an 'Incremental' 'Query' from a 'Dynamic' 'Query'.
tellQueryDyn :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m ()
tellQueryDyn d = tellQueryIncremental $ unsafeBuildIncremental (sample (current d)) $ attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d)

-- | Retrieve 'Dynamic'ally updating 'QueryResult's for a 'Dynamic'ally updating 'Query'.
queryDyn :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m (Dynamic t (QueryResult q))
queryDyn q = do
  tellQueryDyn q
  zipDynWith crop q <$> askQueryResult

-- | Use a query morphism to operate on a smaller version of a query.
subQuery :: (Reflex t, MonadQuery t q2 m) => QueryMorphism q1 q2 -> Dynamic t q1 -> m (Dynamic t (QueryResult q1))
subQuery (QueryMorphism f g) x = fmap g <$> queryDyn (fmap f x)
