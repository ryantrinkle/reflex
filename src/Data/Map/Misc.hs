{-# LANGUAGE LambdaCase #-}

-- | Additional functions for manipulating 'Map's.
module Data.Map.Misc
  (
  -- * Working with Maps
    diffMapNoEq
  , diffMap
  , applyMap
  , mapPartitionEithers
  , applyMapKeysSet
  ) where

import Data.Align
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These

-- |Produce a @'Map' k (Maybe v)@ by comparing two @'Map' k v@s, @old@ and @new@ respectively. @Just@ represents an association present in @new@ and @Nothing@
-- represents an association only present in @old@ but no longer present in @new@.
--
-- Similar to 'diffMap' but doesn't require 'Eq' on the values, thus can't tell if a value has changed or not.
diffMapNoEq :: (Ord k) => Map k v -> Map k v -> Map k (Maybe v)
diffMapNoEq olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These _ new -> Just $ Just new
  That new -> Just $ Just new

-- |Produce a @'Map' k (Maybe v)@ by comparing two @'Map' k v@s, @old@ and @new respectively. @Just@ represents an association present in @new@ and either not
-- present in @old@ or where the value has changed. @Nothing@ represents an association only present in @old@ but no longer present in @new@.
--
-- See also 'diffMapNoEq' for a similar but weaker version which does not require 'Eq' on the values but thus can't indicated a value not changing between
-- @old@ and @new@ with @Nothing@.
diffMap :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (Maybe v)
diffMap olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These old new
    | old == new -> Nothing
    | otherwise -> Just $ Just new
  That new -> Just $ Just new

-- |Given a @'Map' k (Maybe v)@ representing keys to insert/update (@Just@) or delete (@Nothing@), produce a new map from the given input @'Map' k v@.
--
-- See also 'Data.Patch.Map' and 'Data.Patch.MapWithMove'.
applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions, insertions) = Map.mapEither maybeToEither patch
        maybeToEither = \case
          Nothing -> Left ()
          Just r -> Right r

-- |Split a @'Map' k (Either a b)@ into @Map k a@ and @Map k b@, equivalent to @'Map.mapEither' id@
{-# DEPRECATED mapPartitionEithers "Use 'mapEither' instead" #-}
mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers = Map.mapEither id

-- |Given a @'Map' k (Maybe v)@ representing keys to insert/update (@Just@) or delete (@Nothing@), produce a new @'Set' k@ from the given input set.
--
-- Equivalent to:
--
-- @
--     applyMapKeysSet patch ('Map.keysSet' m) == 'Map.keysSet' ('applyMap' patch m)
-- @
--
-- but avoids the intervening @Map@ and needs no values.
applyMapKeysSet :: Ord k => Map k (Maybe v) -> Set k -> Set k
applyMapKeysSet patch old = Map.keysSet insertions `Set.union` (old `Set.difference` Map.keysSet deletions)
  where (insertions, deletions) = Map.partition isJust patch

