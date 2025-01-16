{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:
--   Data.AppendMap
-- Description:
--   Instances and convenience functions for 'Data.Map.Monoidal'. We use
--   monoidal-containers to take advantage of its better monoid instance.
--   'Data.Map' has @mappend = union@, which is left-biased.  'MonoidalMap'
--   has @mappend = unionWith mappend@ instead.
module Data.AppendMap
  ( module Data.AppendMap
  , module Data.Map.Monoidal
  ) where

import Prelude hiding (null)

import Data.Coerce
import Data.Default
import Data.Map (Map)
import qualified Data.Map.Internal.Debug as Map (showTree, showTreeWith)

import Data.Map.Monoidal


{-# DEPRECATED AppendMap "Use 'MonoidalMap' instead" #-}
-- | AppendMap is a synonym for 'Data.Map.Monoidal.MonoidalMap'
type AppendMap = MonoidalMap

{-# DEPRECATED _unAppendMap "Use 'getMonoidalMap' instead" #-}
-- | A synonym for 'getMonoidalMap'
_unAppendMap :: MonoidalMap k v -> Map k v
_unAppendMap = getMonoidalMap

-- | Pattern synonym for 'MonoidalMap'
pattern AppendMap :: Map k v -> MonoidalMap k v
pattern AppendMap m = MonoidalMap m

-- | Deletes a key, returning 'Nothing' if the result is empty.
nonEmptyDelete :: Ord k => k -> MonoidalMap k a -> Maybe (MonoidalMap k a)
nonEmptyDelete k vs =
  let deleted = delete k vs
  in if null deleted
       then Nothing
       else Just deleted

-- | Like 'mapMaybe' but indicates whether the resulting container is empty
mapMaybeNoNull :: (a -> Maybe b)
               -> MonoidalMap token a
               -> Maybe (MonoidalMap token b)
mapMaybeNoNull f as =
  let bs = mapMaybe f as
  in if null bs
       then Nothing
       else Just bs

-- TODO: Move instances to `Data.Patch`
-- | Displays a 'MonoidalMap' as a tree. See 'Data.Map.Lazy.showTree' for details.
showTree :: forall k a. (Show k, Show a) => MonoidalMap k a -> String
showTree = coerce (Map.showTree :: Map k a -> String)

-- | Displays a 'MonoidalMap' as a tree, using the supplied function to convert nodes to string.
showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> MonoidalMap k a -> String
showTreeWith = coerce (Map.showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String)

instance Default (MonoidalMap k a) where
  def = empty
