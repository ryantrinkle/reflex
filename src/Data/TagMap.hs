-- | This module provides 'TagMap', a version of 'IntMap' for
-- GADT keys whose constructors can be counted by 'Int'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Data.TagMap
  (
    TagMap
  ) where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Applicative (liftA2)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Bits
import Data.Coerce
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy
import qualified Data.Semigroup as S
import Data.Some (Some(Some))
import Data.Type.Equality
import Data.Unique.Tag

import GHC.Exts (Any, dataToTag#)
import Unsafe.Coerce

--TODO: Make this module type-safe

newtype TagMap (k :: x -> *) (v :: x -> *) = TagMap (IntMap (f Any))
type role TagMap representational representational

class IsTag k where
  -- For traversing
  unsafeToKeyValue :: Int -> v Any -> KeyValue k v
  -- For inspecting just keys. Do we really want to use Some here,
  -- or are we better off (for performance) using a more legitimate
  -- Some-like type? I don't think we actually use this yet, so it
  -- may not matter much.
  toKey :: Int -> Some k
  -- For inserting and looking up
  fromKey :: k a -> Int

toVany :: v a -> v Any
toVany = unsafeCoerce

empty :: TagMap f
empty = TagMap IntMap.empty

singleton :: forall f a. IsTag k => k a -> f a -> TagMap k f
singleton k v = TagMap $ IntMap.singleton (fromKey k) $ toVany v

insert :: IsTag k => k a -> v a -> TagMap k v -> TagMap k v
insert k v (TagMap m) = TagMap $ IntMap.insert (fromKey k) (toVany v) m

lookup :: IsTag k => k a -> TagMap k v -> Maybe (v a)
lookup k (TagMap m) = unsafeCoerce <$> IntMap.lookup (fromKey k) m

foldrWithKey :: forall k f r. IsTag k => (forall a. k a -> f a -> r -> r) -> r -> TagMap k f -> r
foldrWithKey f b = \(TagMap m) -> IntMap.foldrWithKey go b m
  where
    go :: Int -> f Any -> r -> r
    go ki fany r
      | KeyValue k v <- unsafeToKeyValue ki fany
      = f k v r

data KeyValue k v = forall a. KeyValue !(k a) (v a)

toList :: forall k f. IsTag k => TagMap k f -> [DSum k f]
toList = foldrWithKey go []
  where
    go k v r = (k :=> v) : r

traverseWithKey
  :: forall k v f g. (IsTag k, Applicative f)
  => (forall a. k a -> v a -> f (g a)) -> TagMap k v -> f (TagMap k g)
traverseWithKey f (TagMap m) = TagMap <$> IntMap.traverseWithKey g m
  where
    g :: Int -> v Any -> f (g Any)
    g ki vi
      | KeyValue k v <- unsafeToKeyValue ki vi
      = toVany <$> f k v
