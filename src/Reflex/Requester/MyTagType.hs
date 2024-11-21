{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.MyTagType
  (
    MyTagType (..)
  , Single (..)
  , Multi (..)
  , Multi2 (..)
  , MyTagTypeOffset (..)
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

import GHC.Exts (Any, dataToTag#, I#)
import Unsafe.Coerce

data MyTagType :: * -> * where
  MyTagType_Single :: MyTagType (Single a)
  MyTagType_Multi :: MyTagType Multi
  MyTagType_Multi2 :: MyTagType (Multi2 k)
  MyTagType_Multi3 :: MyTagType Multi3

deriving instance Eq (MyTagType a)
deriving instance Ord (MyTagType a)
deriving instance Show (MyTagType a)

instance IsTag MyTagType where
  unsafeToKeyValue ki va = case ki .&. 0x3 of
    0x0 -> KeyValue MyTagType_Single (unsafeCoerce va)
    0x1 -> KeyValue MyTagType_Multi (unsafeCoerce va)
    0x2 -> KeyValue MyTagType_Multi2 (unsafeCoerce va)
    0x3 -> KeyValue MyTagType_Multi3 (unsafeCoerce va)
    t -> error $ "Data.TagMap.unsafeToKeyValue: no such key type" <> show t

  toKey ki = case ki .&. 0x3 of
    0x0 -> Some MyTagType_Single
    0x1 -> Some MyTagType_Multi
    0x2 -> Some MyTagType_Multi2
    0x3 -> Some MyTagType_Multi3
    t -> error $ "Data.TagMap.myKeyType: no such key type" <> show t

  fromKey :: MyTagType a -> Int
  fromKey t = I# (dataToTag# t)

data Single a
data Multi
data Multi2 (k :: * -> *)
data Multi3

class MyTagTypeOffset x where
  -- | A type-directed version of `tagOffset` for MyTagType
  myTagTypeOffset :: proxy x -> Int

instance MyTagTypeOffset (Single a) where
  myTagTypeOffset _ = 0x0

instance MyTagTypeOffset Multi where
  myTagTypeOffset _ = 0x1

instance MyTagTypeOffset (Multi2 k) where
  myTagTypeOffset _ = 0x2

instance MyTagTypeOffset Multi3 where
  myTagTypeOffset _ = 0x3

instance GEq MyTagType where
  geq MyTagType_Single MyTagType_Single = Just Refl
  geq MyTagType_Multi MyTagType_Multi = Just Refl
  geq MyTagType_Multi2 MyTagType_Multi2 = Just Refl
  geq MyTagType_Multi3 MyTagType_Multi3 = Just Refl
  geq _ _ = Nothing

instance GCompare MyTagType where
  gcompare MyTagType_Single MyTagType_Single = GEQ
  gcompare MyTagType_Single _ = GLT
  gcompare _ MyTagType_Single = GGT
  gcompare MyTagType_Multi MyTagType_Multi = GEQ
  gcompare MyTagType_Multi _ = GLT
  gcompare _ MyTagType_Multi = GGT
  gcompare MyTagType_Multi2 MyTagType_Multi2 = GEQ
  gcompare MyTagType_Multi2 _ = GLT
  gcompare _ MyTagType_Multi2 = GGT
  gcompare MyTagType_Multi3 MyTagType_Multi3 = GEQ
