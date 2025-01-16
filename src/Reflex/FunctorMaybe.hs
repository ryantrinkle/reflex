{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module:
--   Reflex.FunctorMaybe
-- Description:
--   This module defines the FunctorMaybe class, which extends Functors with the
--   ability to delete values.
module Reflex.FunctorMaybe
  ( FunctorMaybe (..)
  ) where

import Data.IntMap (IntMap)
import Data.Map (Map)
#if !MIN_VERSION_base(4,16,0)
import Data.Semigroup (Option(..))
#endif
import Witherable

--TODO: See if there's a better class in the standard libraries already

-- | A class for values that combines filtering and mapping using 'Maybe'.
-- Morally, @'FunctorMaybe' ~ KleisliFunctor 'Maybe'@.
{-# DEPRECATED FunctorMaybe "Use 'Filterable' from Data.Witherable instead" #-}
class FunctorMaybe f where
  -- | Combined mapping and filtering function.
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

instance FunctorMaybe Maybe where
  fmapMaybe = mapMaybe

#if !MIN_VERSION_base(4,16,0)
deriving instance FunctorMaybe Option
#endif

instance FunctorMaybe [] where
  fmapMaybe = mapMaybe

instance FunctorMaybe (Map k) where
  fmapMaybe = mapMaybe

instance FunctorMaybe IntMap where
  fmapMaybe = mapMaybe
