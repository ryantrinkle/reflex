-- | This module defines the 'EventWriter' class.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

module Reflex.EventWriter.Class
  ( EventWriter (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)

#if !MIN_VERSION_base(4,18,0)
import Data.Semigroup (Semigroup)
#endif

import Reflex.Class (Event)


-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them via 'Semigroup' to provide an 'Event' result.
class (Monad m, Semigroup w) => EventWriter t w m | m -> t w where
  tellEvent :: Event t w -> m ()


instance EventWriter t w m => EventWriter t w (ReaderT r m) where
  tellEvent = lift . tellEvent
