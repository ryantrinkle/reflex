{-|
Module: Reflex.BehaviorWriter.Class
Description: This module defines the 'BehaviorWriter' class
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

module Reflex.BehaviorWriter.Class
  ( MonadBehaviorWriter
  , BehaviorWriter(..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Reflex.Class (Behavior)

{-# DEPRECATED MonadBehaviorWriter "Use 'BehaviorWriter' instead" #-}
-- | Type synonym for 'BehaviorWriter'
type MonadBehaviorWriter = BehaviorWriter

-- | 'BehaviorWriter' efficiently collects 'Behavior' values using 'tellBehavior'
-- and combines them monoidally to provide a 'Behavior' result.
class (Monad m, Monoid w) => BehaviorWriter t w m | m -> t w where
  tellBehavior :: Behavior t w -> m ()

instance BehaviorWriter t w m => BehaviorWriter t w (ReaderT r m) where
  tellBehavior = lift . tellBehavior
