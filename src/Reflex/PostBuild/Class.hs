-- | This module defines 'PostBuild', which indicates that an action will be
-- notified when it, and any action it's a part of, has finished executing.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

module Reflex.PostBuild.Class
  ( PostBuild (..)
  ) where

import Reflex.Class

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict

-- | 'PostBuild' represents an action that is notified via an 'Event' when it
-- has finished executing.  Note that the specific definition of "finished" is
-- determined by the instance of 'PostBuild', but the intent is to allow
-- 'Behavior's and 'Dynamic's to be safely sampled, regardless of where they
-- were created, when the post-build 'Event' fires.  The post-build 'Event' will
-- fire exactly once for an given action.
class (Reflex t, Monad m) => PostBuild t m | m -> t where
  -- | Retrieve the post-build 'Event' for this action.
  getPostBuild :: m (Event t ())

instance PostBuild t m => PostBuild t (ReaderT r m) where
  getPostBuild = lift getPostBuild

instance PostBuild t m => PostBuild t (StateT s m) where
  getPostBuild = lift getPostBuild

instance PostBuild t m => PostBuild t (Strict.StateT s m) where
  getPostBuild = lift getPostBuild
