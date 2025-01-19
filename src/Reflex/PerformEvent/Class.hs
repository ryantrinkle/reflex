-- | This module defines 'PerformEvent', which mediates the
-- interaction between a "Reflex"-based program and the external side-effecting
-- actions such as 'IO'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PerformEvent.Class
  ( PerformEvent (..)
  , performEventAsync
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Kind (Type)

import Reflex.Class
import Reflex.TriggerEvent.Class

-- | 'PerformEvent' represents actions that can trigger other actions based on
-- 'Event's.
class (Reflex t, Monad (Performable m), Monad m) => PerformEvent t m | m -> t where
  -- | The type of action to be triggered; this is often not the same type as
  -- the triggering action.
  type Performable m :: Type -> Type
  -- | Perform the action contained in the given 'Event' whenever the 'Event'
  -- fires.  Return the result in another 'Event'.  Note that the output 'Event'
  -- will generally occur later than the input 'Event', since most 'Performable'
  -- actions cannot be performed during 'Event' propagation.
  performEvent :: Event t (Performable m a) -> m (Event t a)
  -- | Like 'performEvent', but do not return the result.  May have slightly
  -- better performance.
  performEvent_ :: Event t (Performable m ()) -> m ()

-- | Like 'performEvent', but the resulting 'Event' occurs only when the
-- callback (@a -> IO ()@) is called, not when the included action finishes.
--
-- NOTE: Despite the name, 'performEventAsync' does not run its action in a
-- separate thread - although the action is free to invoke forkIO and then call
-- the callback whenever it is ready.  This will work properly, even in GHCJS
-- (which fully implements concurrency even though JavaScript does not have
-- built in concurrency).
{-# INLINABLE performEventAsync #-}
performEventAsync :: (TriggerEvent t m, PerformEvent t m) => Event t ((a -> IO ()) -> Performable m ()) -> m (Event t a)
performEventAsync e = do
  (eOut, triggerEOut) <- newTriggerEvent
  performEvent_ $ fmap ($ triggerEOut) e
  return eOut

instance PerformEvent t m => PerformEvent t (ReaderT r m) where
  type Performable (ReaderT r m) = ReaderT r (Performable m)
  performEvent_ e = do
    r <- ask
    lift $ performEvent_ $ flip runReaderT r <$> e
  performEvent e = do
    r <- ask
    lift $ performEvent $ flip runReaderT r <$> e

instance PerformEvent t m => PerformEvent t (MaybeT m) where
  type Performable (MaybeT m) = MaybeT (Performable m)
  performEvent_ = lift . performEvent_ . fmapCheap (void . runMaybeT)
  performEvent = lift . fmap (fmapMaybe id) . performEvent . fmapCheap runMaybeT
