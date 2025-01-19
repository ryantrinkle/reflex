-- | This module defines 'Requester', which indicates that an action can make
-- requests and receive responses to them.  Typically, this is used for things
-- like a WebSocket, where it's desirable to collect many potential sources of
-- events and send them over a single channel, then distribute the results back
-- out efficiently to their original request sites.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Class
 ( Requester (..)
 , withRequesting
 , requestingIdentity
 ) where

import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict
import Data.Kind (Type)
import Reflex.Class

-- | A 'Requester' action can trigger requests of type @Request m a@ based on
-- 'Event's, and receive responses of type @Response m a@ in return.  Note that
-- the @a@ type can vary within the 'Requester' action, but will be linked for a
-- given request.  For example, if @Request m@ is 'IO' and @Response m@ is
-- 'Identity', then 'requestingIdentity' has the same type as
-- 'Reflex.PerformEvent.Class.performEvent'.
class (Reflex t, Monad m) => Requester t m | m -> t where
  -- | The type of requests that this 'Requester' can emit
  type Request m :: Type -> Type
  -- | The type of responses that this 'Requester' can receive
  type Response m :: Type -> Type
  -- | Emit a request whenever the given 'Event' fires, and return responses in
  -- the resulting 'Event'.
  requesting :: Event t (Request m a) -> m (Event t (Response m a))
  -- | Emit a request whenever the given 'Event' fires, and ignore all responses.
  requesting_ :: Event t (Request m a) -> m ()


instance Requester t m => Requester t (ReaderT r m) where
  type Request (ReaderT r m) = Request m
  type Response (ReaderT r m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance Requester t m => Requester t (StateT s m) where
  type Request (StateT s m) = Request m
  type Response (StateT s m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance Requester t m => Requester t (Lazy.StateT s m) where
  type Request (Lazy.StateT s m) = Request m
  type Response (Lazy.StateT s m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

-- | Emit a request whenever the given 'Event' fires, and unwrap the responses
-- before returning them.  @Response m@ must be 'Identity'.
requestingIdentity :: (Requester t m, Response m ~ Identity) => Event t (Request m a) -> m (Event t a)
requestingIdentity = fmap coerceEvent . requesting

withRequesting :: (Requester t m, MonadFix m) => (Event t (Response m a) -> m (Event t (Request m a), r)) -> m r
withRequesting f = do
  rec response <- requesting request
      (request, result) <- f response
  return result
