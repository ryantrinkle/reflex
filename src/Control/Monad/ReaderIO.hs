{-# language RoleAnnotations #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language CPP #-}

module Control.Monad.ReaderIO
  ( ReaderIO (..)
  ) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad.Fix
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class

-- | An approximate clone of @RIO@ from the @rio@ package, but not based on
-- @ReaderT@. The trouble with @ReaderT@ is that its third type argument has a
-- @nominal@ role, so we can't coerce through it when it's wrapped in some
-- other @data@ type. Ugh.
newtype ReaderIO e a = ReaderIO { runReaderIO :: e -> IO a }
type role ReaderIO representational representational

instance Functor (ReaderIO e) where
  fmap = liftM
  {-# INLINE fmap #-}
  a <$ m = m >> pure a
  {-# INLINE (<$) #-}

instance Applicative (ReaderIO e) where
  pure a = ReaderIO $ \_ -> pure a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,10,0)
  liftA2 = liftM2
  {-# INLINE liftA2 #-}
#endif
  (*>) = (>>)
  {-# INLINE (*>) #-}

instance Monad (ReaderIO e) where
  ReaderIO q >>= f = ReaderIO $ \e -> q e >>= \a -> runReaderIO (f a) e
  {-# INLINE (>>=) #-}

instance MonadFix (ReaderIO e) where
  mfix f = ReaderIO $ \e -> mfix $ \r -> runReaderIO (f r) e
  {-# INLINE mfix #-}

instance MonadIO (ReaderIO e) where
  liftIO m = ReaderIO $ \_ -> m
  {-# INLINE liftIO #-}

instance MonadReader e (ReaderIO e) where
  ask = ReaderIO pure
  {-# INLINE ask #-}
  local f (ReaderIO m) = ReaderIO (m . f)
  {-# INLINE local #-}
