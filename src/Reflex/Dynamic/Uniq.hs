{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- | This module provides a variation of 'Dynamic' values that uses cheap
-- pointer equality checks to reduce the amount of signal propagation needed.
module Reflex.Dynamic.Uniq
  ( UniqDynamic
  , uniqDynamic
  , fromUniqDynamic
  , alreadyUniqDynamic
  ) where

import GHC.Exts

#if !MIN_VERSION_base(4,18,0)
import Control.Monad.Applicative (Applicative(..))
#endif

import Reflex.Class

-- | A 'Dynamic' whose 'updated' 'Event' will never fire with the same value as
-- the 'current' 'Behavior''s contents.  In order to maintain this constraint,
-- the value inside a 'UniqDynamic' is always evaluated to
-- <https://wiki.haskell.org/Weak_head_normal_form weak head normal form>.
--
-- Internally, 'UniqDynamic' uses pointer equality as a heuristic to avoid
-- unnecessary update propagation; this is much more efficient than performing
-- full comparisons.  However, when the 'UniqDynamic' is converted back into a
-- regular 'Dynamic', a full comparison is performed.
newtype UniqDynamic t a = UniqDynamic { unUniqDynamic :: Dynamic t a }

-- | Construct a 'UniqDynamic' by eliminating redundant updates from a 'Dynamic'.
uniqDynamic :: Reflex t => Dynamic t a -> UniqDynamic t a
uniqDynamic d = UniqDynamic $ unsafeBuildDynamic (sample $ current d) $ flip pushCheap (updated d) $ \new -> do
  old <- sample $ current d --TODO: Is it better to sample ourselves here?
  return $ unsafeJustChanged old new

-- | Retrieve a normal 'Dynamic' from a 'UniqDynamic'.  This will perform a
-- final check using the output type's 'Eq' instance to ensure deterministic
-- behavior.
--
-- WARNING: If used with a type whose 'Eq' instance is not law-abiding -
-- specifically, if there are cases where @x /= x@, 'fromUniqDynamic' may
-- eliminate more 'updated' occurrences than it should.  For example, NaN values
-- of 'Double' and 'Float' are considered unequal to themselves by the 'Eq'
-- instance, but can be equal by pointer equality.  This may cause 'UniqDynamic'
-- to lose changes from NaN to NaN.
fromUniqDynamic :: (Reflex t, Eq a) => UniqDynamic t a -> Dynamic t a
fromUniqDynamic (UniqDynamic d) = unsafeDynamic (current d) e'
  where
    -- Only consider values different if they fail both pointer equality /and/
    -- 'Eq' equality.  This is to make things a bit more deterministic in the
    -- case of unlawful 'Eq' instances.  However, it is still possible to
    -- achieve nondeterminism by constructing elements that are identical in
    -- value, unequal according to 'Eq', and nondeterministically equal or
    -- nonequal by pointer quality.  I suspect that it is impossible to make the
    -- behavior deterministic in this case.
    superEq a b = a `unsafePtrEq` b || a == b
    e' = attachWithMaybe (\x x' -> if x' `superEq` x then Nothing else Just x') (current d) (updated d)

-- | Create a UniqDynamic without uniqing it on creation.  This will be slightly
-- faster than uniqDynamic when used with a Dynamic whose values are always (or
-- nearly always) different from its previous values; if used with a Dynamic
-- whose values do not change frequently, it may be much slower than uniqDynamic
alreadyUniqDynamic :: Dynamic t a -> UniqDynamic t a
alreadyUniqDynamic = UniqDynamic

unsafePtrEq :: a -> a -> Bool
unsafePtrEq a b = case a `seq` b `seq` reallyUnsafePtrEquality# a b of
  0# -> False
  _ -> True

unsafeJustChanged :: a -> a -> Maybe a
unsafeJustChanged old new =
  if old `unsafePtrEq` new
  then Nothing
  else Just new

instance Reflex t => Accumulator t (UniqDynamic t) where
  accumMaybeM f z e = do
    let f' old change = do
          mNew <- f old change
          return $ unsafeJustChanged old =<< mNew
    d <- accumMaybeMDyn f' z e
    return $ UniqDynamic d
  mapAccumMaybeM f z e = do
    let f' old change = do
          (mNew, output) <- f old change
          return (unsafeJustChanged old =<< mNew, output)
    (d, out) <- mapAccumMaybeMDyn f' z e
    return (UniqDynamic d, out)

instance Reflex t => Functor (UniqDynamic t) where
  fmap f (UniqDynamic d) = uniqDynamic $ fmap f d

instance Reflex t => Applicative (UniqDynamic t) where
  pure = UniqDynamic . constDyn
  UniqDynamic a <*> UniqDynamic b = uniqDynamic $ a <*> b
  _ *> b = b
  a <* _ = a

instance Reflex t => Monad (UniqDynamic t) where
  UniqDynamic x >>= f = uniqDynamic $ x >>= unUniqDynamic . f
