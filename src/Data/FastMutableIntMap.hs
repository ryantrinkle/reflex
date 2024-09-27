{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:
--   Data.FastMutableIntMap
-- Description:
--   A mutable version of 'IntMap'
module Data.FastMutableIntMap
  ( FastMutableIntMap
  , new
  , newEmpty
  , insert
  , isEmpty
  , getFrozenAndClear
  , size
  , applyPatch
  , PatchIntMap (..)
  , traverseIntMapPatchWithKey
  , lookup
  , forIntersectionWithImmutable_
  , for_
  , patchIntMapNewElements
  , patchIntMapNewElementsMap
  , getDeletions
  , toList
  ) where

--TODO: Pure JS version
--TODO: Fast copy to FastIntMap
--TODO: Fast patch type

import Prelude hiding (lookup)

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Patch.Class
import Data.Patch.IntMap

-- | A 'FastMutableIntMap' holds a map of values of type @a@ and allows low-overhead modifications via IO.
-- Operations on 'FastMutableIntMap' run in IO.
newtype FastMutableIntMap a = FastMutableIntMap (IORef (IntMap a))

-- | Create a new 'FastMutableIntMap' out of an 'IntMap'
new :: IntMap a -> IO (FastMutableIntMap a)
new m = FastMutableIntMap <$> newIORef m

-- | Create a new empty 'FastMutableIntMap'
newEmpty :: IO (FastMutableIntMap a)
newEmpty = FastMutableIntMap <$> newIORef IntMap.empty

-- | Insert an element into a 'FastMutableIntMap' at the given key
insert :: FastMutableIntMap a -> Int -> a -> IO ()
insert (FastMutableIntMap r) k v = modifyIORef' r $ IntMap.insert k v

-- | Attempt to lookup an element by key in a 'FastMutableIntMap'
lookup :: FastMutableIntMap a -> Int -> IO (Maybe a)
lookup (FastMutableIntMap r) k = IntMap.lookup k <$> readIORef r

-- | Runs the provided action over the intersection of a 'FastMutableIntMap' and an 'IntMap'
forIntersectionWithImmutable_ :: MonadIO m => FastMutableIntMap a -> IntMap b -> (a -> b -> m ()) -> m ()
forIntersectionWithImmutable_ (FastMutableIntMap r) b f = do
  a <- liftIO $ readIORef r
  traverse_ (uncurry f) $ IntMap.intersectionWith (,) a b

-- | Runs the provided action over the values of a 'FastMutableIntMap'
for_ :: MonadIO m => FastMutableIntMap a -> (a -> m ()) -> m ()
for_ (FastMutableIntMap r) f = do
  a <- liftIO $ readIORef r
  traverse_ f a

-- | Checks whether a 'FastMutableIntMap' is empty
isEmpty :: FastMutableIntMap a -> IO Bool
isEmpty (FastMutableIntMap r) = IntMap.null <$> readIORef r

-- | Retrieves the size of a 'FastMutableIntMap'
size :: FastMutableIntMap a -> IO Int
size (FastMutableIntMap r) = IntMap.size <$> readIORef r

-- | Make an immutable snapshot of the datastructure and clear it
getFrozenAndClear :: FastMutableIntMap a -> IO (IntMap a)
getFrozenAndClear (FastMutableIntMap r) = do
  result <- readIORef r
  writeIORef r IntMap.empty
  return result

-- | Updates the value of a 'FastMutableIntMap' with the given patch (see 'Data.Patch.IntMap'),
-- and returns an 'IntMap' with the modified keys and values.
applyPatch :: FastMutableIntMap a -> PatchIntMap a -> IO (IntMap a)
applyPatch (FastMutableIntMap r) p@(PatchIntMap m) = do
  v <- readIORef r
  writeIORef r $! applyAlways p v
  return $ IntMap.intersection v m

toList :: FastMutableIntMap a -> IO [(Int, a)]
toList (FastMutableIntMap r) = IntMap.toList <$> readIORef r
