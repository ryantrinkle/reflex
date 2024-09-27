{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- | This module defines the 'WeakBag' type, which represents a mutable
-- collection of items that does not cause the items to be retained in memory.
-- This is useful for situations where a value needs to be inspected or modified
-- if it is still alive, but can be ignored if it is dead.
module Data.WeakBag
  ( WeakBag
  , WeakBagTicket
  , empty
  , singleton
  , insert
  , traverse
  , traverse_
  , remove
  -- * Internal functions
  -- These will not always be available.
  , _weakBag_children --TODO: Don't export this
  ) where

import Prelude hiding (traverse)

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import System.Mem.Weak

-- | A 'WeakBag' holds a set of values of type @/a/@, but does not retain them -
-- that is, they can still be garbage-collected.  As long as the @/a/@ values remain
-- alive, the 'WeakBag' will continue to refer to them.
data WeakBag a = WeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(IORef Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(IORef (IntMap (Weak a))) -- ^ The items referenced by the WeakBag
  }

-- | When inserting an item into a 'WeakBag', a 'WeakBagTicket' is returned.  If
-- the caller retains the ticket, the item is guranteed to stay in memory (and
-- thus in the 'WeakBag').  The ticket can also be used to remove the item from
-- the 'WeakBag' prematurely (i.e. while it is still alive), using 'remove'.
data WeakBagTicket = forall a. WeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }

-- | Insert an item into a 'WeakBag'.
{-# INLINE insert #-}
insert :: a -- ^ The item
       -> WeakBag a -- ^ The 'WeakBag' to insert into
       -> IORef (Weak b) -- ^ An arbitrary value to be used in the following
                         -- callback
       -> (b -> IO ()) -- ^ A callback to be invoked when the item is removed
                       -- (whether automatically by the item being garbage
                       -- collected or manually via 'remove')
       -> IO WeakBagTicket -- ^ Returns a 'WeakBagTicket' that ensures the item
                           -- is retained and allows the item to be removed.
insert a (WeakBag nextId children) wbRef finalizer = {-# SCC "insert" #-} do
  a' <- evaluate a
  wbRef' <- evaluate wbRef
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  let cleanup = do
        wb <- readIORef wbRef'
        mb <- deRefWeak wb
        forM_ mb $ \b -> do
          csWithoutMe <- atomicModifyIORef children $ \cs ->
            let !csWithoutMe = IntMap.delete myId cs
            in (csWithoutMe, csWithoutMe)
          when (IntMap.null csWithoutMe) $ finalizer b
  wa <- mkWeakPtr a' $ Just cleanup
  atomicModifyIORef' children $ \cs -> (IntMap.insert myId wa cs, ())
  return $ WeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }

-- | Create an empty 'WeakBag'.
{-# INLINE empty #-}
empty :: IO (WeakBag a)
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  let bag = WeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag

-- | Create a 'WeakBag' with one item; equivalent to creating the 'WeakBag' with
-- 'empty', then using 'insert'.
{-# INLINE singleton #-}
singleton :: a -> IORef (Weak b) -> (b -> IO ()) -> IO (WeakBag a, WeakBagTicket)
singleton a wbRef finalizer = {-# SCC "singleton" #-} do
  bag <- empty
  ticket <- insert a bag wbRef finalizer
  return (bag, ticket)

{-# INLINE traverse_ #-}
-- | Visit every node in the given list.  If new nodes are appended during the
-- traversal, they will not be visited.  Every live node that was in the list
-- when the traversal began will be visited exactly once; however, no guarantee
-- is made about the order of the traversal.
traverse_ :: MonadIO m => WeakBag a -> (a -> m ()) -> m ()
traverse_ (WeakBag _ children) f = {-# SCC "traverse" #-} do
  cs <- liftIO $ readIORef children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma

{-# DEPRECATED traverse "Use 'traverse_' instead" #-}
traverse :: MonadIO m => WeakBag a -> (a -> m ()) -> m ()
traverse = traverse_

-- | Remove an item from the 'WeakBag'; does nothing if invoked multiple times
-- on the same 'WeakBagTicket'.
{-# INLINE remove #-}
remove :: WeakBagTicket -> IO ()
remove (WeakBagTicket w _) = {-# SCC "remove" #-} finalize w
--TODO: Should 'remove' also drop the reference to the item?

--TODO: can/should we provide a null WeakBagTicket?
