{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
#ifdef GHCJS_FAST_WEAK
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif

-- | This module defines the 'FastWeakBag' type, which represents a mutable
-- collection of items that does not cause the items to be retained in memory.
-- This is useful for situations where a value needs to be inspected or modified
-- if it is still alive, but can be ignored if it is dead.
module Data.FastWeakBag
  ( FastWeakBag
  , FastWeakBagTicket
  , empty
  , isEmpty
  , insert
  , traverse
  , traverse_
  , remove
  -- * Internal functions
  -- These will not always be available.
#ifndef GHCJS_FAST_WEAK
  , _weakBag_children --TODO: Don't export this
#endif
  ) where

import Prelude hiding (traverse)

import Control.Monad
import Control.Monad.IO.Class

#ifdef GHCJS_FAST_WEAK
import GHCJS.Types
import Reflex.FastWeak (js_isNull, unsafeFromRawJSVal, unsafeToRawJSVal)
#else
import Control.Exception
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import System.Mem.Weak
#endif

-- | A 'FastWeakBag' holds a set of values of type @/a/@, but does not retain them -
-- that is, they can still be garbage-collected.  As long as the @/a/@ values remain
-- alive, the 'FastWeakBag' will continue to refer to them.
#ifdef GHCJS_FAST_WEAK
newtype FastWeakBag a = FastWeakBag JSVal
#else
data FastWeakBag a = FastWeakBag
  { _weakBag_nextId :: {-# UNPACK #-} !(IORef Int) --TODO: what if this wraps around?
  , _weakBag_children :: {-# UNPACK #-} !(IORef (IntMap (Weak a))) -- ^ Map of items contained by the 'FastWeakBag'
  }
#endif

-- | When inserting an item into a 'FastWeakBag', a 'FastWeakBagTicket' is returned.  If
-- the caller retains the ticket, the item is guranteed to stay in memory (and
-- thus in the 'FastWeakBag').  The ticket can also be used to remove the item from
-- the 'FastWeakBag' prematurely (i.e. while it is still alive), using 'remove'.
#ifdef GHCJS_FAST_WEAK
newtype FastWeakBagTicket a = FastWeakBagTicket JSVal
#else
data FastWeakBagTicket a = FastWeakBagTicket
  { _weakBagTicket_weakItem :: {-# UNPACK #-} !(Weak a)
  , _weakBagTicket_item :: {-# NOUNPACK #-} !a
  }
#endif

-- | Insert an item into a 'FastWeakBag'.
{-# INLINE insert #-}
insert :: a -- ^ The item
       -> FastWeakBag a -- ^ The 'FastWeakBag' to insert into
       -> IO (FastWeakBagTicket a) -- ^ Returns a 'FastWeakBagTicket' that ensures the item
                           -- is retained and allows the item to be removed.
#ifdef GHCJS_FAST_WEAK
insert a wb = js_insert (unsafeToRawJSVal a) wb
foreign import javascript unsafe "$r = new h$FastWeakBagTicket($2, $1);" js_insert :: JSVal -> FastWeakBag a -> IO (FastWeakBagTicket a)
#else
insert a (FastWeakBag nextId children) = {-# SCC "insert" #-} do
  a' <- evaluate a
  myId <- atomicModifyIORef' nextId $ \n -> (succ n, n)
  let cleanup = atomicModifyIORef' children $ \cs -> (IntMap.delete myId cs, ())
  wa <- mkWeakPtr a' $ Just cleanup
  atomicModifyIORef' children $ \cs -> (IntMap.insert myId wa cs, ())
  return $ FastWeakBagTicket
    { _weakBagTicket_weakItem = wa
    , _weakBagTicket_item = a'
    }
#endif

-- | Create an empty 'FastWeakBag'.
{-# INLINE empty #-}
empty :: IO (FastWeakBag a)
#ifdef GHCJS_FAST_WEAK
empty = js_empty
foreign import javascript unsafe "$r = new h$FastWeakBag();" js_empty :: IO (FastWeakBag a)
#else
empty = {-# SCC "empty" #-} do
  nextId <- newIORef 1
  children <- newIORef IntMap.empty
  let bag = FastWeakBag
        { _weakBag_nextId = nextId
        , _weakBag_children = children
        }
  return bag
#endif

-- | Check whether a 'FastWeakBag' is empty.
{-# INLINE isEmpty #-}
isEmpty :: FastWeakBag a -> IO Bool
#ifdef GHCJS_FAST_WEAK
isEmpty = js_isEmpty
foreign import javascript unsafe "(function(){ for(var i = 0; i < $1.tickets.length; i++) { if($1.tickets[i] !== null) { return false; } }; return true; })()" js_isEmpty :: FastWeakBag a -> IO Bool --TODO: Clean up as we go along so this isn't O(n) every time
#else
isEmpty bag = {-# SCC "isEmpty" #-} IntMap.null <$> readIORef (_weakBag_children bag)
#endif

{-# INLINE traverse_ #-}
-- | Visit every node in the given list.  If new nodes are appended during the
-- traversal, they will not be visited.  Every live node that was in the list
-- when the traversal began will be visited exactly once; however, no guarantee
-- is made about the order of the traversal.
traverse_ :: forall a m. MonadIO m => FastWeakBag a -> (a -> m ()) -> m ()
#ifdef GHCJS_FAST_WEAK
traverse_ wb f = do
  let go cursor = when (not $ js_isNull cursor) $ do
        val <- liftIO $ js_getTicketValue cursor
        f $ unsafeFromRawJSVal val
        go =<< liftIO (js_getNext (FastWeakBagTicket cursor))
  go =<< liftIO (js_getInitial wb)
foreign import javascript unsafe "(function(){ for(var i = $1.tickets.length - 1; i >= 0; i--) { if($1.tickets[i] !== null) { return $1.tickets[i]; } }; return null; })()" js_getInitial :: FastWeakBag a -> IO JSVal --TODO: Clean up as we go along so this isn't O(n) every time -- Result can be null or a FastWeakBagTicket a
foreign import javascript unsafe "$r = $1.val;" js_getTicketValue :: JSVal -> IO JSVal
--TODO: Fix the race condition where if a cursor is deleted (presumably using 'remove', below) while we're holding it, it can't find its way back to the correct bag
foreign import javascript unsafe "(function(){ for(var i = $1.pos - 1; i >= 0; i--) { if($1.bag.tickets[i] !== null) { return $1.bag.tickets[i]; } }; return null; })()" js_getNext :: FastWeakBagTicket a -> IO JSVal --TODO: Clean up as we go along so this isn't O(n) every time -- Result can be null or a FastWeakBagTicket a
#else
traverse_ (FastWeakBag _ children) f = {-# SCC "traverse_" #-} do
  cs <- liftIO $ readIORef children
  forM_ cs $ \c -> do
    ma <- liftIO $ deRefWeak c
    mapM_ f ma
#endif

{-# DEPRECATED traverse "Use 'traverse_' instead" #-}
traverse :: forall a m. MonadIO m => FastWeakBag a -> (a -> m ()) -> m ()
traverse = traverse_

-- | Remove an item from the 'FastWeakBag'; does nothing if invoked multiple times
-- on the same 'FastWeakBagTicket'.
{-# INLINE remove #-}
remove :: FastWeakBagTicket a -> IO ()
#ifdef GHCJS_FAST_WEAK
remove = js_remove
foreign import javascript unsafe "$1.bag.tickets[$1.pos] = null; $1.bag = new h$FastWeakBag(); $1.bag.tickets.push($1); $1.pos = 0;" js_remove :: FastWeakBagTicket a -> IO () --TODO: Don't bother with the new surrogate FastWeakBag; instead, make the GC check for bag === null, and then null it out here
#else
remove (FastWeakBagTicket w _) = {-# SCC "remove" #-} finalize w
#endif
--TODO: Should 'remove' also drop the reference to the item?

--TODO: can/should we provide a null FastWeakBagTicket?
