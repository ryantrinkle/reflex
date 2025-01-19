{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
{-# OPTIONS_GHC -Wunused-binds #-}
-- | This module is the implementation of the 'Spider' 'Reflex' engine.  It uses
-- a graph traversal algorithm to propagate 'Event's and 'Behavior's.
module Reflex.Spider.Internal (module Reflex.Spider.Internal) where

#if MIN_VERSION_base(4,10,0)
import Control.Applicative (liftA2)
#endif
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.ReaderIO
import Control.Monad.Ref
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import qualified Control.Monad.Fail as MonadFail
import Data.Align
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.FastMutableIntMap (FastMutableIntMap)
import qualified Data.FastMutableIntMap as FastMutableIntMap
import Data.Foldable hiding (concat, elem, sequence_)
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Kind (Type)
import Data.Maybe hiding (mapMaybe)
import Data.Proxy
import Data.These
import Data.Traversable
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts hiding (toList)
import GHC.IORef (IORef (..))
import GHC.Stack
import Reflex.FastWeak
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce
import Witherable (Filterable, mapMaybe)

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_these(0,8,0)
import Data.These.Combinators (justThese)
#endif
#if MIN_VERSION_semialign(1,1,0)
import Data.Zip (Zip (..))
#endif
#endif

#ifdef DEBUG_CYCLES
import Control.Monad.State hiding (forM, forM_, mapM, mapM_, sequence)
#endif

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tree (Forest, Tree (..), drawForest)

#ifdef DEBUG_HIDE_INTERNALS
import Data.List (isPrefixOf)
#endif

import Data.FastWeakBag (FastWeakBag, FastWeakBagTicket)
import qualified Data.FastWeakBag as FastWeakBag

import Data.Reflection
import Data.Some (Some(Some))
import Data.Type.Coercion
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.WeakBag (WeakBag, WeakBagTicket, _weakBag_children)
import qualified Data.WeakBag as WeakBag
import qualified Reflex.Class
import qualified Reflex.Class as R
import qualified Reflex.Host.Class
import Reflex.NotReady.Class
import Data.Patch
import qualified Data.Patch.DMapWithMove as PatchDMapWithMove
import Reflex.PerformEvent.Base (PerformEventT)

#ifdef DEBUG_TRACE_EVENTS
import qualified Data.ByteString.Char8 as BS8
import System.IO (stderr)
import Data.List (isPrefixOf)
#endif

-- TODO stdout might not be the best channel for debug output
debugStrLn :: String -> IO ()
debugStrLn = putStrLn

#ifdef DEBUG_TRACE_EVENTS
withStackOneLine :: (BS8.ByteString -> a) -> a
withStackOneLine expr = unsafePerformIO $ do
  stack <- currentCallStack
  return (expr . BS8.pack  . unwords . dropInternal . reverse $ stack)
    where dropInternal = filterStack "Reflex.Spider.Internal"

#endif

debugPropagate :: Bool

debugInvalidateHeight :: Bool

debugInvalidate :: Bool

#ifdef DEBUG
#define DEBUG_NODEIDS

#ifdef DEBUG_TRACE_PROPAGATION
debugPropagate = True
#else
debugPropagate = False
#endif

#ifdef DEBUG_TRACE_HEIGHT
debugInvalidateHeight = True
#else
debugInvalidateHeight = False
#endif

#ifdef DEBUG_TRACE_INVALIDATION
debugInvalidate = True
#else
debugInvalidate = False
#endif

class HasNodeId a where
  getNodeId :: a -> Int

instance HasNodeId (CacheSubscribed x a) where
  getNodeId = _cacheSubscribed_nodeId

instance HasNodeId (FanInt x a) where
  getNodeId = _fanInt_nodeId

instance HasNodeId (Hold x p) where
  getNodeId = holdNodeId

instance HasNodeId (SwitchSubscribed x a) where
  getNodeId = switchSubscribedNodeId

instance HasNodeId (FanSubscribed x v a) where
  getNodeId = fanSubscribedNodeId

instance HasNodeId (CoincidenceSubscribed x a) where
  getNodeId = coincidenceSubscribedNodeId

instance HasNodeId (RootSubscribed x a) where
  getNodeId = rootSubscribedNodeId

instance HasNodeId (Pull x a) where
  getNodeId = pullNodeId

{-# INLINE showNodeId #-}
showNodeId :: HasNodeId a => a -> String
showNodeId = showNodeId' . getNodeId

showNodeId' :: Int -> String
showNodeId' = ("#"<>) . show


#else

debugPropagate = False
debugInvalidateHeight = False
debugInvalidate = False

-- This must be inline, or error messages will cause memory leaks due to retaining the node in question
{-# INLINE showNodeId #-}
showNodeId :: a -> String
showNodeId _ = ""

{-# INLINE showNodeId' #-}
showNodeId' :: Int -> String
showNodeId' _ = ""

#endif

#ifdef DEBUG_NODEIDS
{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef Int
nextNodeIdRef = unsafePerformIO $ newIORef 1

newNodeId :: IO Int
newNodeId = atomicModifyIORef' nextNodeIdRef $ \n -> (succ n, n)
#endif

--------------------------------------------------------------------------------
-- EventSubscription
--------------------------------------------------------------------------------

--NB: Once you subscribe to an Event, you must always hold on the the WHOLE EventSubscription you get back
-- If you do not retain the subscription, you may be prematurely unsubscribed from the parent event.
data EventSubscription x = EventSubscription
  { _eventSubscription_unsubscribe :: !(IO ())
  , _eventSubscription_subscribed :: {-# UNPACK #-} !(EventSubscribed x)
  }

unsubscribe :: EventSubscription x -> IO ()
unsubscribe (EventSubscription u _) = u

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------

newtype Event x a = Event { unEvent :: Subscriber x a -> EventM x (EventSubscription x, Maybe a) }

{-# INLINE subscribeAndRead #-}
subscribeAndRead :: Event x a -> Subscriber x a -> EventM x (EventSubscription x, Maybe a)
subscribeAndRead = unEvent

{-# RULES
"cacheEvent/cacheEvent" forall e. cacheEvent (cacheEvent e) = cacheEvent e
"cacheEvent/pushCheap" forall f e. pushCheap f (cacheEvent e) = cacheEvent (pushCheap f e)
"hold/cacheEvent" forall f e. hold f (cacheEvent e) = hold f e
  #-}

-- | Construct an 'Event' equivalent to that constructed by 'push', but with no
-- caching; if the computation function is very cheap, this is (much) more
-- efficient than 'push'
{-# INLINE [1] pushCheap #-}
pushCheap :: (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
pushCheap !f e = Event $ \sub -> do
  (subscription, occ) <- subscribeAndRead e $ debugSubscriber' "push" $ sub
    { subscriberPropagate = \a -> do
        mb <- f a
        mapM_ (subscriberPropagate sub) mb
    }
  occ' <- join <$> mapM f occ
  return (subscription, occ')

-- | A subscriber that never triggers other 'Event's
{-# INLINE terminalSubscriber #-}
terminalSubscriber :: (a -> EventM x ()) -> Subscriber x a
terminalSubscriber p = Subscriber
  { subscriberPropagate = p
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

-- | Subscribe to an Event only for the duration of one occurrence
{-# INLINE subscribeAndReadHead #-}
subscribeAndReadHead :: Event x a -> Subscriber x a -> EventM x (EventSubscription x, Maybe a)
subscribeAndReadHead e sub = do
  subscriptionRef <- liftIO $ newIORef $ error "subscribeAndReadHead: not initialized"
  (subscription, occ) <- subscribeAndRead e $ debugSubscriber' "head" $ sub
    { subscriberPropagate = \a -> do
        liftIO $ unsubscribe =<< readIORef subscriptionRef
        subscriberPropagate sub a
    }
  liftIO $ case occ of
    Nothing -> writeIORef subscriptionRef $! subscription
    Just _ -> unsubscribe subscription
  return (subscription, occ)

--TODO: Make this lazy in its input event
headE :: (Defer (SomeMergeInit x) m) => Event x a -> m (Event x a)
headE originalE = do
  parent <- liftIO $ newIORef $ Just originalE
  defer $ SomeMergeInit $ do --TODO: Rename SomeMergeInit appropriately
    let clearParent = liftIO $ writeIORef parent Nothing
    (_, occ) <- subscribeAndReadHead originalE $ terminalSubscriber $ const clearParent
    when (isJust occ) clearParent
  return $ Event $ \sub ->
    liftIO (readIORef parent) >>= \case
      Nothing -> subscribeAndReadNever
      Just e -> subscribeAndReadHead e sub

data CacheSubscribed x a
   = CacheSubscribed { _cacheSubscribed_subscribers :: {-# UNPACK #-} !(FastWeakBag (Subscriber x a))
                     , _cacheSubscribed_parent :: {-# UNPACK #-} !(EventSubscription x)
                     , _cacheSubscribed_occurrence :: {-# UNPACK #-} !(IORef (Maybe a))
#ifdef DEBUG_NODEIDS
                     , _cacheSubscribed_nodeId :: {-# UNPACK #-} !Int
#endif
                     }

nowSpiderEventM :: (HasSpiderTimeline x) => EventM x (R.Event (SpiderTimeline x) ())
nowSpiderEventM =
  SpiderEvent <$> now

now :: (Defer (Some Clear) m) => m (Event x ())
now = do
  nowOrNot <- liftIO $ newIORef $ Just ()
  scheduleClear nowOrNot
  return . Event $ \_ -> do
    occ <- liftIO . readIORef $ nowOrNot
    return ( EventSubscription (return ()) eventSubscribedNow
           , occ
           )

-- | Construct an 'Event' whose value is guaranteed not to be recomputed
-- repeatedly
--
--TODO: Try a caching strategy where we subscribe directly to the parent when
--there's only one subscriber, and then build our own FastWeakBag only when a second
--subscriber joins
{-# NOINLINE [0] cacheEvent #-}
cacheEvent :: forall x a. HasSpiderTimeline x => Event x a -> Event x a
cacheEvent e =
#ifdef DEBUG_TRACE_EVENTS
  withStackOneLine $ \callSite -> Event $
#else
  Event $
#endif
  unsafePerformIO $ do
    mSubscribedRef :: IORef (FastWeak (CacheSubscribed x a))
        <- newIORef emptyFastWeak
    pure $ \sub -> {-# SCC "cacheEvent" #-} do
#ifdef DEBUG_TRACE_EVENTS
          unless (BS8.null callSite) $ liftIO $ BS8.hPutStrLn stderr callSite
#endif
          subscribedTicket <- liftIO (readIORef mSubscribedRef >>= getFastWeakTicket) >>= \case
            Just subscribedTicket -> return subscribedTicket
            Nothing -> do
#ifdef DEBUG_NODEIDS
              nodeId <- liftIO newNodeId
#endif
              subscribers <- liftIO FastWeakBag.empty
              occRef <- liftIO $ newIORef Nothing -- This should never be read prior to being set below
#ifdef DEBUG_NODEIDS
              (parentSub, occ) <- subscribeAndRead e $ debugSubscriber' ("cacheEvent" <> showNodeId' nodeId) $ Subscriber
#else
              (parentSub, occ) <- subscribeAndRead e $ Subscriber
#endif
                  { subscriberPropagate = \a -> do
                      liftIO $ writeIORef occRef (Just a)
                      scheduleClear occRef
                      propagateFast a subscribers
                  , subscriberInvalidateHeight = FastWeakBag.traverse_ subscribers . invalidateSubscriberHeight
                  , subscriberRecalculateHeight = FastWeakBag.traverse_ subscribers . recalculateSubscriberHeight
                  }
              when (isJust occ) $ do
                liftIO $ writeIORef occRef occ -- Set the initial value of occRef; we don't need to do this if occ is Nothing
                scheduleClear occRef
              let !subscribed = CacheSubscribed
                    { _cacheSubscribed_subscribers = subscribers
                    , _cacheSubscribed_parent = parentSub
                    , _cacheSubscribed_occurrence = occRef
#ifdef DEBUG_NODEIDS
                    , _cacheSubscribed_nodeId = nodeId
#endif
                    }
              subscribedTicket <- liftIO $ mkFastWeakTicket subscribed
              liftIO $ writeIORef mSubscribedRef =<< getFastWeakTicketWeak subscribedTicket
              return subscribedTicket
          liftIO $ cacheSubscription sub mSubscribedRef subscribedTicket

cacheSubscription :: Subscriber x a -> IORef (FastWeak (CacheSubscribed x a))
                  -> FastWeakTicket (CacheSubscribed x a) -> IO (EventSubscription x, Maybe a)
cacheSubscription sub mSubscribedRef subscribedTicket = do
  subscribed <- getFastWeakTicketValue subscribedTicket
  ticket <- FastWeakBag.insert sub $ _cacheSubscribed_subscribers subscribed
  occ <- readIORef $ _cacheSubscribed_occurrence subscribed

  let parentSub = _cacheSubscribed_parent subscribed
      es = EventSubscription
        { _eventSubscription_unsubscribe = do
          FastWeakBag.remove ticket

          isEmpty <- FastWeakBag.isEmpty $ _cacheSubscribed_subscribers subscribed
          when isEmpty $ do
            writeIORef mSubscribedRef emptyFastWeak
            unsubscribe parentSub
          touch ticket
          touch subscribedTicket
        , _eventSubscription_subscribed = EventSubscribed
          { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed parentSub
          , eventSubscribedRetained = toAny subscribedTicket
#ifdef DEBUG_CYCLES
          , eventSubscribedGetParents = return [_eventSubscription_subscribed parentSub]
          , eventSubscribedHasOwnHeightRef = False
          , eventSubscribedWhoCreated = whoCreatedIORef mSubscribedRef
#endif
          }
        }
  return (es, occ)


subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribe e s = fst <$> subscribeAndRead e s

{-# INLINE wrap #-}
wrap :: MonadIO m => (t -> EventSubscribed x) -> (Subscriber x a -> m (WeakBagTicket, t, Maybe a)) -> Subscriber x a -> m (EventSubscription x, Maybe a)
wrap tag getSpecificSubscribed sub = do
  (sln, subd, occ) <- getSpecificSubscribed sub
  let es = tag subd
  return (EventSubscription (WeakBag.remove sln >> touch sln) es, occ)

eventRoot :: (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ wrap eventSubscribedRoot $ liftIO . getRootSubscribed k r

subscribeAndReadNever :: EventM x (EventSubscription x, Maybe a)
subscribeAndReadNever = return (EventSubscription (return ()) eventSubscribedNever, Nothing)

eventNever :: Event x a
eventNever = Event $ const subscribeAndReadNever

eventFan :: (GCompare k, HasSpiderTimeline x) => k a -> Fan x k v -> Event x (v a)
eventFan !k !f = Event $ wrap eventSubscribedFan $ getFanSubscribed k f

eventSwitch :: HasSpiderTimeline x => Switch x a -> Event x a
eventSwitch !s = Event $ wrap eventSubscribedSwitch $ getSwitchSubscribed s

eventCoincidence :: HasSpiderTimeline x => Coincidence x a -> Event x a
eventCoincidence !c = Event $ wrap eventSubscribedCoincidence $ getCoincidenceSubscribed c

eventHold :: Hold x p -> Event x p
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Event x p
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
subscribeCoincidenceInner :: HasSpiderTimeline x => Event x a -> Height -> CoincidenceSubscribed x a -> EventM x (Maybe a, Height, EventSubscribed x)
subscribeCoincidenceInner inner outerHeight subscribedUnsafe = do
  subInner <- liftIO $ newSubscriberCoincidenceInner subscribedUnsafe
  (subscription@(EventSubscription _ innerSubd), innerOcc) <- subscribeAndRead inner subInner
  innerHeight <- liftIO $ getEventSubscribedHeight innerSubd
  let height = max innerHeight outerHeight
  defer $ SomeResetCoincidence subscription $ if height > outerHeight then Just subscribedUnsafe else Nothing
  return (innerOcc, height, innerSubd)

--------------------------------------------------------------------------------
-- Subscriber
--------------------------------------------------------------------------------

data Subscriber x a = Subscriber
  { subscriberPropagate :: !(a -> EventM x ())
  , subscriberInvalidateHeight :: !(Height -> IO ())
  , subscriberRecalculateHeight :: !(Height -> IO ())
  }

newSubscriberHold :: (HasSpiderTimeline x, Patch p) => Hold x p -> IO (Subscriber x p)
newSubscriberHold h = return $ Subscriber
  { subscriberPropagate = {-# SCC "traverseHold" #-} propagateSubscriberHold h
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

newSubscriberFan :: forall x k v. (HasSpiderTimeline x, GCompare k) => FanSubscribed x k v -> IO (Subscriber x (DMap k v))
newSubscriberFan subscribed = debugSubscriber ("SubscriberFan " <> showNodeId subscribed)  $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
      subs <- liftIO $ readIORef $ fanSubscribedSubscribers subscribed
      tracePropagate (Proxy :: Proxy x) $ show (DMap.size subs) <> " keys subscribed, " <> show (DMap.size a) <> " keys firing"
      liftIO $ writeIORef (fanSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ fanSubscribedOccurrence subscribed
      let f _ (Pair v subsubs) = do
            propagate v $ _fanSubscribedChildren_list subsubs
            return $ Constant ()
      _ <- DMap.traverseWithKey f $ DMap.intersectionWithKey (\_ -> Pair) a subs --TODO: Would be nice to have DMap.traverse_
      return ()
  , subscriberInvalidateHeight = \old -> do
      subscribers <- readIORef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse_ (_fanSubscribedChildren_list v) $ invalidateSubscriberHeight old
  , subscriberRecalculateHeight = \new -> do
      subscribers <- readIORef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse_ (_fanSubscribedChildren_list v) $ recalculateSubscriberHeight new
  }

newSubscriberSwitch :: forall x a. HasSpiderTimeline x => SwitchSubscribed x a -> IO (Subscriber x a)
newSubscriberSwitch subscribed = debugSubscriber ("SubscriberCoincidenceOuter" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseSwitch" #-} do
      liftIO $ writeIORef (switchSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ switchSubscribedOccurrence subscribed
      propagate a $ switchSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \_ -> do
      oldHeight <- readIORef $ switchSubscribedHeight subscribed
      when (oldHeight /= invalidHeight) $ do
        writeIORef (switchSubscribedHeight subscribed) $! invalidHeight
        WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight
  , subscriberRecalculateHeight = (`updateSwitchHeight` subscribed)
    }

newSubscriberCoincidenceOuter :: forall x b. HasSpiderTimeline x => CoincidenceSubscribed x b -> IO (Subscriber x (Event x b))
newSubscriberCoincidenceOuter subscribed = debugSubscriber ("SubscriberCoincidenceOuter" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceOuter" #-} do
      outerHeight <- liftIO $ readIORef $ coincidenceSubscribedHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  outerHeight = " <> show outerHeight
      (occ, innerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  isJust occ = " <> show (isJust occ)
      tracePropagate (Proxy :: Proxy x) $ "  innerHeight = " <> show innerHeight

      liftIO $ writeIORef (coincidenceSubscribedInnerParent subscribed) $ Just innerSubd
      scheduleClear $ coincidenceSubscribedInnerParent subscribed
      case occ of
        Nothing ->
          when (innerHeight > outerHeight) $ liftIO $ do -- If the event fires, it will fire at a later height
            writeIORef (coincidenceSubscribedHeight subscribed) $! innerHeight
            WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight outerHeight
            WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight innerHeight
        Just o -> do -- Since it's already firing, no need to adjust height
          liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) occ
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate o $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

newSubscriberCoincidenceInner :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO (Subscriber x a)
newSubscriberCoincidenceInner subscribed = debugSubscriber ("SubscriberCoincidenceInner" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceInner" #-} do
      occ <- liftIO $ readIORef $ coincidenceSubscribedOccurrence subscribed
      case occ of
        Just _ -> return () -- SubscriberCoincidenceOuter must have already propagated this event
        Nothing -> do
          liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) $ Just a
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate a $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

invalidateSubscriberHeight :: Height -> Subscriber x a -> IO ()
invalidateSubscriberHeight = flip subscriberInvalidateHeight

recalculateSubscriberHeight :: Height -> Subscriber x a -> IO ()
recalculateSubscriberHeight = flip subscriberRecalculateHeight

-- | Propagate everything at the current height
propagate :: forall x a. a -> WeakBag (Subscriber x a) -> EventM x ()
propagate a subscribers = withIncreasedDepth (Proxy::Proxy x) $
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  WeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

-- | Propagate everything at the current height
propagateFast :: forall x a. a -> FastWeakBag (Subscriber x a) -> EventM x ()
propagateFast a subscribers = withIncreasedDepth (Proxy::Proxy x) $
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  FastWeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

--------------------------------------------------------------------------------
-- EventSubscribed
--------------------------------------------------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

-- Why do we use Any here, instead of just giving eventSubscribedRetained an
-- existential type? Sadly, GHC does not currently know how to unbox types
-- with existentially quantified fields. So instead we just coerce values
-- to type Any on the way in. Since we never coerce them back, this is
-- perfectly safe.
data EventSubscribed x = EventSubscribed
  { eventSubscribedHeightRef :: {-# UNPACK #-} !(IORef Height)
  , eventSubscribedRetained :: {-# NOUNPACK #-} !Any
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents :: !(IO [EventSubscribed x]) -- For debugging loops
  , eventSubscribedHasOwnHeightRef :: !Bool
  , eventSubscribedWhoCreated :: !(IO [String])
#endif
  }

eventSubscribedRoot :: RootSubscribed x a -> EventSubscribed x
eventSubscribedRoot !r = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny r
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["root"]
#endif
  }

eventSubscribedNever :: EventSubscribed x
eventSubscribedNever = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny ()
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["never"]
#endif
  }
eventSubscribedNow :: EventSubscribed x
eventSubscribedNow = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny ()
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["now"]
#endif
  }

eventSubscribedFan :: FanSubscribed x k v -> EventSubscribed x
eventSubscribedFan !subscribed = EventSubscribed
  { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed $ fanSubscribedParent subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return [_eventSubscription_subscribed $ fanSubscribedParent subscribed]
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = whoCreatedIORef $ fanSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedSwitch :: SwitchSubscribed x a -> EventSubscribed x
eventSubscribedSwitch !subscribed = EventSubscribed
  { eventSubscribedHeightRef = switchSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      s <- readIORef $ switchSubscribedCurrentParent subscribed
      return [_eventSubscription_subscribed s]
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ switchSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedCoincidence :: CoincidenceSubscribed x a -> EventSubscribed x
eventSubscribedCoincidence !subscribed = EventSubscribed
  { eventSubscribedHeightRef = coincidenceSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      innerSubscription <- readIORef $ coincidenceSubscribedInnerParent subscribed
      let outerParent = _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
          innerParents = maybeToList $ innerSubscription
      return $ outerParent : innerParents
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ coincidenceSubscribedCachedSubscribed subscribed
#endif
  }

getEventSubscribedHeight :: EventSubscribed x -> IO Height
getEventSubscribedHeight es = readIORef $ eventSubscribedHeightRef es

#ifdef DEBUG_CYCLES
whoCreatedEventSubscribed :: EventSubscribed x -> IO [String]
whoCreatedEventSubscribed = eventSubscribedWhoCreated

walkInvalidHeightParents :: EventSubscribed x -> IO [EventSubscribed x]
walkInvalidHeightParents s0 = do
  subscribers <- flip execStateT mempty $ ($ s0) $ fix $ \loop s -> do
    h <- liftIO $ readIORef $ eventSubscribedHeightRef s
    when (h == invalidHeight) $ do
      when (eventSubscribedHasOwnHeightRef s) $ liftIO $ writeIORef (eventSubscribedHeightRef s) $! invalidHeightBeingTraversed
      modify (s :)
      mapM_ loop =<< liftIO (eventSubscribedGetParents s)
  forM_ subscribers $ \s -> writeIORef (eventSubscribedHeightRef s) $! invalidHeight
  return subscribers
#endif

{-# INLINE subscribeHoldEvent #-}
subscribeHoldEvent :: Hold x p -> Subscriber x p -> EventM x (EventSubscription x, Maybe p)
subscribeHoldEvent = subscribeAndRead . holdEvent

--------------------------------------------------------------------------------
-- Behavior
--------------------------------------------------------------------------------

newtype Behavior x a = Behavior { readBehaviorTracked :: BehaviorM x a }

behaviorHold :: Hold x p -> Behavior x (PatchTarget p)
behaviorHold !h = Behavior $ readHoldTracked h

behaviorHoldIdentity :: Hold x (Identity a) -> Behavior x a
behaviorHoldIdentity = behaviorHold

behaviorConst :: a -> Behavior x a
behaviorConst !a = Behavior $ return a

behaviorPull :: Pull x a -> Behavior x a
behaviorPull !p = Behavior $ do
    val <- liftIO $ readIORef $ pullValue p
    case val of
      Just subscribed -> do
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (pullSubscribedInvalidators subscribed) (wi:))
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        i <- liftIO $ newInvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newIORef []
        holdInits <- askBehaviorHoldInits
        a <- liftIO $ runReaderIO (unBehaviorM $ pullCompute p) (Just (wi, parentsRef), holdInits)
        invsRef <- liftIO . newIORef . maybeToList =<< askInvalidator
        parents <- liftIO $ readIORef parentsRef
        let subscribed = PullSubscribed
              { pullSubscribedValue = a
              , pullSubscribedInvalidators = invsRef
              , pullSubscribedOwnInvalidator = i
              , pullSubscribedParents = parents
              }
        liftIO $ writeIORef (pullValue p) $ Just subscribed
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        return a

behaviorDyn :: Patch p => Dyn x p -> Behavior x (PatchTarget p)
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

{-# INLINE readHoldTracked #-}
readHoldTracked :: Hold x p -> BehaviorM x (PatchTarget p)
readHoldTracked h = do
  result <- liftIO $ readIORef $ holdValue h
  askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (holdInvalidators h) (wi:))
  askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedHold h)) :))
  liftIO $ touch h -- Otherwise, if this gets inlined enough, the hold's parent reference may get collected
  return result

{-# INLINABLE readBehaviorUntracked #-}
readBehaviorUntracked :: Defer (SomeHoldInit x) m => Behavior x a -> m a
readBehaviorUntracked b = do
  holdInits <- getDeferralQueue
  liftIO $ runBehaviorM (readBehaviorTracked b) Nothing holdInits --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

--------------------------------------------------------------------------------
-- Dynamic
--------------------------------------------------------------------------------

type DynamicS x p = Dynamic x (PatchTarget p) p

data Dynamic x target p = Dynamic
  { dynamicCurrent :: !(Behavior x target)
  , dynamicUpdated :: Event x p -- This must be lazy; see the comment on holdEvent --TODO: Would this let us eliminate `Dyn`?
  }

deriving instance (HasSpiderTimeline x) => Functor (Dynamic x target)




dynamicHold :: Hold x p -> DynamicS x p
dynamicHold !h = Dynamic
  { dynamicCurrent = behaviorHold h
  , dynamicUpdated = eventHold h
  }

dynamicHoldIdentity :: Hold x (Identity a) -> DynamicS x (Identity a)
dynamicHoldIdentity = dynamicHold

dynamicConst :: PatchTarget p -> DynamicS x p
dynamicConst !a = Dynamic
  { dynamicCurrent = behaviorConst a
  , dynamicUpdated = eventNever
  }

dynamicDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> DynamicS x p
dynamicDyn !d = Dynamic
  { dynamicCurrent = behaviorDyn d
  , dynamicUpdated = eventDyn d
  }

dynamicDynIdentity :: HasSpiderTimeline x => Dyn x (Identity a) -> DynamicS x (Identity a)
dynamicDynIdentity = dynamicDyn

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

--type role Hold representational
data Hold x p
   = Hold { holdValue :: !(IORef (PatchTarget p))
          , holdInvalidators :: !(IORef [Weak (Invalidator x)])
          , holdEvent :: Event x p -- This must be lazy, or holds cannot be defined before their input Events
          , holdParent :: !(IORef (Maybe (EventSubscription x))) -- Keeps its parent alive (will be undefined until the hold is initialized) --TODO: Probably shouldn't be an IORef
#ifdef DEBUG_NODEIDS
          , holdNodeId :: Int
#endif
          }

-- | A statically allocated 'SpiderTimeline'
data Global

{-# NOINLINE globalSpiderTimelineEnv #-}
globalSpiderTimelineEnv :: SpiderTimelineEnv Global
globalSpiderTimelineEnv = unsafePerformIO unsafeNewSpiderTimelineEnv

-- | Stores all global data relevant to a particular Spider timeline; only one
-- value should exist for each type @x@
newtype SpiderTimelineEnv x = STE {unSTE :: SpiderTimelineEnv' x}
-- We implement SpiderTimelineEnv with a newtype wrapper so
-- we can get the coercions we want safely.
type role SpiderTimelineEnv nominal

data SpiderTimelineEnv' x = SpiderTimelineEnv
  { _spiderTimeline_lock :: {-# UNPACK #-} !(MVar ())
  , _spiderTimeline_eventEnv :: {-# UNPACK #-} !(EventEnv x)
#ifdef DEBUG
  , _spiderTimeline_depth :: {-# UNPACK #-} !(IORef Int)
#endif
  }
type role SpiderTimelineEnv' phantom

instance Eq (SpiderTimelineEnv x) where
  _ == _ = True -- Since only one exists of each type

instance GEq SpiderTimelineEnv where
  a `geq` b = if _spiderTimeline_lock (unSTE a) == _spiderTimeline_lock (unSTE b)
              then Just $ unsafeCoerce Refl -- This unsafeCoerce is safe because the same SpiderTimelineEnv can't have two different 'x' arguments
              else Nothing

data EventEnv x
   = EventEnv { eventEnvAssignments :: !(IORef [SomeAssignment x]) -- Needed for Subscribe
              , eventEnvHoldInits :: !(IORef [SomeHoldInit x]) -- Needed for Subscribe
              , eventEnvDynInits :: !(IORef [SomeDynInit x])
              , eventEnvMergeUpdates :: !(IORef [SomeMergeUpdate x])
              , eventEnvMergeInits :: !(IORef [SomeMergeInit x]) -- Needed for Subscribe
              , eventEnvClears :: !(IORef [Some Clear]) -- Needed for Subscribe
              , eventEnvIntClears :: !(IORef [Some IntClear])
              , eventEnvRootClears :: !(IORef [Some RootClear])
              , eventEnvCurrentHeight :: !(IORef Height) -- Needed for Subscribe
              , eventEnvResetCoincidences :: !(IORef [SomeResetCoincidence x]) -- Needed for Subscribe
              , eventEnvDelayedMerges :: !(IORef (IntMap [EventM x ()]))
              }

{-# INLINE runEventM #-}
runEventM :: EventM x a -> IO a
runEventM = unEventM

asksEventEnv :: forall x a. HasSpiderTimeline x => (EventEnv x -> a) -> EventM x a
asksEventEnv f = return $ f $ _spiderTimeline_eventEnv (unSTE (spiderTimeline :: SpiderTimelineEnv x))

class MonadIO m => Defer a m where
  getDeferralQueue :: m (IORef [a])

{-# INLINE defer #-}
defer :: Defer a m => a -> m ()
defer a = do
  q <- getDeferralQueue
  liftIO $ modifyIORef' q (a:)

instance HasSpiderTimeline x => Defer (SomeAssignment x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvAssignments

instance HasSpiderTimeline x => Defer (SomeHoldInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvHoldInits

instance HasSpiderTimeline x => Defer (SomeDynInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvDynInits

instance Defer (SomeHoldInit x) (BehaviorM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = BehaviorM $ asks snd

instance HasSpiderTimeline x => Defer (SomeMergeUpdate x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeUpdates

instance HasSpiderTimeline x => Defer (SomeMergeInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeInits

class HasSpiderTimeline x => HasCurrentHeight x m | m -> x where
  getCurrentHeight :: m Height
  scheduleMerge :: Height -> EventM x () -> m ()

instance HasSpiderTimeline x => HasCurrentHeight x (EventM x) where
  {-# INLINE getCurrentHeight #-}
  getCurrentHeight = do
    heightRef <- asksEventEnv eventEnvCurrentHeight
    liftIO $ readIORef heightRef
  {-# INLINE scheduleMerge #-}
  scheduleMerge height subscribed = do
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    liftIO $ modifyIORef' delayedRef $ IntMap.insertWith (++) (unHeight height) [subscribed]

class HasSpiderTimeline x where
  -- | Retrieve the current SpiderTimelineEnv
  spiderTimeline :: SpiderTimelineEnv x

instance HasSpiderTimeline Global where
  spiderTimeline = globalSpiderTimelineEnv

putCurrentHeight :: HasSpiderTimeline x => Height -> EventM x ()
putCurrentHeight h = do
  heightRef <- asksEventEnv eventEnvCurrentHeight
  liftIO $ writeIORef heightRef $! h

instance HasSpiderTimeline x => Defer (Some Clear) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvClears

{-# INLINE scheduleClear #-}
scheduleClear :: Defer (Some Clear) m => IORef (Maybe a) -> m ()
scheduleClear r = defer $ Some $ Clear r

instance HasSpiderTimeline x => Defer (Some IntClear) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvIntClears

{-# INLINE scheduleIntClear #-}
scheduleIntClear :: Defer (Some IntClear) m => IORef (IntMap a) -> m ()
scheduleIntClear r = defer $ Some $ IntClear r

instance HasSpiderTimeline x => Defer (Some RootClear) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvRootClears

{-# INLINE scheduleRootClear #-}
scheduleRootClear :: Defer (Some RootClear) m => IORef (DMap k Identity) -> m ()
scheduleRootClear r = defer $ Some $ RootClear r

instance HasSpiderTimeline x => Defer (SomeResetCoincidence x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvResetCoincidences

-- Note: hold cannot examine its event until after the phase is over
{-# INLINE [1] hold #-}
hold :: (Patch p, Defer (SomeHoldInit x) m) => PatchTarget p -> Event x p -> m (Hold x p)
hold v0 e = do
  valRef <- liftIO $ newIORef v0
  invsRef <- liftIO $ newIORef []
  parentRef <- liftIO $ newIORef Nothing
#ifdef DEBUG_NODEIDS
  nodeId <- liftIO newNodeId
#endif
  let h = Hold
        { holdValue = valRef
        , holdInvalidators = invsRef
        , holdEvent = e
        , holdParent = parentRef
#ifdef DEBUG_NODEIDS
        , holdNodeId = nodeId
#endif
        }
  defer $ SomeHoldInit h
  return h

{-# INLINE getHoldEventSubscription #-}
getHoldEventSubscription :: forall p x. (HasSpiderTimeline x, Patch p) => Hold x p -> EventM x (EventSubscription x)
getHoldEventSubscription h = do
  ep <- liftIO $ readIORef $ holdParent h
  case ep of
    Just subd -> return subd
    Nothing -> do
      let e = holdEvent h
      subscriptionRef <- liftIO $ newIORef $ error "getHoldEventSubscription: subdRef uninitialized"
      (subscription@(EventSubscription _ _), occ) <- subscribeAndRead e =<< liftIO (newSubscriberHold h)
      liftIO $ writeIORef subscriptionRef $! subscription
      case occ of
        Nothing -> return ()
        Just o -> do
          old <- liftIO $ readIORef $ holdValue h
          case apply o old of
            Nothing -> return ()
            Just new -> do
              -- Need to evaluate these so that we don't retain the Hold itself
              v <- liftIO $ evaluate $ holdValue h
              i <- liftIO $ evaluate $ holdInvalidators h
              defer $ SomeAssignment v i new
      liftIO $ writeIORef (holdParent h) $ Just subscription
      return subscription

type BehaviorEnv x = (Maybe (Weak (Invalidator x), IORef [SomeBehaviorSubscribed x]), IORef [SomeHoldInit x])

-- BehaviorM can sample behaviors
newtype BehaviorM x a = BehaviorM { unBehaviorM :: ReaderIO (BehaviorEnv x) a }
  deriving (Functor, Applicative, MonadIO, MonadFix, MonadReader (BehaviorEnv x))

instance Monad (BehaviorM x) where
  {-# INLINE (>>=) #-}
  BehaviorM x >>= f = BehaviorM $ x >>= unBehaviorM . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail s = BehaviorM $ fail s
#endif

data BehaviorSubscribed x a
   = forall p. BehaviorSubscribedHold (Hold x p)
   | BehaviorSubscribedPull (PullSubscribed x a)

newtype SomeBehaviorSubscribed x = SomeBehaviorSubscribed (Some (BehaviorSubscribed x))

--type role PullSubscribed representational
data PullSubscribed x a
   = PullSubscribed { pullSubscribedValue :: !a
                    , pullSubscribedInvalidators :: !(IORef [Weak (Invalidator x)])
                    , pullSubscribedOwnInvalidator :: !(Invalidator x)
                    , pullSubscribedParents :: ![SomeBehaviorSubscribed x] -- Need to keep parent behaviors alive, or they won't let us know when they're invalidated
                    }

--type role Pull representational
data Pull x a
   = Pull { pullValue :: !(IORef (Maybe (PullSubscribed x a)))
          , pullCompute :: !(BehaviorM x a)
#ifdef DEBUG_NODEIDS
          , pullNodeId :: Int
#endif
          }

data Invalidator x
   = forall a. InvalidatorPull (Pull x a)
   | forall a. InvalidatorSwitch (SwitchSubscribed x a)

data RootSubscribed x a = forall k. GCompare k => RootSubscribed
  { rootSubscribedKey :: !(k a)
  , rootSubscribedCachedSubscribed :: !(IORef (DMap k (RootSubscribed x))) -- From the original Root
  , rootSubscribedSubscribers :: !(WeakBag (Subscriber x a))
  , rootSubscribedOccurrence :: !(IO (Maybe a)) -- Lookup from rootOccurrence
  , rootSubscribedUninit :: IO ()
  , rootSubscribedWeakSelf :: !(IORef (Weak (RootSubscribed x a))) --TODO: Can we make this a lazy non-IORef and then force it manually to avoid an indirection each time we use it?
#ifdef DEBUG_NODEIDS
  , rootSubscribedNodeId :: Int
#endif
  }

data Root x k
   = Root { rootOccurrence :: !(IORef (DMap k Identity)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(IORef (DMap k (RootSubscribed x)))
          , rootInit :: !(forall a. k a -> RootTrigger x a -> IO (IO ()))
          }

data SomeHoldInit x = forall p. Patch p => SomeHoldInit !(Hold x p)

data SomeDynInit x = forall p. Patch p => SomeDynInit !(Dyn x p)

data SomeMergeUpdate x = SomeMergeUpdate
  { _someMergeUpdate_update :: !(EventM x [EventSubscription x])
  , _someMergeUpdate_invalidateHeight :: !(IO ())
  , _someMergeUpdate_recalculateHeight :: !(IO ())
  }

newtype SomeMergeInit x = SomeMergeInit { unSomeMergeInit :: EventM x () }

-- EventM can do everything BehaviorM can, plus create holds
newtype EventM x a = EventM { unEventM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException, MonadCatch, MonadThrow, MonadMask)

newtype MergeSubscribedParent x a = MergeSubscribedParent { unMergeSubscribedParent :: EventSubscription x }

data MergeSubscribedParentWithMove x k a = MergeSubscribedParentWithMove
  { _mergeSubscribedParentWithMove_subscription :: !(EventSubscription x)
  , _mergeSubscribedParentWithMove_key :: !(IORef (k a))
  }

data HeightBag = HeightBag
  { _heightBag_size :: {-# UNPACK #-} !Int
  , _heightBag_contents :: !(IntMap Word) -- Number of excess in each bucket
  }
  deriving (Show, Read, Eq, Ord)

heightBagEmpty :: HeightBag
heightBagEmpty = heightBagVerify $ HeightBag 0 IntMap.empty

heightBagSize :: HeightBag -> Int
heightBagSize = _heightBag_size

heightBagFromList :: [Height] -> HeightBag
heightBagFromList heights = heightBagVerify $ foldl' (flip heightBagAdd) heightBagEmpty heights

heightBagAdd :: Height -> HeightBag -> HeightBag
heightBagAdd (Height h) (HeightBag s c) = heightBagVerify $ HeightBag (succ s) $
  IntMap.insertWithKey (\_ _ old -> succ old) h 0 c

heightBagRemove :: Height -> HeightBag -> HeightBag
heightBagRemove (Height h) b@(HeightBag s c) = heightBagVerify $ case IntMap.lookup h c of
  Nothing -> error $ "heightBagRemove: Height " <> show h <> " not present in bag " <> show b
  Just old -> HeightBag (pred s) $ case old of
    0 -> IntMap.delete h c
    _ -> IntMap.insert h (pred old) c

heightBagRemoveMaybe :: Height -> HeightBag -> Maybe HeightBag
heightBagRemoveMaybe (Height h) (HeightBag s c) = heightBagVerify . removed <$> IntMap.lookup h c where
  removed old = HeightBag (pred s) $ case old of
    0 -> IntMap.delete h c
    _ -> IntMap.insert h (pred old) c


heightBagMax :: HeightBag -> Height
heightBagMax (HeightBag _ c) = case IntMap.maxViewWithKey c of
  Just ((h, _), _) -> Height h
  Nothing -> zeroHeight

heightBagVerify :: HeightBag -> HeightBag
#ifdef DEBUG
heightBagVerify b@(HeightBag s c) = if
  | s /= IntMap.size c + fromIntegral (sum (IntMap.elems c))
    -> error $ "heightBagVerify: size doesn't match: " <> show b
  | unHeight invalidHeight `IntMap.member` c
    -> error $ "heightBagVerify: contains invalid height: " <> show b
  | otherwise -> b
#else
heightBagVerify = id
#endif

data FanSubscribedChildren x k v a = FanSubscribedChildren
  { _fanSubscribedChildren_list :: !(WeakBag (Subscriber x (v a)))
  , _fanSubscribedChildren_self :: {-# NOUNPACK #-} !(k a, FanSubscribed x k v)
  , _fanSubscribedChildren_weakSelf :: !(IORef (Weak (k a, FanSubscribed x k v)))
  }

data FanSubscribed x k v
   = FanSubscribed { fanSubscribedCachedSubscribed :: !(IORef (Maybe (FanSubscribed x k v)))
                   , fanSubscribedOccurrence :: !(IORef (Maybe (DMap k v)))
                   , fanSubscribedSubscribers :: !(IORef (DMap k (FanSubscribedChildren x k v))) -- This DMap should never be empty
                   , fanSubscribedParent :: !(EventSubscription x)
#ifdef DEBUG_NODEIDS
                   , fanSubscribedNodeId :: Int
#endif
                   }

data Fan x k v
   = Fan { fanParent :: !(Event x (DMap k v))
         , fanSubscribed :: !(IORef (Maybe (FanSubscribed x k v)))
         }

data SwitchSubscribed x a
   = SwitchSubscribed { switchSubscribedCachedSubscribed :: !(IORef (Maybe (SwitchSubscribed x a)))
                      , switchSubscribedOccurrence :: !(IORef (Maybe a))
                      , switchSubscribedHeight :: !(IORef Height)
                      , switchSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                      , switchSubscribedOwnInvalidator :: {-# NOUNPACK #-} !(Invalidator x)
                      , switchSubscribedOwnWeakInvalidator :: !(IORef (Weak (Invalidator x)))
                      , switchSubscribedBehaviorParents :: !(IORef [SomeBehaviorSubscribed x])
                      , switchSubscribedParent :: !(Behavior x (Event x a))
                      , switchSubscribedCurrentParent :: !(IORef (EventSubscription x))
                      , switchSubscribedWeakSelf :: !(IORef (Weak (SwitchSubscribed x a)))
#ifdef DEBUG_NODEIDS
                      , switchSubscribedNodeId :: Int
#endif
                      }

data Switch x a
   = Switch { switchParent :: !(Behavior x (Event x a))
            , switchSubscribed :: !(IORef (Maybe (SwitchSubscribed x a)))
            }

#ifdef USE_TEMPLATE_HASKELL
{-# ANN CoincidenceSubscribed "HLint: ignore Redundant bracket" #-}
#endif
data CoincidenceSubscribed x a
   = CoincidenceSubscribed { coincidenceSubscribedCachedSubscribed :: !(IORef (Maybe (CoincidenceSubscribed x a)))
                           , coincidenceSubscribedOccurrence :: !(IORef (Maybe a))
                           , coincidenceSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                           , coincidenceSubscribedHeight :: !(IORef Height)
                           , coincidenceSubscribedOuter :: {-# NOUNPACK #-} (Subscriber x (Event x a))
                           , coincidenceSubscribedOuterParent :: !(EventSubscription x)
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventSubscribed x)))
                           , coincidenceSubscribedWeakSelf :: !(IORef (Weak (CoincidenceSubscribed x a)))
#ifdef DEBUG_NODEIDS
                           , coincidenceSubscribedNodeId :: Int
#endif
                           }

data Coincidence x a
   = Coincidence { coincidenceParent :: !(Event x (Event x a))
                 , coincidenceSubscribed :: !(IORef (Maybe (CoincidenceSubscribed x a)))
                 }

{-# NOINLINE newInvalidatorSwitch #-}
newInvalidatorSwitch :: SwitchSubscribed x a -> IO (Invalidator x)
newInvalidatorSwitch subd = return $! InvalidatorSwitch subd

{-# NOINLINE newInvalidatorPull #-}
newInvalidatorPull :: Pull x a -> IO (Invalidator x)
newInvalidatorPull p = return $! InvalidatorPull p

instance HasSpiderTimeline x => Filterable (Event x) where
  mapMaybe f = push $ return . f

instance HasSpiderTimeline x => Align (Event x) where
  nil = eventNever
#if MIN_VERSION_these(0, 8, 0)
instance HasSpiderTimeline x => Semialign (Event x) where
#endif
  align ea eb = mapMaybe dmapToThese $ mergeG coerce $ dynamicConst $
     DMap.fromDistinctAscList [LeftTag :=> ea, RightTag :=> eb]

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_semialign(1,1,0)
instance HasSpiderTimeline x => Zip (Event x) where
#endif
  zip x y = mapMaybe justThese $ align x y
#endif

data DynType x p = UnsafeDyn !(BehaviorM x (PatchTarget p), Event x p)
                 | BuildDyn  !(EventM x (PatchTarget p), Event x p)
                 | HoldDyn   !(Hold x p)

newtype Dyn (x :: Type) p = Dyn { unDyn :: IORef (DynType x p) }

newMapDyn :: HasSpiderTimeline x => (a -> b) -> DynamicS x (Identity a) -> DynamicS x (Identity b)
newMapDyn f d = dynamicDynIdentity $ unsafeBuildDynamic (fmap f $ readBehaviorTracked $ dynamicCurrent d) (Identity . f . runIdentity <$> dynamicUpdated d)

--TODO: Avoid the duplication between this and R.zipDynWith
zipDynWith :: HasSpiderTimeline x => (a -> b -> c) -> DynamicS x (Identity a) -> DynamicS x (Identity b) -> DynamicS x (Identity c)
zipDynWith f da db =
  let eab = align (dynamicUpdated da) (dynamicUpdated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This (Identity a) -> do
            b <- readBehaviorUntracked $ dynamicCurrent db
            return (a, b)
          That (Identity b) -> do
            a <- readBehaviorUntracked $ dynamicCurrent da
            return (a, b)
          These (Identity a) (Identity b) -> return (a, b)
        return $ Just $ Identity $ f a b
  in dynamicDynIdentity $ unsafeBuildDynamic (f <$> readBehaviorUntracked (dynamicCurrent da) <*> readBehaviorUntracked (dynamicCurrent db)) ec

buildDynamic :: (Defer (SomeDynInit x) m, Patch p) => EventM x (PatchTarget p) -> Event x p -> m (Dyn x p)
buildDynamic readV0 v' = do
  result <- liftIO $ newIORef $ BuildDyn (readV0, v')
  let !d = Dyn result
  defer $ SomeDynInit d
  return d

unsafeBuildDynamic :: BehaviorM x (PatchTarget p) -> Event x p -> Dyn x p
unsafeBuildDynamic readV0 v' =
  Dyn $ unsafePerformIO $ newIORef $ UnsafeDyn (readV0, v')

-- ResultM can read behaviors and events
type ResultM = EventM

instance HasSpiderTimeline x => Functor (Event x) where
  fmap f = push $ return . Just . f

instance Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

{-# INLINE push #-}
push :: HasSpiderTimeline x => (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
push f e = cacheEvent (pushCheap f e)

{-# INLINABLE pull #-}
pull :: BehaviorM x a -> Behavior x a
pull a = unsafePerformIO $ do
  ref <- newIORef Nothing
#ifdef DEBUG_NODEIDS
  nid <- newNodeId
#endif
  pure $ behaviorPull $ Pull
    { pullCompute = a
    , pullValue = ref
#ifdef DEBUG_NODEIDS
    , pullNodeId = nid
#endif
    }

{-# INLINABLE switch #-}
switch :: HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switch a = unsafePerformIO $ do
  ref <- newIORef Nothing
  pure $ eventSwitch $ Switch
    { switchParent = a
    , switchSubscribed = ref
    }

coincidence :: HasSpiderTimeline x => Event x (Event x a) -> Event x a
coincidence a = unsafePerformIO $ do
  ref <- newIORef Nothing
  pure $ eventCoincidence $ Coincidence
    { coincidenceParent = a
    , coincidenceSubscribed = ref
    }

-- Propagate the given event occurrence; before cleaning up, run the given action, which may read the state of events and behaviors
run :: forall x b. HasSpiderTimeline x => [DSum (RootTrigger x) Identity] -> ResultM x b -> SpiderHost x b
run roots after = do
  tracePropagate (Proxy :: Proxy x) $ "Running an event frame with " <> show (length roots) <> " events"
  let t = spiderTimeline :: SpiderTimelineEnv x
  result <- SpiderHost $ withMVar (_spiderTimeline_lock (unSTE t)) $ \_ -> unSpiderHost $ runFrame $ do
    rootsToPropagate <- forM roots $ \r@(RootTrigger (_, occRef, k) :=> a) -> do
      occBefore <- liftIO $ do
        occBefore <- readIORef occRef
        writeIORef occRef $! DMap.insert k a occBefore
        return occBefore
      if DMap.null occBefore
        then do scheduleRootClear occRef
                return $ Just r
        else return Nothing
    forM_ (catMaybes rootsToPropagate) $ \(RootTrigger (subscribersRef, _, _) :=> Identity a) -> do
      propagate a subscribersRef
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    let go = do
          delayed <- liftIO $ readIORef delayedRef
          case IntMap.minViewWithKey delayed of
            Nothing -> return ()
            Just ((currentHeight, cur), future) -> do
              tracePropagate (Proxy :: Proxy x) $ "Running height " ++ show currentHeight
              putCurrentHeight $ Height currentHeight
              liftIO $ writeIORef delayedRef $! future
              sequence_ cur
              go
    go
    putCurrentHeight maxBound
    after
  tracePropagate (Proxy :: Proxy x) "Done running an event frame"
  return result

scheduleMerge' :: HasSpiderTimeline x => Height -> IORef Height -> EventM x () -> EventM x ()
scheduleMerge' initialHeight heightRef a = scheduleMerge initialHeight $ do
  height <- liftIO $ readIORef heightRef
  currentHeight <- getCurrentHeight
  case height `compare` currentHeight of
    LT -> error "Somehow a merge's height has been decreased after it was scheduled"
    GT -> scheduleMerge' height heightRef a -- The height has been increased (by a coincidence event; TODO: is this the only way?)
    EQ -> a

newtype Clear a = Clear (IORef (Maybe a))

newtype IntClear a = IntClear (IORef (IntMap a))

newtype RootClear k = RootClear (IORef (DMap k Identity))

data SomeAssignment x = forall a. SomeAssignment {-# UNPACK #-} !(IORef a) {-# UNPACK #-} !(IORef [Weak (Invalidator x)]) a

debugFinalize :: Bool
debugFinalize = False

mkWeakPtrWithDebug :: a -> String -> IO (Weak a)
mkWeakPtrWithDebug x debugNote = do
  x' <- evaluate x
  mkWeakPtr x' $
    if debugFinalize
    then Just $ debugStrLn $ "finalizing: " ++ debugNote
    else Nothing

type WeakList a = [Weak a]

type CanTrace x m = (HasSpiderTimeline x, MonadIO m)




#ifdef DEBUG

debugSubscriber :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber description = return . debugSubscriber' description

debugSubscriber' :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> Subscriber x a
debugSubscriber' description subscribed = Subscriber
  {
    subscriberPropagate = \m -> do
      tracePropagate (Proxy :: Proxy x) ("subscriberPropagate: " <> description)
      subscriberPropagate subscribed m
  , subscriberInvalidateHeight = \old -> do
      traceInvalidateHeight $ "invalidateSubscriberHeight: " <> description <> ", old = " <> show (unHeight old)
      subscriberInvalidateHeight subscribed old
      traceInvalidateHeight $ "invalidateSubscriberHeight: " <> description <> ", done"
  , subscriberRecalculateHeight = \new -> do
      traceInvalidateHeight $ "subscriberRecalculateHeight: " <> description <> ", new = " <> show (unHeight new)
      subscriberRecalculateHeight subscribed new
      traceInvalidateHeight $ "subscriberRecalculateHeight: " <> description <> ", done"
  }


{-# INLINE withIncreasedDepth #-}
withIncreasedDepth :: forall proxy x m a. CanTrace x m => proxy x -> m a -> m a
withIncreasedDepth _ a = do
  liftIO $ modifyIORef' (_spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) succ
  result <- a
  liftIO $ modifyIORef' (_spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) pred
  return result

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => proxy x -> String -> m ()
tracePropagate p = when debugPropagate . trace p

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate = when debugInvalidate . liftIO . debugStrLn

{-# INLINE traceInvalidateHeight #-}
traceInvalidateHeight :: String -> IO ()
traceInvalidateHeight = when debugInvalidateHeight . liftIO . debugStrLn

{-# INLINE trace #-}
trace :: (CanTrace x m) => proxy x ->  String -> m ()
trace p message = traceM p $ return message

{-# INLINE traceM #-}
traceM :: forall x proxy m. (CanTrace x m) => proxy x -> m String -> m ()
traceM _ getMessage = do
  message <- getMessage
  d <- liftIO $ readIORef $ _spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)
  liftIO $ debugStrLn $ replicate d ' ' <> message

#else

{-# INLINE withIncreasedDepth #-}
withIncreasedDepth ::  proxy x -> m a -> m a
withIncreasedDepth _ = id

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => proxy x -> String -> m ()
tracePropagate _ _ = return ()

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate _ = return ()

{-# INLINE traceInvalidateHeight #-}
traceInvalidateHeight :: String -> IO ()
traceInvalidateHeight _ = return ()

{-# INLINE debugSubscriber #-}
debugSubscriber :: String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber _ = return

{-# INLINE debugSubscriber' #-}
debugSubscriber' :: String -> Subscriber x a -> Subscriber x a
debugSubscriber' _ = id



{-# INLINE trace #-}
trace :: (CanTrace x m) => proxy x ->  String -> m ()
trace _ _ = return ()

{-# INLINE traceM #-}
traceM :: (CanTrace x m) => proxy x -> m String -> m ()
traceM _ _ = return ()

#endif

whoCreatedIORef :: IORef a -> IO [String]
whoCreatedIORef (IORef a) = whoCreated $! a

groupByHead :: Eq a => [NonEmpty a] -> [(a, NonEmpty [a])]
groupByHead = \case
  [] -> []
  (x :| xs) : t -> case groupByHead t of
    [] -> [(x, xs :| [])]
    l@((y, yss) : t')
      | x == y -> (x, xs `NonEmpty.cons` yss) : t'
      | otherwise -> (x, xs :| []) : l

listsToForest :: Eq a => [[a]] -> Forest a
listsToForest lists = buildForest <$> groupByHead (mapMaybe nonEmpty lists)
    where buildForest (a, lists') = Node a $ listsToForest $ toList lists'

showStacks :: [[String]] -> String
showStacks = drawForest . listsToForest . fmap (filterStack "Reflex.Spider.Internal")

filterStack :: String -> [String] -> [String]
#ifdef DEBUG_HIDE_INTERNALS
filterStack prefix = filter (not . (prefix `isPrefixOf`))
#else
filterStack _prefix = id
#endif

#ifdef DEBUG_CYCLES

data EventLoopException = EventLoopException [[String]]
instance Exception EventLoopException

instance Show EventLoopException where
  show (EventLoopException stacks) = "causality loop detected:\n" <> if null stacks
    then "no location information, compile with profiling enabled for stack tree"
    else showStacks stacks

#else

data EventLoopException = EventLoopException
instance Exception EventLoopException

instance Show EventLoopException where
  show EventLoopException = "causality loop detected: \n" <>
    "compile reflex with flag 'debug-cycles' and compile with profiling enabled for stack tree"

#endif


{-# INLINE propagateSubscriberHold #-}
propagateSubscriberHold :: forall x p. (HasSpiderTimeline x, Patch p) => Hold x p -> p -> EventM x ()
propagateSubscriberHold h a = do
  {-# SCC "trace" #-} when debugPropagate $ traceM (Proxy :: Proxy x) $ liftIO $ do
    invalidators <- liftIO $ readIORef $ holdInvalidators h
    return $ "SubscriberHold" <> showNodeId h <> ": " ++ show (length invalidators)

  v <- {-# SCC "read" #-} liftIO $ readIORef $ holdValue h
  case {-# SCC "apply" #-} apply a v of
    Nothing -> return ()
    Just v' -> do
      {-# SCC "trace2" #-} withIncreasedDepth (Proxy :: Proxy x) $
        tracePropagate (Proxy :: Proxy x) ("propagateSubscriberHold: assigning Hold" <> showNodeId h)
      vRef <- {-# SCC "vRef" #-} liftIO $ evaluate $ holdValue h
      iRef <- {-# SCC "iRef" #-} liftIO $ evaluate $ holdInvalidators h
      defer $ {-# SCC "assignment" #-} SomeAssignment vRef iRef v'

data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x) !(Maybe (CoincidenceSubscribed x a)) -- The CoincidenceSubscriber will be present only if heights need to be reset

runBehaviorM :: BehaviorM x a -> Maybe (Weak (Invalidator x), IORef [SomeBehaviorSubscribed x]) -> IORef [SomeHoldInit x] -> IO a
runBehaviorM a mwi holdInits = runReaderIO (unBehaviorM a) (mwi, holdInits)

askInvalidator :: BehaviorM x (Maybe (Weak (Invalidator x)))
askInvalidator = do
  (!m, _) <- ask
  case m of
    Nothing -> return Nothing
    Just (!wi, _) -> return $ Just wi

askParentsRef :: BehaviorM x (Maybe (IORef [SomeBehaviorSubscribed x]))
askParentsRef = do
  (!m, _) <- ask
  case m of
    Nothing -> return Nothing
    Just (_, !p) -> return $ Just p

askBehaviorHoldInits :: BehaviorM x (IORef [SomeHoldInit x])
askBehaviorHoldInits = do
  (_, !his) <- ask
  return his

{-# INLINE getDynHold #-}
getDynHold :: (Defer (SomeHoldInit x) m, Patch p) => Dyn x p -> m (Hold x p)
getDynHold d = do
  mh <- liftIO $ readIORef $ unDyn d
  case mh of
    HoldDyn h -> return h
    UnsafeDyn (readV0, v') -> do
      holdInits <- getDeferralQueue
      v0 <- liftIO $ runBehaviorM readV0 Nothing holdInits
      hold' v0 v'
    BuildDyn (readV0, v') -> do
      v0 <- liftIO $ runEventM readV0
      hold' v0 v'
  where
    hold' v0 v' = do
      h <- hold v0 v'
      liftIO $ writeIORef (unDyn d) $ HoldDyn h
      return h


-- Always refers to 0
{-# NOINLINE zeroRef #-}
zeroRef :: IORef Height
zeroRef = unsafePerformIO $ newIORef zeroHeight

getRootSubscribed :: forall k x a. (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Subscriber x a -> IO (WeakBagTicket, RootSubscribed x a, Maybe a)
getRootSubscribed k r sub = do
  mSubscribed <- readIORef $ rootSubscribed r
  let getOcc = fmap (coerce . DMap.lookup k) $ readIORef $ rootOccurrence r
  case DMap.lookup k mSubscribed of
    Just subscribed -> {-# SCC "hitRoot" #-} do
      sln <- subscribeRootSubscribed subscribed sub
      occ <- getOcc
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missRoot" #-} do
      weakSelf <- newIORef $ error "getRootSubscribed: weakSelfRef not initialized"
      let !cached = rootSubscribed r
      uninitRef <- newIORef $ error "getRootsubscribed: uninitRef not initialized"
      (subs, sln) <- WeakBag.singleton sub weakSelf cleanupRootSubscribed

      tracePropagate (Proxy::Proxy x) $  "getRootSubscribed: calling rootInit"

      uninit <- rootInit r k $ RootTrigger (subs, rootOccurrence r, k)
      writeIORef uninitRef $! uninit
#ifdef DEBUG_NODEIDS
      nid <- newNodeId
#endif
      let !subscribed = RootSubscribed
            { rootSubscribedKey = k
            , rootSubscribedCachedSubscribed = cached
            , rootSubscribedOccurrence = getOcc
            , rootSubscribedSubscribers = subs
            , rootSubscribedUninit = uninit
            , rootSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , rootSubscribedNodeId = nid
#endif
            }
          -- If we die at the same moment that all our children die, they will
          -- try to clean us up but will fail because their Weak reference to us
          -- will also be dead.  So, if we are dying, check if there are any
          -- children; since children don't bother cleaning themselves up if
          -- their parents are already dead, I don't think there's a race
          -- condition here.  However, if there are any children, then we can
          -- infer that we need to clean ourselves up, so we do.
          finalCleanup = do
            cs <- readIORef $ _weakBag_children subs
            when (not $ IntMap.null cs) (cleanupRootSubscribed subscribed)
      writeIORef weakSelf =<< evaluate =<< mkWeakPtr subscribed (Just finalCleanup)
      modifyIORef' (rootSubscribed r) $ DMap.insertWith (error $ "getRootSubscribed: duplicate key inserted into Root") k subscribed --TODO: I think we can just write back mSubscribed rather than re-reading it
      occ <- getOcc
      return (sln, subscribed, occ)

#ifdef USE_TEMPLATE_HASKELL
{-# ANN cleanupRootSubscribed "HLint: ignore Redundant bracket" #-}
#endif
cleanupRootSubscribed :: RootSubscribed x a -> IO ()
cleanupRootSubscribed self@RootSubscribed { rootSubscribedKey = k, rootSubscribedCachedSubscribed = cached } = do
  rootSubscribedUninit self
  modifyIORef' cached $ DMap.delete k

{-# INLINE subscribeRootSubscribed #-}
subscribeRootSubscribed :: RootSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeRootSubscribed subscribed sub = WeakBag.insert sub (rootSubscribedSubscribers subscribed) (rootSubscribedWeakSelf subscribed) cleanupRootSubscribed

newtype EventSelectorInt x a = EventSelectorInt { selectInt :: Int -> Event x a }

data FanInt x a = FanInt
  { _fanInt_subscribers :: {-# UNPACK #-} !(FastMutableIntMap (FastWeakBag (Subscriber x a))) --TODO: Clean up the keys in here when their child weak bags get empty --TODO: Remove our own subscription when the subscribers list is completely empty
  , _fanInt_subscriptionRef :: {-# UNPACK #-} !(IORef (EventSubscription x)) -- This should have a valid subscription iff subscribers is non-empty
  , _fanInt_occRef :: {-# UNPACK #-} !(IORef (IntMap a))
#ifdef DEBUG_NODEIDS
  , _fanInt_nodeId :: {-# UNPACK #-} !Int
#endif
  }

newFanInt :: IO (FanInt x a)
newFanInt = do
  subscribers <- FastMutableIntMap.newEmpty --TODO: Clean up the keys in here when their child weak bags get empty --TODO: Remove our own subscription when the subscribers list is completely empty
  subscriptionRef <- newIORef $ error "fanInt: no subscription"
  occRef <- newIORef $ error "fanInt: no occurrence"
#ifdef DEBUG_NODEIDS
  nodeId <- newNodeId
#endif
  return $ FanInt
    { _fanInt_subscribers = subscribers
    , _fanInt_subscriptionRef = subscriptionRef
    , _fanInt_occRef = occRef
#ifdef DEBUG_NODEIDS
    , _fanInt_nodeId = nodeId
#endif
    }

fanInt :: HasSpiderTimeline x => Event x (IntMap a) -> EventSelectorInt x a
fanInt p = unsafePerformIO $ do
  self <- newFanInt
  pure $ EventSelectorInt $ \k -> Event $ \sub -> do
    isEmpty <- liftIO $ FastMutableIntMap.isEmpty (_fanInt_subscribers self)
    when isEmpty $ do -- This is the first subscriber, so we need to subscribe to our input
      let desc = "fanInt" <> showNodeId self <> ", k = "  <> show k
      (subscription, parentOcc) <- subscribeAndRead p $ debugSubscriber' desc $ Subscriber
        { subscriberPropagate = \m -> do
            liftIO $ writeIORef (_fanInt_occRef self) m
            scheduleIntClear $ _fanInt_occRef self
            FastMutableIntMap.forIntersectionWithImmutable_ (_fanInt_subscribers self) m $ \b v ->  --TODO: Do we need to know that no subscribers are being added as we traverse?
              FastWeakBag.traverse_ b $ \s ->
                subscriberPropagate s v
        , subscriberInvalidateHeight = \old ->
            FastMutableIntMap.for_ (_fanInt_subscribers self) $ \b ->
              FastWeakBag.traverse_ b $ \s ->
                subscriberInvalidateHeight s old
        , subscriberRecalculateHeight = \new ->
            FastMutableIntMap.for_ (_fanInt_subscribers self) $ \b ->
              FastWeakBag.traverse_ b $ \s ->
                subscriberRecalculateHeight s new
        }
      liftIO $ do
        writeIORef (_fanInt_subscriptionRef self) subscription
        writeIORef (_fanInt_occRef self) $ fromMaybe IntMap.empty parentOcc
      scheduleIntClear $ _fanInt_occRef self
    liftIO $ do
      b <- FastMutableIntMap.lookup (_fanInt_subscribers self) k >>= \case
        Nothing -> do
          b <- FastWeakBag.empty
          FastMutableIntMap.insert (_fanInt_subscribers self) k b
          return b
        Just b -> return b
      ticket <- liftIO $ FastWeakBag.insert sub b
      currentOcc <- readIORef (_fanInt_occRef self)

      subscribed <- fanIntSubscribed ticket self
      return (EventSubscription (FastWeakBag.remove ticket) subscribed, IntMap.lookup k currentOcc)

fanIntSubscribed :: FastWeakBagTicket k -> FanInt x a -> IO (EventSubscribed x)
fanIntSubscribed ticket self = do
  subscribedParent <- _eventSubscription_subscribed <$> readIORef (_fanInt_subscriptionRef self)
  return $ EventSubscribed
    { eventSubscribedHeightRef = eventSubscribedHeightRef subscribedParent
    , eventSubscribedRetained = toAny (_fanInt_subscriptionRef self, ticket)
#ifdef DEBUG_CYCLES
    , eventSubscribedGetParents = return [subscribedParent]
    , eventSubscribedHasOwnHeightRef = False
    , eventSubscribedWhoCreated = whoCreatedIORef $ _fanInt_subscriptionRef self
#endif
    }


{-# INLINABLE getFanSubscribed #-}
getFanSubscribed :: (HasSpiderTimeline x, GCompare k) => k a -> Fan x k v -> Subscriber x (v a) -> EventM x (WeakBagTicket, FanSubscribed x k v, Maybe (v a))
getFanSubscribed k f sub = do
  mSubscribed <- liftIO $ readIORef $ fanSubscribed f
  case mSubscribed of
    Just subscribed -> {-# SCC "hitFan" #-} liftIO $ do
      sln <- subscribeFanSubscribed k subscribed sub
      occ <- readIORef $ fanSubscribedOccurrence subscribed
      return (sln, subscribed, coerce $ DMap.lookup k =<< occ)
    Nothing -> {-# SCC "missFan" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getFanSubscribed: subscribedRef not yet initialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      s <- liftIO $ newSubscriberFan subscribedUnsafe
      (subscription, parentOcc) <- subscribeAndRead (fanParent f) s
      weakSelf <- liftIO $ newIORef $ error "getFanSubscribed: weakSelf not yet initialized"
      (subsForK, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupFanSubscribed
      subscribersRef <- liftIO $ newIORef $ error "getFanSubscribed: subscribersRef not yet initialized"
      occRef <- liftIO $ newIORef parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
#ifdef DEBUG_NODEIDS
      nid <- liftIO newNodeId
#endif
      let subscribed = FanSubscribed
            { fanSubscribedCachedSubscribed = fanSubscribed f
            , fanSubscribedOccurrence = occRef
            , fanSubscribedParent = subscription
            , fanSubscribedSubscribers = subscribersRef
#ifdef DEBUG_NODEIDS
            , fanSubscribedNodeId = nid
#endif
            }
      let !self = (k, subscribed)
      liftIO $ writeIORef subscribersRef $! DMap.singleton k $ FanSubscribedChildren subsForK self weakSelf
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug self "FanSubscribed"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (fanSubscribed f) $ Just subscribed
      return (slnForSub, subscribed, coerce $ DMap.lookup k =<< parentOcc)

cleanupFanSubscribed :: GCompare k => (k a, FanSubscribed x k v) -> IO ()
cleanupFanSubscribed (k, subscribed) = do
  subscribers <- readIORef $ fanSubscribedSubscribers subscribed
  let reducedSubscribers = DMap.delete k subscribers
  if DMap.null reducedSubscribers
    then do
      unsubscribe $ fanSubscribedParent subscribed
      -- Not necessary in this case, because this whole FanSubscribed is dead: writeIORef (fanSubscribedSubscribers subscribed) reducedSubscribers
      writeIORef (fanSubscribedCachedSubscribed subscribed) Nothing
    else writeIORef (fanSubscribedSubscribers subscribed) $! reducedSubscribers

{-# INLINE subscribeFanSubscribed #-}
subscribeFanSubscribed :: GCompare k => k a -> FanSubscribed x k v -> Subscriber x (v a) -> IO WeakBagTicket
subscribeFanSubscribed k subscribed sub = do
  subscribers <- readIORef $ fanSubscribedSubscribers subscribed
  case DMap.lookup k subscribers of
    Nothing -> {-# SCC "missSubscribeFanSubscribed" #-} do
      let !self = (k, subscribed)
      weakSelf <- newIORef =<< mkWeakPtrWithDebug self "FanSubscribed"
      (list, sln) <- WeakBag.singleton sub weakSelf cleanupFanSubscribed
      writeIORef (fanSubscribedSubscribers subscribed) $! DMap.insertWith (error "subscribeFanSubscribed: key that we just failed to find is present - should be impossible") k (FanSubscribedChildren list self weakSelf) subscribers
      return sln
    Just (FanSubscribedChildren list _ weakSelf) -> {-# SCC "hitSubscribeFanSubscribed" #-} WeakBag.insert sub list weakSelf cleanupFanSubscribed

{-# INLINABLE getSwitchSubscribed #-}
getSwitchSubscribed :: HasSpiderTimeline x => Switch x a -> Subscriber x a -> EventM x (WeakBagTicket, SwitchSubscribed x a, Maybe a)
getSwitchSubscribed s sub = do
  mSubscribed <- liftIO $ readIORef $ switchSubscribed s
  case mSubscribed of
    Just subscribed -> {-# SCC "hitSwitch" #-} liftIO $ do
      sln <- subscribeSwitchSubscribed subscribed sub
      occ <- readIORef $ switchSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missSwitch" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getSwitchSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      i <- liftIO $ newInvalidatorSwitch subscribedUnsafe
      mySub <- liftIO $ newSubscriberSwitch subscribedUnsafe
      wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorSwitch"
      wiRef <- liftIO $ newIORef wi
      parentsRef <- liftIO $ newIORef [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
      holdInits <- getDeferralQueue
      e <- liftIO $ runBehaviorM (readBehaviorTracked (switchParent s)) (Just (wi, parentsRef)) holdInits
      (subscription@(EventSubscription _ subd), parentOcc) <- subscribeAndRead e mySub
      heightRef <- liftIO $ newIORef =<< getEventSubscribedHeight subd
      subscriptionRef <- liftIO $ newIORef subscription
      occRef <- liftIO $ newIORef parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      weakSelf <- liftIO $ newIORef $ error "getSwitchSubscribed: weakSelf not yet initialized"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupSwitchSubscribed
#ifdef DEBUG_NODEIDS
      nid <- liftIO newNodeId
#endif
      let !subscribed = SwitchSubscribed
            { switchSubscribedCachedSubscribed = switchSubscribed s
            , switchSubscribedOccurrence = occRef
            , switchSubscribedHeight = heightRef
            , switchSubscribedSubscribers = subs
            , switchSubscribedOwnInvalidator = i
            , switchSubscribedOwnWeakInvalidator = wiRef
            , switchSubscribedBehaviorParents = parentsRef
            , switchSubscribedParent = switchParent s
            , switchSubscribedCurrentParent = subscriptionRef
            , switchSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , switchSubscribedNodeId = nid
#endif
            }
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "switchSubscribedWeakSelf"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (switchSubscribed s) $ Just subscribed
      return (slnForSub, subscribed, parentOcc)

cleanupSwitchSubscribed :: SwitchSubscribed x a -> IO ()
cleanupSwitchSubscribed subscribed = do
  unsubscribe =<< readIORef (switchSubscribedCurrentParent subscribed)
  finalize =<< readIORef (switchSubscribedOwnWeakInvalidator subscribed) -- We don't need to get invalidated if we're dead
  writeIORef (switchSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeSwitchSubscribed #-}
subscribeSwitchSubscribed :: SwitchSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeSwitchSubscribed subscribed sub = WeakBag.insert sub (switchSubscribedSubscribers subscribed) (switchSubscribedWeakSelf subscribed) cleanupSwitchSubscribed

{-# INLINABLE getCoincidenceSubscribed #-}
getCoincidenceSubscribed :: forall x a. HasSpiderTimeline x => Coincidence x a -> Subscriber x a -> EventM x (WeakBagTicket, CoincidenceSubscribed x a, Maybe a)
getCoincidenceSubscribed c sub = do
  mSubscribed <- liftIO $ readIORef $ coincidenceSubscribed c
  case mSubscribed of
    Just subscribed -> {-# SCC "hitCoincidence" #-} liftIO $ do
      sln <- subscribeCoincidenceSubscribed subscribed sub
      occ <- readIORef $ coincidenceSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missCoincidence" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getCoincidenceSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      subOuter <- liftIO $ newSubscriberCoincidenceOuter subscribedUnsafe
      (outerSubscription@(EventSubscription _ outerSubd), outerOcc) <- subscribeAndRead (coincidenceParent c) subOuter
      outerHeight <- liftIO $ getEventSubscribedHeight outerSubd
      (occ, height, mInnerSubd) <- case outerOcc of
        Nothing -> return (Nothing, outerHeight, Nothing)
        Just o -> do
          (occ, height, innerSubd) <- subscribeCoincidenceInner o outerHeight subscribedUnsafe
          return (occ, height, Just innerSubd)
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      heightRef <- liftIO $ newIORef height
      innerSubdRef <- liftIO $ newIORef mInnerSubd
      scheduleClear innerSubdRef
      weakSelf <- liftIO $ newIORef $ error "getCoincidenceSubscribed: weakSelf not yet implemented"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupCoincidenceSubscribed
#ifdef DEBUG_NODEIDS
      nid <- liftIO newNodeId
#endif
      let subscribed = CoincidenceSubscribed
            { coincidenceSubscribedCachedSubscribed = coincidenceSubscribed c
            , coincidenceSubscribedOccurrence = occRef
            , coincidenceSubscribedHeight = heightRef
            , coincidenceSubscribedSubscribers = subs
            , coincidenceSubscribedOuter = subOuter
            , coincidenceSubscribedOuterParent = outerSubscription
            , coincidenceSubscribedInnerParent = innerSubdRef
            , coincidenceSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , coincidenceSubscribedNodeId = nid
#endif
            }
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "CoincidenceSubscribed"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (coincidenceSubscribed c) $ Just subscribed
      return (slnForSub, subscribed, occ)

cleanupCoincidenceSubscribed :: CoincidenceSubscribed x a -> IO ()
cleanupCoincidenceSubscribed subscribed = do
  unsubscribe $ coincidenceSubscribedOuterParent subscribed
  writeIORef (coincidenceSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeCoincidenceSubscribed #-}
subscribeCoincidenceSubscribed :: CoincidenceSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeCoincidenceSubscribed subscribed sub = WeakBag.insert sub (coincidenceSubscribedSubscribers subscribed) (coincidenceSubscribedWeakSelf subscribed) cleanupCoincidenceSubscribed

{-# INLINE mergeG #-}
mergeG :: forall k q x v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q) -> Event x (DMap k v)
mergeG nt d = cacheEvent (mergeCheap nt d)

{-# INLINE mergeWithMove #-}
mergeWithMove :: forall k v q x. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q) -> Event x (DMap k v)
mergeWithMove nt d = cacheEvent (mergeCheapWithMove nt d)

{-# INLINE [1] mergeCheap #-}
mergeCheap
  :: forall k x q v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q)
  -> Event x (DMap k v)
mergeCheap nt = mergeGCheap' unMergeSubscribedParent getInitialSubscribers updateMe destroy
  where
      updateMe :: MergeUpdateFunc k v x (PatchDMap k q) (MergeSubscribedParent x)
      updateMe subscriber heightBagRef oldParents (PatchDMap p) = do
        let f (subscriptionsToKill, ps) (k :=> ComposeMaybe me) = do
              (mOldSubd, newPs) <- case me of
                Nothing -> return $ DMap.updateLookupWithKey (\_ _ -> Nothing) k ps
                Just e -> do
                  let s = subscriber $ return k
                  subscription@(EventSubscription _ subd) <- subscribe (nt e) s
                  newParentHeight <- liftIO $ getEventSubscribedHeight subd
                  let newParent = MergeSubscribedParent subscription
                  liftIO $ modifyIORef' heightBagRef $ heightBagAdd newParentHeight
                  return $ DMap.insertLookupWithKey' (\_ new _ -> new) k newParent ps
              forM_ mOldSubd $ \oldSubd -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $
                  _eventSubscription_subscribed $ unMergeSubscribedParent oldSubd

                liftIO $ modifyIORef heightBagRef $ heightBagRemove oldHeight
              return (maybeToList (unMergeSubscribedParent <$> mOldSubd) ++ subscriptionsToKill, newPs)
        foldM f ([], oldParents) $ DMap.toList p

      getInitialSubscribers :: MergeInitFunc k v q x (MergeSubscribedParent x)
      getInitialSubscribers initialParents subscriber = do
        subscribers <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
          let s = subscriber $ return k
          (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead (nt e) s
          height <- liftIO $ getEventSubscribedHeight parentSubd
          return (fmap (k :=>) parentOcc, height, k :=> MergeSubscribedParent subscription)
        return ( DMap.fromDistinctAscList $ mapMaybe (\(x, _, _) -> x) subscribers
               , fmap (\(_, h, _) -> h) subscribers --TODO: Assert that there's no invalidHeight in here
               , DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
               )

      destroy :: MergeDestroyFunc k (MergeSubscribedParent x)
      destroy s = forM_ (DMap.toList s) $ \(_ :=> MergeSubscribedParent sub) -> unsubscribe sub

{-# INLINE [1] mergeCheapWithMove #-}
mergeCheapWithMove :: forall k x v q. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q)
  -> Event x (DMap k v)
mergeCheapWithMove nt = mergeGCheap' _mergeSubscribedParentWithMove_subscription getInitialSubscribers updateMe destroy
  where
      updateMe :: MergeUpdateFunc k v x (PatchDMapWithMove k q) (MergeSubscribedParentWithMove x k)
      updateMe subscriber heightBagRef oldParents p = do
        -- Prepare new parents for insertion
        let subscribeParent :: forall a. k a -> Event x (v a) -> EventM x (MergeSubscribedParentWithMove x k a)
            subscribeParent k e = do
              keyRef <- liftIO $ newIORef k
              let s = subscriber $ liftIO $ readIORef keyRef
              subscription@(EventSubscription _ subd) <- subscribe e s
              liftIO $ do
                newParentHeight <- getEventSubscribedHeight subd
                modifyIORef' heightBagRef $ heightBagAdd newParentHeight
                return $ MergeSubscribedParentWithMove subscription keyRef
        p' <- PatchDMapWithMove.traversePatchDMapWithMoveWithKey (\k q -> subscribeParent k (nt q)) p
        -- Collect old parents for deletion and update the keys of moved parents
        let moveOrDelete :: forall a. k a -> PatchDMapWithMove.NodeInfo k q a -> MergeSubscribedParentWithMove x k a -> Constant (EventM x (Maybe (EventSubscription x))) a
            moveOrDelete _ ni parent = Constant $ case getComposeMaybe $ PatchDMapWithMove._nodeInfo_to ni of
              Nothing -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $ _eventSubscription_subscribed $
                  _mergeSubscribedParentWithMove_subscription parent

                liftIO $ modifyIORef heightBagRef $ heightBagRemove oldHeight
                return $ Just $ _mergeSubscribedParentWithMove_subscription parent
              Just toKey -> do
                liftIO $ writeIORef (_mergeSubscribedParentWithMove_key parent) $! toKey
                return Nothing
        toDelete <- fmap catMaybes $ mapM (\(_ :=> v) -> getConstant v) $ DMap.toList $
          DMap.intersectionWithKey moveOrDelete (unPatchDMapWithMove p) oldParents

        return (toDelete, applyAlways p' oldParents)
      getInitialSubscribers :: MergeInitFunc k v q x (MergeSubscribedParentWithMove x k)
      getInitialSubscribers initialParents subscriber = do
        subscribers <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
          keyRef <- liftIO $ newIORef k
          let s = subscriber $ liftIO $ readIORef keyRef
          (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead (nt e) s
          height <- liftIO $ getEventSubscribedHeight parentSubd
          return (fmap (k :=>) parentOcc, height, k :=> MergeSubscribedParentWithMove subscription keyRef)
        return ( DMap.fromDistinctAscList $ mapMaybe (\(x, _, _) -> x) subscribers
               , fmap (\(_, h, _) -> h) subscribers --TODO: Assert that there's no invalidHeight in here
               , DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
               )

      destroy :: MergeDestroyFunc k (MergeSubscribedParentWithMove x k)
      destroy s = forM_ (DMap.toList s) $ \(_ :=> MergeSubscribedParentWithMove sub _) -> unsubscribe sub

type MergeUpdateFunc k v x p s
   = (forall a. EventM x (k a) -> Subscriber x (v a))
  -> IORef HeightBag
  -> DMap k s
  -> p
  -> EventM x ([EventSubscription x], DMap k s)

type MergeGetSubscription x s = forall a. s a -> EventSubscription x

type MergeInitFunc k v q x s
   = DMap k q
  -> (forall a. EventM x (k a) -> Subscriber x (v a))
  -> EventM x (DMap k v, [Height], DMap k s)

type MergeDestroyFunc k s
   = DMap k s
  -> IO ()

data Merge x k v s = Merge
  { _merge_parentsRef :: {-# UNPACK #-} !(IORef (DMap k s))
  , _merge_heightBagRef :: {-# UNPACK #-} !(IORef HeightBag)
  , _merge_heightRef :: {-# UNPACK #-} !(IORef Height)
  , _merge_sub :: {-# UNPACK #-} !(Subscriber x (DMap k v))
  , _merge_accumRef :: {-# UNPACK #-} !(IORef (DMap k v))
  }

invalidateMergeHeight :: Merge x k v s -> IO ()
invalidateMergeHeight m = invalidateMergeHeight' (_merge_heightRef m) (_merge_sub m)

invalidateMergeHeight' :: IORef Height -> Subscriber x a -> IO ()
invalidateMergeHeight' heightRef sub = do
  oldHeight <- readIORef heightRef
  -- If the height used to be valid, it must be invalid now; we should never have *more* heights than we have parents
  when (oldHeight /= invalidHeight) $ do
    writeIORef heightRef $! invalidHeight
    subscriberInvalidateHeight sub oldHeight

revalidateMergeHeight :: Merge x k v s -> IO ()
revalidateMergeHeight m = do
  currentHeight <- readIORef $ _merge_heightRef m

  -- revalidateMergeHeight may be called multiple times; perhaps the's a way to finesse it to avoid this check
  when (currentHeight == invalidHeight) $ do
    heights <- readIORef $ _merge_heightBagRef m
    parents <- readIORef $ _merge_parentsRef m
    -- When the number of heights in the bag reaches the number of parents, we should have a valid height
    case heightBagSize heights `compare` DMap.size parents of
      LT -> return ()
      EQ -> do
        let height = succHeight $ heightBagMax heights
        traceInvalidateHeight $ "recalculateSubscriberHeight: height: " <> show height
        writeIORef (_merge_heightRef m) $! height
        subscriberRecalculateHeight (_merge_sub m) height
      GT -> error $ "revalidateMergeHeight: more heights (" <> show (heightBagSize heights) <> ") than parents (" <> show (DMap.size parents) <> ") for Merge"

scheduleMergeSelf :: HasSpiderTimeline x => Merge x k v s -> Height -> EventM x ()
scheduleMergeSelf m height = scheduleMerge' height (_merge_heightRef m) $ do
  vals <- liftIO $ readIORef $ _merge_accumRef m
  -- Once we're done with this, we can clear it immediately, because if there's a cacheEvent in front of us,
  -- it'll handle subsequent subscribers, and if not, we won't get subsequent subscribers
  liftIO $ writeIORef (_merge_accumRef m) $! DMap.empty
  --TODO: Assert that m is not empty
  subscriberPropagate (_merge_sub m) vals

checkCycle :: EventSubscribed x -> EventM x ()
checkCycle subscribed = liftIO $ do
    height <- readIORef (eventSubscribedHeightRef subscribed)

    -- currentHeight <- getCurrentHeight
    -- when (height <= currentHeight) $ if height /= invalidHeight
    --     then do
    --       myStack <- liftIO $ whoCreatedIORef undefined --TODO
    --       error $ "Height (" ++ show height ++ ") is not greater than current height (" ++ show currentHeight ++ ")\n" ++ unlines (reverse myStack)
    --     else liftIO $
    when (height == invalidHeight) $
#ifdef DEBUG_CYCLES
          do
            nodesInvolvedInCycle <- walkInvalidHeightParents subscribed
            stacks <- forM nodesInvolvedInCycle whoCreatedEventSubscribed
            throwIO (EventLoopException stacks)
#else
          throwIO EventLoopException
#endif


mergeSubscriber :: forall x k v s a. (HasSpiderTimeline x, GCompare k) => EventSubscribed x -> Merge x k v s -> EventM x (k a) -> Subscriber x (v a)
mergeSubscriber subscribed m getKey = Subscriber
  { subscriberPropagate = \a -> do
      oldM <- liftIO $ readIORef $ _merge_accumRef m
      k <- getKey
      let newM = DMap.insertWith (error "Same key fired multiple times for Merge") k a oldM
      tracePropagate (Proxy :: Proxy x) $ "  DMap.size oldM = " <> show (DMap.size oldM) <> "; DMap.size newM = " <> show (DMap.size newM)
      liftIO $ writeIORef (_merge_accumRef m) $! newM
      when (DMap.null oldM) $ do -- Only schedule the firing once
        height <- liftIO $ readIORef $ _merge_heightRef m
        checkCycle subscribed

        scheduleMergeSelf m height
  , subscriberInvalidateHeight = \old -> do --TODO: When removing a parent doesn't actually change the height, maybe we can avoid invalidating
      modifyIORef' (_merge_heightBagRef m) $ heightBagRemove old
      invalidateMergeHeight m
  , subscriberRecalculateHeight = \new -> do
      modifyIORef' (_merge_heightBagRef m) $ heightBagAdd new
      revalidateMergeHeight m
  }

--TODO: Be able to run as much of this as possible promptly
updateMerge :: (HasSpiderTimeline x, GCompare k) => EventSubscribed x -> Merge x k v s -> MergeUpdateFunc k v x p s -> p -> SomeMergeUpdate x
updateMerge subscribed m updateFunc p = SomeMergeUpdate updateMe (invalidateMergeHeight m) (revalidateMergeHeight m)
  where updateMe = do
          oldParents <- liftIO $ readIORef $ _merge_parentsRef m
          (subscriptionsToKill, newParents) <- updateFunc (mergeSubscriber subscribed m) (_merge_heightBagRef m) oldParents p
          liftIO $ writeIORef (_merge_parentsRef m) $! newParents
          return subscriptionsToKill

{-# INLINE mergeGCheap' #-}
mergeGCheap' :: forall k v x p s q. (HasSpiderTimeline x, GCompare k, PatchTarget p ~ DMap k q)
  => MergeGetSubscription x s -> MergeInitFunc k v q x s -> MergeUpdateFunc k v x p s -> MergeDestroyFunc k s -> DynamicS x p -> Event x (DMap k v)
mergeGCheap' getParent getInitialSubscribers updateFunc destroy d = Event $ \sub -> do
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accumRef <- liftIO $ newIORef $ error "merge: accumRef not yet initialized"
  heightRef <- liftIO $ newIORef $ error "merge: heightRef not yet initialized"
  heightBagRef <- liftIO $ newIORef $ error "merge: heightBagRef not yet initialized"
  parentsRef :: IORef (DMap k s) <- liftIO $ newIORef $ error "merge: parentsRef not yet initialized"
  changeSubdRef <- liftIO $ newIORef $ error "getMergeSubscribed: changeSubdRef not yet initialized"

  let subscribed = EventSubscribed
        { eventSubscribedHeightRef = heightRef
        , eventSubscribedRetained = toAny (parentsRef, changeSubdRef)
#ifdef DEBUG_CYCLES
      , eventSubscribedGetParents = do
          let getParent' (_ :=> v) = _eventSubscription_subscribed (getParent v)
          fmap getParent' . DMap.toList  <$> readIORef parentsRef
      , eventSubscribedHasOwnHeightRef = False
      , eventSubscribedWhoCreated = whoCreatedIORef heightRef
#endif
      }

      m = Merge
        { _merge_parentsRef = parentsRef
        , _merge_heightBagRef = heightBagRef
        , _merge_heightRef = heightRef
        , _merge_sub = sub
        , _merge_accumRef = accumRef
        }
  (dm, heights, initialParentState) <- getInitialSubscribers initialParents $ mergeSubscriber subscribed m
  let myHeightBag = heightBagFromList $ filter (/= invalidHeight) heights
      myHeight = if invalidHeight `elem` heights
                 then invalidHeight
                 else succHeight $ heightBagMax myHeightBag
  currentHeight <- getCurrentHeight
  let (occ, accum) = if currentHeight >= myHeight -- If we should have fired by now
                     then (if DMap.null dm then Nothing else Just dm, DMap.empty)
                     else (Nothing, dm)
  unless (DMap.null accum) $ scheduleMergeSelf m myHeight
  liftIO $ writeIORef accumRef $! accum
  liftIO $ writeIORef heightRef $! myHeight
  liftIO $ writeIORef heightBagRef $! myHeightBag
  liftIO $ writeIORef parentsRef $! initialParentState
  defer $ SomeMergeInit $ do
    let changeSubscriber = Subscriber
          { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
              tracePropagate (Proxy :: Proxy x) "SubscriberMerge/Change"
              defer $ updateMerge subscribed m updateFunc a
          , subscriberInvalidateHeight = \_ -> return ()
          , subscriberRecalculateHeight = \_ -> return ()
          }
    (changeSubscription, change) <- subscribeAndRead (dynamicUpdated d) changeSubscriber
    forM_ change $ \c -> defer $ updateMerge subscribed m updateFunc c
    -- We explicitly hold on to the unsubscribe function from subscribing to the update event.
    -- If we don't do this, there are certain cases where mergeCheap will fail to properly retain
    -- its subscription.
    liftIO $ writeIORef changeSubdRef (changeSubscriber, changeSubscription)
  let unsubscribeAll = destroy =<< readIORef parentsRef

  return (EventSubscription unsubscribeAll subscribed, occ)


mergeInt :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeInt = cacheEvent . mergeIntCheap

{-# INLINABLE mergeIntCheap #-}
mergeIntCheap :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeIntCheap d = Event $ \sub -> do
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accum <- liftIO $ FastMutableIntMap.newEmpty
  heightRef <- liftIO $ newIORef zeroHeight
  heightBagRef <- liftIO $ newIORef heightBagEmpty
  parents <- liftIO $ FastMutableIntMap.newEmpty
  changeSubdRef <- liftIO $ newIORef $ error "getMergeSubscribed: changeSubdRef not yet initialized"
  let subscribed = EventSubscribed
        { eventSubscribedHeightRef = heightRef
        , eventSubscribedRetained = toAny (parents, changeSubdRef)
#ifdef DEBUG_CYCLES
        , eventSubscribedGetParents = fmap (_eventSubscription_subscribed . snd) <$> FastMutableIntMap.toList parents
        , eventSubscribedHasOwnHeightRef = False
        , eventSubscribedWhoCreated = whoCreatedIORef heightRef
#endif
        }
  let scheduleSelf = do
        height <- liftIO $ readIORef $ heightRef
        scheduleMerge' height heightRef $ do
          vals <- liftIO $ FastMutableIntMap.getFrozenAndClear accum
          subscriberPropagate sub vals
      invalidateMyHeight = invalidateMergeHeight' heightRef sub
      recalculateMyHeight = do
        currentHeight <- readIORef heightRef
        when (currentHeight == invalidHeight) $ do --TODO: This will almost always be true; can we get rid of this check and just proceed to the next one always?
          heights <- readIORef heightBagRef
          numParents <- FastMutableIntMap.size parents
          case heightBagSize heights `compare` numParents of
            LT -> return ()
            EQ -> do
              let height = succHeight $ heightBagMax heights
              traceInvalidateHeight $ "recalculateSubscriberHeight: height: " <> show height
              writeIORef heightRef $! height
              subscriberRecalculateHeight sub height
            GT -> error $ "revalidateMergeHeight: more heights (" <> show (heightBagSize heights) <> ") than parents (" <> show numParents <> ") for Merge"
      mySubscriber k = Subscriber
        { subscriberPropagate = \a -> do
            checkCycle subscribed

            wasEmpty <- liftIO $ FastMutableIntMap.isEmpty accum
            liftIO $ FastMutableIntMap.insert accum k a
            when wasEmpty scheduleSelf
        , subscriberInvalidateHeight = \old -> do
            modifyIORef' heightBagRef $ heightBagRemove old
            invalidateMyHeight
        , subscriberRecalculateHeight = \new -> do
            modifyIORef' heightBagRef $ heightBagAdd new
            recalculateMyHeight
        }
  forM_ (IntMap.toList initialParents) $ \(k, p) -> do
    (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead p $ mySubscriber k
    liftIO $ do
      forM_ parentOcc $ FastMutableIntMap.insert accum k
      FastMutableIntMap.insert parents k subscription
      height <- getEventSubscribedHeight parentSubd
      if height == invalidHeight
        then writeIORef heightRef invalidHeight
        else do
          modifyIORef' heightBagRef $ heightBagAdd height
          modifyIORef' heightRef $ \oldHeight ->
            if oldHeight == invalidHeight
            then invalidHeight
            else max (succHeight height) oldHeight
  myHeight <- liftIO $ readIORef heightRef
  currentHeight <- getCurrentHeight
  isEmpty <- liftIO $ FastMutableIntMap.isEmpty accum
  occ <- if currentHeight >= myHeight -- If we should have fired by now
    then if isEmpty
         then return Nothing
         else liftIO $ Just <$> FastMutableIntMap.getFrozenAndClear accum
    else do when (not isEmpty) scheduleSelf -- We have things accumulated, but we shouldn't have fired them yet
            return Nothing
  defer $ SomeMergeInit $ do
    let updateMe a = SomeMergeUpdate u invalidateMyHeight recalculateMyHeight
          where
            u = do
              let f k newParent = do
                    subscription@(EventSubscription _ subd) <- subscribe newParent $ mySubscriber k
                    newParentHeight <- liftIO $ getEventSubscribedHeight subd
                    liftIO $ modifyIORef' heightBagRef $ heightBagAdd newParentHeight
                    return subscription
              newSubscriptions <- FastMutableIntMap.traverseIntMapPatchWithKey f a
              oldParents <- liftIO $ FastMutableIntMap.applyPatch parents newSubscriptions
              liftIO $ for_ oldParents $ \oldParent -> do
                oldParentHeight <- getEventSubscribedHeight $ _eventSubscription_subscribed oldParent

                print ("updateMe", oldParentHeight)
                modifyIORef' heightBagRef $ heightBagRemove oldParentHeight
              return $ IntMap.elems oldParents
    let changeSubscriber = Subscriber
          { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
              tracePropagate (Proxy :: Proxy x) $ "SubscriberMergeInt/Change"
              defer $ updateMe a
          , subscriberInvalidateHeight = \_ -> return ()
          , subscriberRecalculateHeight = \_ -> return ()
          }
    (changeSubscription, change) <- subscribeAndRead (dynamicUpdated d) changeSubscriber
    forM_ change $ \c -> defer $ updateMe c
    -- We explicitly hold on to the unsubscribe function from subscribing to the update event.
    -- If we don't do this, there are certain cases where mergeCheap will fail to properly retain
    -- its subscription.
    liftIO $ writeIORef changeSubdRef (changeSubscriber, changeSubscription)
  let unsubscribeAll = traverse_ unsubscribe =<< FastMutableIntMap.getFrozenAndClear parents


  return (EventSubscription unsubscribeAll subscribed, occ)

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }
newtype EventSelectorG x k v = EventSelectorG { selectG :: forall a. k a -> Event x (v a) }

fanG :: (HasSpiderTimeline x, GCompare k) => Event x (DMap k v) -> EventSelectorG x k v
fanG e = unsafePerformIO $ do
  ref <- newIORef Nothing
  let f = Fan
        { fanParent = e
        , fanSubscribed = ref
        }
  pure $ EventSelectorG $ \k -> eventFan k f

runHoldInits :: HasSpiderTimeline x => IORef [SomeHoldInit x] -> IORef [SomeDynInit x] -> IORef [SomeMergeInit x] -> EventM x ()
runHoldInits holdInitRef dynInitRef mergeInitRef = do
  holdInits <- liftIO $ readIORef holdInitRef
  dynInits <- liftIO $ readIORef dynInitRef
  mergeInits <- liftIO $ readIORef mergeInitRef
  unless (null holdInits && null dynInits && null mergeInits) $ do
    liftIO $ writeIORef holdInitRef []
    liftIO $ writeIORef dynInitRef []
    liftIO $ writeIORef mergeInitRef []
    mapM_ initHold holdInits
    mapM_ initDyn dynInits
    mapM_ unSomeMergeInit mergeInits
    runHoldInits holdInitRef dynInitRef mergeInitRef

initHold :: HasSpiderTimeline x => SomeHoldInit x -> EventM x ()
initHold (SomeHoldInit h) = void $ getHoldEventSubscription h

initDyn :: HasSpiderTimeline x => SomeDynInit x -> EventM x ()
initDyn (SomeDynInit d) = void $ getDynHold d

newEventEnv :: IO (EventEnv x)
newEventEnv = do
  toAssignRef <- newIORef [] -- This should only actually get used when events are firing
  holdInitRef <- newIORef []
  dynInitRef <- newIORef []
  mergeUpdateRef <- newIORef []
  mergeInitRef <- newIORef []
  heightRef <- newIORef zeroHeight
  toClearRef <- newIORef []
  toClearIntRef <- newIORef []
  toClearRootRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  delayedRef <- newIORef IntMap.empty
  return $ EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearIntRef toClearRootRef heightRef coincidenceInfosRef delayedRef

clearEventEnv :: EventEnv x -> IO ()
clearEventEnv (EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearIntRef toClearRootRef heightRef coincidenceInfosRef delayedRef) = do
  writeIORef toAssignRef []
  writeIORef holdInitRef []
  writeIORef dynInitRef []
  writeIORef mergeUpdateRef []
  writeIORef mergeInitRef []
  writeIORef heightRef zeroHeight
  writeIORef toClearRef []
  writeIORef toClearIntRef []
  writeIORef toClearRootRef []
  writeIORef coincidenceInfosRef []
  writeIORef delayedRef IntMap.empty

-- | Run an event action outside of a frame
runFrame :: forall x a. HasSpiderTimeline x => EventM x a -> SpiderHost x a --TODO: This function also needs to hold the mutex
runFrame a = SpiderHost $ do
  let env = _spiderTimeline_eventEnv $ unSTE (spiderTimeline :: SpiderTimelineEnv x)
  let go = do
        result <- a
        runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
        return result
  result <- runEventM go
  toClear <- readIORef $ eventEnvClears env
  forM_ toClear $ \(Some (Clear ref)) -> {-# SCC "clear" #-} writeIORef ref Nothing
  toClearInt <- readIORef $ eventEnvIntClears env
  forM_ toClearInt $ \(Some (IntClear ref)) -> {-# SCC "intClear" #-} writeIORef ref $! IntMap.empty
  toClearRoot <- readIORef $ eventEnvRootClears env
  forM_ toClearRoot $ \(Some (RootClear ref)) -> {-# SCC "rootClear" #-} writeIORef ref $! DMap.empty
  toAssign <- readIORef $ eventEnvAssignments env
  toReconnectRef <- newIORef []
  coincidenceInfos <- readIORef $ eventEnvResetCoincidences env
  forM_ toAssign $ \(SomeAssignment vRef iRef v) -> {-# SCC "assignment" #-} do
    writeIORef vRef v
    traceInvalidate $ "Invalidating Hold"
    writeIORef iRef =<< evaluate =<< invalidate toReconnectRef =<< readIORef iRef
  mergeUpdates <- readIORef $ eventEnvMergeUpdates env
  writeIORef (eventEnvMergeUpdates env) []
  tracePropagate (Proxy::Proxy x) $ "Updating merges"
  mergeSubscriptionsToKill <- runEventM $ concat <$> mapM _someMergeUpdate_update mergeUpdates
  tracePropagate (Proxy::Proxy x) $ "Updating merges done"
  toReconnect <- readIORef toReconnectRef
  clearEventEnv env
  switchSubscriptionsToKill <- forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    oldSubscription <- readIORef $ switchSubscribedCurrentParent subscribed
    wi <- readIORef $ switchSubscribedOwnWeakInvalidator subscribed
    traceInvalidate $ "Finalizing invalidator for Switch" <> showNodeId subscribed
    finalize wi
    i <- evaluate $ switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeIORef (switchSubscribedOwnWeakInvalidator subscribed) $! wi'
    writeIORef (switchSubscribedBehaviorParents subscribed) []
    writeIORef (eventEnvHoldInits env) [] --TODO: Should we reuse this?
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed)) $ eventEnvHoldInits env
    runEventM $ runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) --TODO: Is this actually OK? It seems like it should be, since we know that no events are firing at this point, but it still seems inelegant
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    sub <- newSubscriberSwitch subscribed
    subscription <- unSpiderHost $ runFrame $ {-# SCC "subscribeSwitch" #-} subscribe e sub --TODO: Assert that the event isn't firing --TODO: This should not loop because none of the events should be firing, but still, it is inefficient
    {-
    stackTrace <- liftIO $ fmap renderStack $ ccsToStrings =<< (getCCSOf $! switchSubscribedParent subscribed)
    liftIO $ debugStrLn $ (++stackTrace) $ "subd' subscribed to " ++ case e of
      EventRoot _ -> "EventRoot"
      EventNever -> "EventNever"
      _ -> "something else"
    -}
    writeIORef (switchSubscribedCurrentParent subscribed) $! subscription
    return oldSubscription
  liftIO $ mapM_ unsubscribe mergeSubscriptionsToKill
  liftIO $ mapM_ unsubscribe switchSubscriptionsToKill
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    EventSubscription _ subd' <- readIORef $ switchSubscribedCurrentParent subscribed
    parentHeight <- getEventSubscribedHeight subd'
    myHeight <- readIORef $ switchSubscribedHeight subscribed
    when (parentHeight /= myHeight) $ do
      writeIORef (switchSubscribedHeight subscribed) $! invalidHeight
      WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight myHeight
  mapM_ _someMergeUpdate_invalidateHeight mergeUpdates --TODO: In addition to when the patch is completely empty, we should also not run this if it has some Nothing values, but none of them have actually had any effect; potentially, we could even check for Just values with no effect (e.g. by comparing their IORefs and ignoring them if they are unchanged); actually, we could just check if the new height is different
  forM_ coincidenceInfos $ \(SomeResetCoincidence subscription mcs) -> do
    unsubscribe subscription
    mapM_ invalidateCoincidenceHeight mcs
  forM_ coincidenceInfos $ \(SomeResetCoincidence _ mcs) -> mapM_ recalculateCoincidenceHeight mcs
  mapM_ _someMergeUpdate_recalculateHeight mergeUpdates
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> do
    height <- calculateSwitchHeight subscribed
    updateSwitchHeight height subscribed
  return result

newtype Height = Height { unHeight :: Int } deriving (Show, Read, Eq, Ord, Bounded)

{-# INLINE zeroHeight #-}
zeroHeight :: Height
zeroHeight = Height 0

{-# INLINE invalidHeight #-}
invalidHeight :: Height
invalidHeight = Height (-1000)

#ifdef DEBUG_CYCLES
-- | An invalid height that is currently being traversed, e.g. by walkInvalidHeightParents
{-# INLINE invalidHeightBeingTraversed #-}
invalidHeightBeingTraversed :: Height
invalidHeightBeingTraversed = Height (-1001)
#endif

{-# INLINE succHeight #-}
succHeight :: Height -> Height
succHeight h@(Height a) =
  if h == invalidHeight
  then invalidHeight
  else Height $ succ a

invalidateCoincidenceHeight :: CoincidenceSubscribed x a -> IO ()
invalidateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight /= invalidHeight) $ do
    writeIORef (coincidenceSubscribedHeight subscribed) $! invalidHeight
    WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight

updateSwitchHeight :: Height -> SwitchSubscribed x a -> IO ()
updateSwitchHeight new subscribed = do
  oldHeight <- readIORef $ switchSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    when (new /= invalidHeight) $ do --TODO: This 'when' should probably be an assertion
      writeIORef (switchSubscribedHeight subscribed) $! new
      WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ recalculateSubscriberHeight new

recalculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO ()
recalculateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    height <- calculateCoincidenceHeight subscribed
    when (height /= invalidHeight) $ do
      writeIORef (coincidenceSubscribedHeight subscribed) $! height
      WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight height

calculateSwitchHeight :: SwitchSubscribed x a -> IO Height
calculateSwitchHeight subscribed = getEventSubscribedHeight . _eventSubscription_subscribed =<< readIORef (switchSubscribedCurrentParent subscribed)

calculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO Height
calculateCoincidenceHeight subscribed = do
  outerHeight <- getEventSubscribedHeight $ _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
  innerHeight <- maybe (return zeroHeight) getEventSubscribedHeight =<< readIORef (coincidenceSubscribedInnerParent subscribed)
  return $ if outerHeight == invalidHeight || innerHeight == invalidHeight then invalidHeight else max outerHeight innerHeight

data SomeSwitchSubscribed x = forall a. SomeSwitchSubscribed {-# NOUNPACK #-} (SwitchSubscribed x a)

invalidate :: IORef [SomeSwitchSubscribed x] -> WeakList (Invalidator x) -> IO (WeakList (Invalidator x))
invalidate toReconnectRef wis = do
  forM_ wis $ \wi -> do
    mi <- deRefWeak wi
    case mi of
      Nothing -> do
        traceInvalidate "invalidate Dead"
        return () --TODO: Should we clean this up here?
      Just i -> do
        finalize wi -- Once something's invalidated, it doesn't need to hang around; this will change when some things are strict
        case i of
          InvalidatorPull p -> do
            traceInvalidate $ "invalidate: Pull" <> showNodeId p
            mVal <- readIORef $ pullValue p
            forM_ mVal $ \val -> do
              writeIORef (pullValue p) Nothing
              writeIORef (pullSubscribedInvalidators val) =<< evaluate =<< invalidate toReconnectRef =<< readIORef (pullSubscribedInvalidators val)
          InvalidatorSwitch subscribed -> do
            traceInvalidate $ "invalidate: Switch" <> showNodeId subscribed
            modifyIORef' toReconnectRef (SomeSwitchSubscribed subscribed :)
  return [] -- Since we always finalize everything, always return an empty list --TODO: There are some things that will need to be re-subscribed every time; we should try to avoid finalizing them

--------------------------------------------------------------------------------
-- Reflex integration
--------------------------------------------------------------------------------

-- | Designates the default, global Spider timeline
data SpiderTimeline x
type role SpiderTimeline nominal

-- | The default, global Spider environment
type Spider = SpiderTimeline Global

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (EventM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (EventM x) where
  {-# INLINABLE hold #-}
  hold = holdSpiderEventM
  {-# INLINABLE holdDyn #-}
  holdDyn = holdDynSpiderEventM
  {-# INLINABLE holdIncremental #-}
  holdIncremental = holdIncrementalSpiderEventM
  {-# INLINABLE buildDynamic #-}
  buildDynamic = buildDynamicSpiderEventM
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = nowSpiderEventM

instance Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPullM x) where
  {-# INLINABLE sample #-}
  sample = coerce . readBehaviorTracked . unSpiderBehavior

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = SpiderPushM $ readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Class.current <$> Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . dynamicHold <$> Reflex.Spider.Internal.hold v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce e
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderPushM $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderPushM nowSpiderEventM


instance HasSpiderTimeline x => Monad (Reflex.Class.Dynamic (SpiderTimeline x)) where
  {-# INLINE (>>=) #-}
  x >>= f = SpiderDynamic $ dynamicDynIdentity $ newJoinDyn $ newMapDyn (unSpiderDynamic . f) $ unSpiderDynamic x
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail _ = error "Dynamic does not support 'fail'"
#endif

{-# INLINABLE newJoinDyn #-}
newJoinDyn :: HasSpiderTimeline x => DynamicS x (Identity (DynamicS x (Identity a))) -> Reflex.Spider.Internal.Dyn x (Identity a)
newJoinDyn d =
  let readV0 = readBehaviorTracked . dynamicCurrent =<< readBehaviorTracked (dynamicCurrent d)
      eOuter = Reflex.Spider.Internal.push (fmap (Just . Identity) . readBehaviorUntracked . dynamicCurrent . runIdentity) $ dynamicUpdated d
      eInner = Reflex.Spider.Internal.switch $ dynamicUpdated <$> dynamicCurrent d
      eBoth = Reflex.Spider.Internal.coincidence $ dynamicUpdated . runIdentity <$> dynamicUpdated d
      v' = unSpiderEvent $ Reflex.Class.leftmost $ map SpiderEvent [eBoth, eOuter, eInner]
  in Reflex.Spider.Internal.unsafeBuildDynamic readV0 v'

instance HasSpiderTimeline x => Functor (Reflex.Class.Dynamic (SpiderTimeline x)) where
  fmap = mapDynamicSpider
  x <$ d = R.unsafeBuildDynamic (return x) $ x <$ R.updated d

mapDynamicSpider :: HasSpiderTimeline x => (a -> b) -> Reflex.Class.Dynamic (SpiderTimeline x) a -> Reflex.Class.Dynamic (SpiderTimeline x) b
mapDynamicSpider f = SpiderDynamic . newMapDyn f . unSpiderDynamic
{-# INLINE [1] mapDynamicSpider #-}

instance HasSpiderTimeline x => Applicative (Reflex.Class.Dynamic (SpiderTimeline x)) where
  pure = SpiderDynamic . dynamicConst
#if MIN_VERSION_base(4,10,0)
  liftA2 f a b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith f (unSpiderDynamic a) (unSpiderDynamic b)
#endif
  SpiderDynamic a <*> SpiderDynamic b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith ($) a b
  a *> b = R.unsafeBuildDynamic (R.sample $ R.current b) $ R.leftmost [R.updated b, R.tag (R.current b) $ R.updated a]
  (<*) = flip (*>) -- There are no effects, so order doesn't matter

holdSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Behavior (SpiderTimeline x) a)
holdSpiderEventM v0 e = fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdDynSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
holdDynSpiderEventM v0 e = fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdIncrementalSpiderEventM :: (HasSpiderTimeline x, Patch p) => PatchTarget p -> Reflex.Class.Event (SpiderTimeline x) p -> EventM x (Reflex.Class.Incremental (SpiderTimeline x) p)
holdIncrementalSpiderEventM v0 e = fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e

buildDynamicSpiderEventM :: HasSpiderTimeline x => SpiderPushM x a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
buildDynamicSpiderEventM getV0 e = fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE hold #-}
  hold v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdIncremental v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = runFrame . runSpiderHostFrame $ Reflex.Class.buildDynamic getV0 e
  {-# INLINABLE headE #-}
  headE e = runFrame . runSpiderHostFrame $ Reflex.Class.headE e
  {-# INLINABLE now #-}
  now = runFrame . runSpiderHostFrame $ Reflex.Class.now


instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHostFrame x) where
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE hold #-}
  hold v0 e = SpiderHostFrame $ fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderHostFrame $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderHostFrame Reflex.Class.now

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE sample #-}
  sample = runFrame . readBehaviorUntracked . unSpiderBehavior

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# INLINABLE sample #-}
  sample = Reflex.Spider.Internal.ReadPhase . Reflex.Class.sample

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdIncremental v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.buildDynamic getV0 e
  {-# INLINABLE headE #-}
  headE e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.headE e
  {-# INLINABLE now #-}
  now = Reflex.Spider.Internal.ReadPhase Reflex.Class.now

--------------------------------------------------------------------------------
-- Deprecated items
--------------------------------------------------------------------------------

-- | 'SpiderEnv' is the old name for 'SpiderTimeline'
{-# DEPRECATED SpiderEnv "Use 'SpiderTimelineEnv' instead" #-}
type SpiderEnv = SpiderTimeline
instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent e = SpiderHostFrame $ do
    --TODO: Unsubscribe eventually (manually and/or with weak ref)
    val <- liftIO $ newIORef Nothing
    subscription <- subscribe (unSpiderEvent e) $ Subscriber
      { subscriberPropagate = \a -> do
          liftIO $ writeIORef val $ Just a
          scheduleClear val
      , subscriberInvalidateHeight = \_ -> return ()
      , subscriberRecalculateHeight = \_ -> return ()
      }
    return $ SpiderEventHandle
      { spiderEventHandleSubscription = subscription
      , spiderEventHandleValue = val
      }

instance HasSpiderTimeline x => Reflex.Host.Class.ReflexHost (SpiderTimeline x) where
  type EventTrigger (SpiderTimeline x) = RootTrigger x
  type EventHandle (SpiderTimeline x) = SpiderEventHandle x
  type HostFrame (SpiderTimeline x) = SpiderHostFrame x

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReadEvent (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# NOINLINE readEvent #-}
  readEvent h = Reflex.Spider.Internal.ReadPhase $ fmap (fmap return) $ liftIO $ do
    result <- readIORef $ spiderEventHandleValue h
    touch h
    return result

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHost x) where
  newEventWithTrigger = SpiderHost . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHost $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHostFrame x) where
  newEventWithTrigger = SpiderHostFrame . EventM . liftIO . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHostFrame $ EventM $ liftIO $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es

instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = runFrame . runSpiderHostFrame . Reflex.Host.Class.subscribeEvent

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexHost (SpiderTimeline x) (SpiderHost x) where
  type ReadPhase (SpiderHost x) = Reflex.Spider.Internal.ReadPhase x
  fireEventsAndRead es (Reflex.Spider.Internal.ReadPhase a) = run es a
  runHostFrame = runFrame . runSpiderHostFrame

unsafeNewSpiderTimelineEnv :: forall x. IO (SpiderTimelineEnv x)
unsafeNewSpiderTimelineEnv = do
  lock <- newMVar ()
  env <- newEventEnv
#ifdef DEBUG
  depthRef <- newIORef 0
#endif
  return $ STE $ SpiderTimelineEnv
    { _spiderTimeline_lock = lock
    , _spiderTimeline_eventEnv = env
#ifdef DEBUG
    , _spiderTimeline_depth = depthRef
#endif
    }

-- | Create a new SpiderTimelineEnv
newSpiderTimeline :: IO (Some SpiderTimelineEnv)
newSpiderTimeline = withSpiderTimeline (pure . Some)

data LocalSpiderTimeline (x :: Type) s

instance Reifies s (SpiderTimelineEnv x) =>
         HasSpiderTimeline (LocalSpiderTimeline x s) where
  spiderTimeline = localSpiderTimeline Proxy $ reflect (Proxy :: Proxy s)

localSpiderTimeline
  :: proxy s
  -> SpiderTimelineEnv x
  -> SpiderTimelineEnv (LocalSpiderTimeline x s)
localSpiderTimeline _ = coerce

-- | Pass a new timeline to the given function.
withSpiderTimeline :: (forall x. HasSpiderTimeline x => SpiderTimelineEnv x -> IO r) -> IO r
withSpiderTimeline k = do
  env <- unsafeNewSpiderTimelineEnv
  reify env $ \s -> k $ localSpiderTimeline s env

newtype SpiderPullM (x :: Type) a = SpiderPullM (BehaviorM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

type ComputeM = EventM

newtype SpiderPushM (x :: Type) a = SpiderPushM (ComputeM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance HasSpiderTimeline x => R.Reflex (SpiderTimeline x) where
  {-# SPECIALIZE instance R.Reflex (SpiderTimeline Global) #-}
  newtype Behavior (SpiderTimeline x) a = SpiderBehavior { unSpiderBehavior :: Behavior x a }
  newtype Event (SpiderTimeline x) a = SpiderEvent { unSpiderEvent :: Event x a }
  newtype Dynamic (SpiderTimeline x) a = SpiderDynamic { unSpiderDynamic :: DynamicS x (Identity a) } -- deriving (Functor, Applicative, Monad)
  newtype Incremental (SpiderTimeline x) p = SpiderIncremental { unSpiderIncremental :: DynamicS x p }
  type PullM (SpiderTimeline x) = SpiderPullM x
  type PushM (SpiderTimeline x) = SpiderPushM x
  {-# INLINABLE never #-}
  never = SpiderEvent eventNever
  {-# INLINABLE constant #-}
  constant = SpiderBehavior . behaviorConst
  {-# INLINE push #-}
  push f = SpiderEvent . push (coerce f) . unSpiderEvent
  {-# INLINE pushCheap #-}
  pushCheap f = SpiderEvent . pushCheap (coerce f) . unSpiderEvent
  {-# INLINABLE pull #-}
  pull = SpiderBehavior . pull . coerce
  {-# INLINABLE fanG #-}
  fanG e = R.EventSelectorG $ SpiderEvent . selectG (fanG (unSpiderEvent e))
  {-# INLINABLE mergeG #-}
  mergeG
    :: forall k2 (k :: k2 -> Type) q (v :: k2 -> Type). GCompare k
    => (forall a. q a -> R.Event (SpiderTimeline x) (v a))
    -> DMap k q
    -> R.Event (SpiderTimeline x) (DMap k v)
  mergeG nt = SpiderEvent . mergeG (unSpiderEvent #. nt) . dynamicConst
  {-# INLINABLE switch #-}
  switch = SpiderEvent . switch . (coerce :: Behavior x (R.Event (SpiderTimeline x) a) -> Behavior x (Event x a)) . unSpiderBehavior
  {-# INLINABLE coincidence #-}
  coincidence = SpiderEvent . coincidence . (coerce :: Event x (R.Event (SpiderTimeline x) a) -> Event x (Event x a)) . unSpiderEvent
  {-# INLINABLE current #-}
  current = SpiderBehavior . dynamicCurrent . unSpiderDynamic
  {-# INLINABLE updated #-}
  updated = SpiderEvent #. dynamicUpdated .# fmap coerce . unSpiderDynamic
  {-# INLINABLE unsafeBuildDynamic #-}
  unsafeBuildDynamic readV0 v' = SpiderDynamic $ dynamicDynIdentity $ unsafeBuildDynamic (coerce readV0) $ coerce $ unSpiderEvent v'
  {-# INLINABLE unsafeBuildIncremental #-}
  unsafeBuildIncremental readV0 dv = SpiderIncremental $ dynamicDyn $ unsafeBuildDynamic (coerce readV0) $ unSpiderEvent dv
  {-# INLINABLE mergeIncrementalG #-}
  mergeIncrementalG nt = SpiderEvent #. mergeG (coerce #. nt) .# unSpiderIncremental
  {-# INLINABLE mergeIncrementalWithMoveG #-}
  mergeIncrementalWithMoveG nt = SpiderEvent #. mergeWithMove (coerce #. nt) .# unSpiderIncremental
  {-# INLINABLE currentIncremental #-}
  currentIncremental = SpiderBehavior . dynamicCurrent . unSpiderIncremental
  {-# INLINABLE updatedIncremental #-}
  updatedIncremental = SpiderEvent . dynamicUpdated . unSpiderIncremental
  {-# INLINABLE incrementalToDynamic #-}
  incrementalToDynamic (SpiderIncremental i) = SpiderDynamic $ dynamicDynIdentity $ unsafeBuildDynamic (readBehaviorUntracked $ dynamicCurrent i) $ flip push (dynamicUpdated i) $ \p -> do
    c <- readBehaviorUntracked $ dynamicCurrent i
    return $ Identity <$> apply p c --TODO: Avoid the redundant 'apply'
  eventCoercion Coercion = Coercion
  behaviorCoercion Coercion = Coercion
  dynamicCoercion Coercion = Coercion
  incrementalCoercion Coercion Coercion = Coercion
  {-# INLINABLE mergeIntIncremental #-}
  mergeIntIncremental = SpiderEvent . mergeInt . coerce
  {-# INLINABLE fanInt #-}
  fanInt e = R.EventSelectorInt $ SpiderEvent . selectInt (fanInt (unSpiderEvent e))

data RootTrigger x a = forall k. GCompare k => RootTrigger (WeakBag (Subscriber x a), IORef (DMap k Identity), k a)

data SpiderEventHandle x a = SpiderEventHandle
  { spiderEventHandleSubscription :: EventSubscription x
  , spiderEventHandleValue :: IORef (Maybe a)
  }

instance MonadRef (EventM x) where
  type Ref (EventM x) = Ref IO
  {-# INLINABLE newRef #-}
  {-# INLINABLE readRef #-}
  {-# INLINABLE writeRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a

instance MonadAtomicRef (EventM x) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r f = liftIO $ atomicModifyRef r f

-- | The monad for actions that manipulate a Spider timeline identified by @x@
newtype SpiderHost (x :: Type) a = SpiderHost { unSpiderHost :: IO a }
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHost x) where
  {-# INLINABLE (>>=) #-}
  SpiderHost x >>= f = SpiderHost $ x >>= unSpiderHost . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINABLE fail #-}
  fail = MonadFail.fail
#endif

instance MonadFail (SpiderHost x) where
  {-# INLINABLE fail #-}
  fail s = SpiderHost $ MonadFail.fail s

-- | Run an action affecting the global Spider timeline; this will be guarded by
-- a mutex for that timeline
runSpiderHost :: SpiderHost Global a -> IO a
runSpiderHost (SpiderHost a) = a

-- | Run an action affecting a given Spider timeline; this will be guarded by a
-- mutex for that timeline
runSpiderHostForTimeline :: SpiderHost x a -> SpiderTimelineEnv x -> IO a
runSpiderHostForTimeline (SpiderHost a) _ = a

newtype SpiderHostFrame (x :: Type) a = SpiderHostFrame { runSpiderHostFrame :: EventM x a }
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException, MonadMask, MonadThrow, MonadCatch)

instance Monad (SpiderHostFrame x) where
  {-# INLINABLE (>>=) #-}
  SpiderHostFrame x >>= f = SpiderHostFrame $ x >>= runSpiderHostFrame . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINABLE fail #-}
  fail s = SpiderHostFrame $ fail s
#endif

instance NotReady (SpiderTimeline x) (SpiderHostFrame x) where
  notReadyUntil _ = pure ()
  notReady = pure ()

newEventWithTriggerIO :: forall x a. HasSpiderTimeline x => (RootTrigger x a -> IO (IO ())) -> IO (Event x a)
newEventWithTriggerIO f = do
  es <- newFanEventWithTriggerIO $ \Refl -> f
  return $ select es Refl

newFanEventWithTriggerIO :: (HasSpiderTimeline x, GCompare k) => (forall a. k a -> RootTrigger x a -> IO (IO ())) -> IO (EventSelector x k)
newFanEventWithTriggerIO f = do
  occRef <- newIORef DMap.empty
  subscribedRef <- newIORef DMap.empty
  let !r = Root
        { rootOccurrence = occRef
        , rootSubscribed = subscribedRef
        , rootInit = f
        }
  return $ EventSelector $ \k -> eventRoot k r

newtype ReadPhase x a = ReadPhase (ResultM x a) deriving (Functor, Applicative, Monad, MonadFix)

instance MonadRef (SpiderHost x) where
  type Ref (SpiderHost x) = Ref IO
  newRef = SpiderHost . newRef
  readRef = SpiderHost . readRef
  writeRef r = SpiderHost . writeRef r

instance MonadAtomicRef (SpiderHost x) where
  atomicModifyRef r = SpiderHost . atomicModifyRef r

instance MonadRef (SpiderHostFrame x) where
  type Ref (SpiderHostFrame x) = Ref IO
  newRef = SpiderHostFrame . newRef
  readRef = SpiderHostFrame . readRef
  writeRef r = SpiderHostFrame . writeRef r

instance MonadAtomicRef (SpiderHostFrame x) where
  atomicModifyRef r = SpiderHostFrame . atomicModifyRef r

instance PrimMonad (SpiderHostFrame x) where
  type PrimState (SpiderHostFrame x) = PrimState IO
  primitive = SpiderHostFrame . EventM . primitive

instance NotReady (SpiderTimeline x) (SpiderHost x) where
  notReadyUntil _ = return ()
  notReady = return ()

instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)) where
  notReadyUntil _ = return ()
  notReady = return ()
