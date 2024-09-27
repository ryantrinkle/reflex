-- | This module provides 'EventWriterT', the standard implementation of
-- 'EventWriter'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

module Reflex.EventWriter.Base
  ( EventWriterT (..)
  , runEventWriterT
  , runWithReplaceEventWriterTWith
  , sequenceDMapWithAdjustEventWriterTWith
  , mapEventWriterT
  , withEventWriterT
  ) where

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.EventWriter.Class (EventWriter, tellEvent)
import Reflex.DynamicWriter.Class (DynamicWriter, tellDyn)
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Query.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose
import Data.Functor.Misc
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Some (Some)
import Data.Tuple
import Data.Type.Equality

import Unsafe.Coerce

{-# DEPRECATED TellId "Do not construct this directly; use tellId instead" #-}
newtype TellId w x
  = TellId Int -- ^ WARNING: Do not construct this directly; use 'tellId' instead
  deriving (Show, Eq, Ord, Enum)

tellId :: Int -> TellId w w
tellId = TellId
{-# INLINE tellId #-}

tellIdRefl :: TellId w x -> w :~: x
tellIdRefl _ = unsafeCoerce Refl

withTellIdRefl :: TellId w x -> (w ~ x => r) -> r
withTellIdRefl tid r = case tellIdRefl tid of
  Refl -> r

instance GEq (TellId w) where
  a `geq` b =
    withTellIdRefl a $
    withTellIdRefl b $
    if a == b
    then Just Refl
    else Nothing

instance GCompare (TellId w) where
  a `gcompare` b =
    withTellIdRefl a $
    withTellIdRefl b $
    case a `compare` b of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT

data EventWriterState t w = EventWriterState
  { _eventWriterState_nextId :: {-# UNPACK #-} !Int -- Always negative (and decreasing over time)
  , _eventWriterState_told :: ![DSum (TellId w) (Event t)] -- In increasing order
  }

-- | A basic implementation of 'EventWriter'.
newtype EventWriterT t w m a = EventWriterT { unEventWriterT :: StateT (EventWriterState t w) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MFunctor
    , MonadFix
    , MonadIO
    , MonadException
    , MonadAsyncException
    , MonadMask
    , MonadCatch
    , MonadThrow
    )

-- | Run a 'EventWriterT' action.
runEventWriterT :: forall t m w a. (Reflex t, Monad m, Semigroup w) => EventWriterT t w m a -> m (a, Event t w)
runEventWriterT (EventWriterT a) = do
  (result, requests) <- runStateT a $ EventWriterState (-1) []
  let combineResults :: DMap (TellId w) Identity -> w
      combineResults = sconcat
        . (\(h : t) -> h :| t) -- Unconditional; 'merge' guarantees that it will only fire with non-empty DMaps
        . DMap.foldlWithKey (\vs tid (Identity v) -> withTellIdRefl tid $ v : vs) [] -- This is where we finally reverse the DMap to get things in the correct order
  return (result, fmap combineResults $ merge $ DMap.fromDistinctAscList $ _eventWriterState_told requests) --TODO: We can probably make this fromDistinctAscList more efficient by knowing the length in advance, but this will require exposing internals of DMap; also converting it to use a strict list might help

instance (Reflex t, Monad m, Semigroup w) => EventWriter t w (EventWriterT t w m) where
  tellEvent w = EventWriterT $ modify $ \old ->
    let myId = _eventWriterState_nextId old
    in EventWriterState
       { _eventWriterState_nextId = pred myId
       , _eventWriterState_told = (tellId myId :=> w) : _eventWriterState_told old
       }

instance MonadSample t m => MonadSample t (EventWriterT t w m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (EventWriterT t w m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE
  {-# INLINABLE now #-}
  now = lift now

instance (Reflex t, Adjustable t m, MonadHold t m, Semigroup w) => Adjustable t (EventWriterT t w m) where
  runWithReplace = runWithReplaceEventWriterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = sequenceIntMapWithAdjustEventWriterTWith (\f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm') mergeIntIncremental coincidencePatchIntMap
  traverseDMapWithKeyWithAdjust = sequenceDMapWithAdjustEventWriterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith mergeMapIncremental coincidencePatchMap
  traverseDMapWithKeyWithAdjustWithMove = sequenceDMapWithAdjustEventWriterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith mergeMapIncrementalWithMove coincidencePatchMapWithMove

instance Requester t m => Requester t (EventWriterT t w m) where
  type Request (EventWriterT t w m) = Request m
  type Response (EventWriterT t w m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

-- | Given a function like 'runWithReplace' for the underlying monad, implement
-- 'runWithReplace' for 'EventWriterT'.  This is necessary when the underlying
-- monad doesn't have a 'Adjustable' instance or to override the default
-- 'Adjustable' behavior.
runWithReplaceEventWriterTWith :: forall m t w a b. (Reflex t, MonadHold t m, Semigroup w)
                               => (forall a' b'. m a' -> Event t (m b') -> EventWriterT t w m (a', Event t b'))
                               -> EventWriterT t w m a
                               -> Event t (EventWriterT t w m b)
                               -> EventWriterT t w m (a, Event t b)
runWithReplaceEventWriterTWith f a0 a' = do
  (result0, result') <- f (runEventWriterT a0) $ fmapCheap runEventWriterT a'
  tellEvent =<< switchHoldPromptOnly (snd result0) (fmapCheap snd result')
  return (fst result0, fmapCheap fst result')

-- | Like 'runWithReplaceEventWriterTWith', but for 'sequenceIntMapWithAdjust'.
sequenceIntMapWithAdjustEventWriterTWith
  :: forall t m p w v v'
  .  ( Reflex t
     , MonadHold t m
     , Semigroup w
     , Functor p
     , Patch (p (Event t w))
     , PatchTarget (p (Event t w)) ~ IntMap (Event t w)
     , Patch (p w)
     , PatchTarget (p w) ~ IntMap w
     )
  => (   (IntMap.Key -> v -> m (Event t w, v'))
      -> IntMap v
      -> Event t (p v)
      -> EventWriterT t w m (IntMap (Event t w, v'), Event t (p (Event t w, v')))
     )
  -> (Incremental t (p (Event t w)) -> Event t (PatchTarget (p w)))
  -> (Event t (p (Event t w)) -> Event t (p w))
  -> (IntMap.Key -> v -> EventWriterT t w m v')
  -> IntMap v
  -> Event t (p v)
  -> EventWriterT t w m (IntMap v', Event t (p v'))
sequenceIntMapWithAdjustEventWriterTWith base mergePatchIncremental coincidencePatch f dm0 dm' = do
  let f' :: IntMap.Key -> v -> m (Event t w, v')
      f' k v = swap <$> runEventWriterT (f k v)
  (children0, children') <- base f' dm0 dm'
  let result0 = fmap snd children0
      result' = fmapCheap (fmap snd) children'
      requests0 :: IntMap (Event t w)
      requests0 = fmap fst children0
      requests' :: Event t (p (Event t w))
      requests' = fmapCheap (fmap fst) children'
  e <- switchHoldPromptOnlyIncremental mergePatchIncremental coincidencePatch requests0 requests'
  tellEvent $ fforMaybeCheap e $ \m ->
    case IntMap.elems m of
      [] -> Nothing
      h : t -> Just $ sconcat $ h :| t
  return (result0, result')

-- | Like 'runWithReplaceEventWriterTWith', but for 'sequenceDMapWithAdjust'.
sequenceDMapWithAdjustEventWriterTWith
  :: forall t m p p' w k v v'
  .  ( Reflex t
     , MonadHold t m
     , Semigroup w
     , Patch (p' (Some k) (Event t w))
     , PatchTarget (p' (Some k) (Event t w)) ~ Map (Some k) (Event t w)
     , GCompare k
     , Patch (p' (Some k) w)
     , PatchTarget (p' (Some k) w) ~ Map (Some k) w
     )
  => (   (forall a. k a -> v a -> m (Compose ((,) (Event t w)) v' a))
      -> DMap k v
      -> Event t (p k v)
      -> EventWriterT t w m (DMap k (Compose ((,) (Event t w)) v'), Event t (p k (Compose ((,) (Event t w)) v')))
     )
  -> ((forall a. Compose ((,) (Event t w)) v' a -> v' a) -> p k (Compose ((,) (Event t w)) v') -> p k v')
  -> ((forall a. Compose ((,) (Event t w)) v' a -> Event t w) -> p k (Compose ((,) (Event t w)) v') -> p' (Some k) (Event t w))
  -> (Incremental t (p' (Some k) (Event t w)) -> Event t (PatchTarget (p' (Some k) w)))
  -> (Event t (p' (Some k) (Event t w)) -> Event t (p' (Some k) w))
  -> (forall a. k a -> v a -> EventWriterT t w m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> EventWriterT t w m (DMap k v', Event t (p k v'))
sequenceDMapWithAdjustEventWriterTWith base mapPatch weakenPatchWith mergePatchIncremental coincidencePatch f dm0 dm' = do
  let f' :: forall a. k a -> v a -> m (Compose ((,) (Event t w)) v' a)
      f' k v = Compose . swap <$> runEventWriterT (f k v)
  (children0, children') <- base f' dm0 dm'
  let result0 = DMap.map (snd . getCompose) children0
      result' = fforCheap children' $ mapPatch $ snd . getCompose
      requests0 :: Map (Some k) (Event t w)
      requests0 = weakenDMapWith (fst . getCompose) children0
      requests' :: Event t (p' (Some k) (Event t w))
      requests' = fforCheap children' $ weakenPatchWith $ fst . getCompose
  e <- switchHoldPromptOnlyIncremental mergePatchIncremental coincidencePatch requests0 requests'
  tellEvent $ fforMaybeCheap e $ \m ->
    case Map.elems m of
      [] -> Nothing
      h : t -> Just $ sconcat $ h :| t
  return (result0, result')

instance PerformEvent t m => PerformEvent t (EventWriterT t w m) where
  type Performable (EventWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PostBuild t m => PostBuild t (EventWriterT t w m) where
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (EventWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadReader r m => MonadReader r (EventWriterT t w m) where
  ask = lift ask
  local f (EventWriterT a) = EventWriterT $ mapStateT (local f) a
  reader = lift . reader

instance MonadRef m => MonadRef (EventWriterT t w m) where
  type Ref (EventWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (EventWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (EventWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (MonadQuery t q m, Monad m) => MonadQuery t q (EventWriterT t w m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance DynamicWriter t w m => DynamicWriter t w (EventWriterT t v m) where
  tellDyn = lift . tellDyn

instance PrimMonad m => PrimMonad (EventWriterT t w m) where
  type PrimState (EventWriterT t w m) = PrimState m
  primitive = lift . primitive

-- | Map a function over the output of a 'EventWriterT'.
withEventWriterT :: (Semigroup w, Semigroup w', Reflex t, MonadHold t m)
                 => (w -> w')
                 -> EventWriterT t w m a
                 -> EventWriterT t w' m a
withEventWriterT f ew = do
  (r, e) <- lift $ do
    (r, e) <- runEventWriterT ew
    let e' = fmap f e
    return (r, e')
  tellEvent e
  return r

-- | Change the monad underlying an EventWriterT
mapEventWriterT
  :: (forall x. m x -> n x)
  -> EventWriterT t w m a
  -> EventWriterT t w n a
mapEventWriterT f (EventWriterT a) = EventWriterT $ mapStateT f a
