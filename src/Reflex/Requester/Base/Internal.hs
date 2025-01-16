-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base.Internal where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Dynamic
import Reflex.EventWriter.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Bits
import Data.Coerce
import Data.Constraint
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Some (Some(Some))
import Data.Type.Equality
import Data.Unique.Tag

import GHC.Exts (Any)
import Unsafe.Coerce

--TODO: Make this module type-safe

newtype TagMap (f :: Type -> Type) = TagMap (IntMap Any)

newtype RequesterData f = RequesterData (TagMap (Entry f))

emptyRequesterData :: RequesterData f
emptyRequesterData = RequesterData $ TagMap IntMap.empty

data RequesterDataKey a where
  RequesterDataKey_Single :: {-# UNPACK #-} !(MyTag (Single a)) -> RequesterDataKey a
  RequesterDataKey_Multi :: {-# UNPACK #-} !(MyTag Multi) -> {-# UNPACK #-} !Int -> !(RequesterDataKey a) -> RequesterDataKey a --TODO: Don't put a second Int here (or in the other Multis); use a single Int instead
  RequesterDataKey_Multi2 :: GCompare k => {-# UNPACK #-} !(MyTag (Multi2 k)) -> !(Some k) -> {-# UNPACK #-} !Int -> !(RequesterDataKey a) -> RequesterDataKey a
  RequesterDataKey_Multi3 :: {-# UNPACK #-} !(MyTag Multi3) -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> !(RequesterDataKey a) -> RequesterDataKey a

singletonRequesterData :: RequesterDataKey a -> f a -> RequesterData f
singletonRequesterData rdk v = case rdk of
  RequesterDataKey_Single k -> RequesterData $ singletonTagMap k $ Entry v
  RequesterDataKey_Multi k k' k'' -> RequesterData $ singletonTagMap k $ Entry $ IntMap.singleton k' $ singletonRequesterData k'' v
  RequesterDataKey_Multi2 k k' k'' k''' -> RequesterData $ singletonTagMap k $ Entry $ Multi2Contents
    { _multi2Contents_values = Map.singleton k' $ IntMap.singleton k'' $ singletonRequesterData k''' v
    , _multi2Contents_dict = Dict
    }
  RequesterDataKey_Multi3 k k' k'' k''' -> RequesterData $ singletonTagMap k $ Entry $ IntMap.singleton k' $ IntMap.singleton k'' $ singletonRequesterData k''' v

mergeRequesterData :: RequesterData f -> RequesterData f -> RequesterData f
mergeRequesterData (RequesterData a) (RequesterData b) = RequesterData $ mergeTagMap a b

mergeTagMap :: forall f. TagMap (Entry f) -> TagMap (Entry f) -> TagMap (Entry f)
mergeTagMap (TagMap m) (TagMap n) =
  TagMap $ IntMap.unionWithKey (g' combiner) m n
  where
    combiner :: forall a. MyTag a -> Entry f a -> Entry f a -> Entry f a
    combiner k (Entry a) (Entry b) = Entry $ case myKeyType k of
      MyTagType_Single -> a
      MyTagType_Multi -> IntMap.unionWith mergeRequesterData a b
      MyTagType_Multi2 -> case _multi2Contents_dict a of
        Dict -> Multi2Contents
          { _multi2Contents_values = Map.unionWith (IntMap.unionWith mergeRequesterData) (_multi2Contents_values a) (_multi2Contents_values b)
          , _multi2Contents_dict = Dict
          }
      MyTagType_Multi3 -> IntMap.unionWith (IntMap.unionWith mergeRequesterData) a b
    g' :: (forall a. MyTag a -> Entry f a -> Entry f a -> Entry f a) -> Int -> Any -> Any -> Any
    g' f rawKey a b =
      let k = MyTag rawKey :: MyTag a
          fromAny :: Any -> Entry f a
          fromAny = unsafeCoerce
          toAny :: Entry f a -> Any
          toAny = unsafeCoerce
      in toAny $ f k (fromAny a) (fromAny b)

requesterDataToList :: RequesterData f -> [DSum RequesterDataKey f]
requesterDataToList (RequesterData m) = do
  k :=> Entry e <- tagMapToList m
  case myKeyType k of
    MyTagType_Single -> return $ RequesterDataKey_Single k :=> e
    MyTagType_Multi -> do
      (k', e') <- IntMap.toList e
      k'' :=> e'' <- requesterDataToList e'
      return $ RequesterDataKey_Multi k k' k'' :=> e''
    MyTagType_Multi2 -> case _multi2Contents_dict e of
      Dict -> do
        (k', e') <- Map.toList $ _multi2Contents_values e
        (k'', e'') <- IntMap.toList e'
        k''' :=> e''' <- requesterDataToList e''
        return $ RequesterDataKey_Multi2 k k' k'' k''' :=> e'''
    MyTagType_Multi3 -> do
      (k', e') <- IntMap.toList e
      (k'', e'') <- IntMap.toList e'
      k''' :=> e''' <- requesterDataToList e''
      return $ RequesterDataKey_Multi3 k k' k'' k''' :=> e'''

singletonTagMap :: forall f a. MyTag a -> f a -> TagMap f
singletonTagMap (MyTag k) v = TagMap $ IntMap.singleton k $ (unsafeCoerce :: f a -> Any) v

tagMapToList :: forall f. TagMap f -> [DSum MyTag f]
tagMapToList (TagMap m) = f <$> IntMap.toList m
  where f :: (Int, Any) -> DSum MyTag f
        f (k, v) = MyTag k :=> (unsafeCoerce :: Any -> f a) v

traverseTagMapWithKey :: forall t f g. Applicative t => (forall a. MyTag a -> f a -> t (g a)) -> TagMap f -> t (TagMap g)
traverseTagMapWithKey f (TagMap m) = TagMap <$> IntMap.traverseWithKey g m
  where
    g :: Int -> Any -> t Any
    g k v = (unsafeCoerce :: g a -> Any) <$> f (MyTag k) ((unsafeCoerce :: Any -> f a) v)

-- | Runs in reverse to accommodate for the fact that we accumulate it in reverse
traverseRequesterData :: forall m request response. Applicative m => (forall a. request a -> m (response a)) -> RequesterData request -> m (RequesterData response)
traverseRequesterData f (RequesterData m) = RequesterData <$> traverseTagMapWithKey go m --TODO: reverse this, since our tags are in reverse order
  where go :: forall x. MyTag x -> Entry request x -> m (Entry response x)
        go k (Entry request) = Entry <$> case myKeyType k of
          MyTagType_Single -> f request
          MyTagType_Multi -> traverse (traverseRequesterData f) request
          MyTagType_Multi2 -> case request of
            Multi2Contents { _multi2Contents_values = request', _multi2Contents_dict = Dict } -> do
              v <- traverse (traverse (traverseRequesterData f)) request'
              pure $ Multi2Contents
                { _multi2Contents_values = v
                , _multi2Contents_dict = Dict
                }
          MyTagType_Multi3 -> traverse (traverse (traverseRequesterData f)) request

-- | 'traverseRequesterData' with its arguments flipped
forRequesterData :: forall request response m. Applicative m => RequesterData request -> (forall a. request a -> m (response a)) -> m (RequesterData response)
forRequesterData r f = traverseRequesterData f r

data MyTagType :: Type -> Type where
  MyTagType_Single :: MyTagType (Single a)
  MyTagType_Multi :: MyTagType Multi
  MyTagType_Multi2 :: MyTagType (Multi2 k)
  MyTagType_Multi3 :: MyTagType Multi3

myKeyType :: MyTag x -> MyTagType x
myKeyType (MyTag k) = case k .&. 0x3 of
  0x0 -> unsafeCoerce MyTagType_Single
  0x1 -> unsafeCoerce MyTagType_Multi
  0x2 -> unsafeCoerce MyTagType_Multi2
  0x3 -> unsafeCoerce MyTagType_Multi3
  t -> error $ "Reflex.Requester.Base.myKeyType: no such key type" <> show t

data Single a
data Multi
data Multi2 (k :: Type -> Type)
data Multi3

class MyTagTypeOffset x where
  myTagTypeOffset :: proxy x -> Int

instance MyTagTypeOffset (Single a) where
  myTagTypeOffset _ = 0x0

instance MyTagTypeOffset Multi where
  myTagTypeOffset _ = 0x1

instance MyTagTypeOffset (Multi2 k) where
  myTagTypeOffset _ = 0x2

instance MyTagTypeOffset Multi3 where
  myTagTypeOffset _ = 0x3

type family EntryContents request a where
  EntryContents request (Single a) = request a
  EntryContents request Multi = IntMap (RequesterData request)
  EntryContents request (Multi2 k) = Multi2Contents k request
  EntryContents request Multi3 = IntMap (IntMap (RequesterData request))

data Multi2Contents k request = Multi2Contents
  { _multi2Contents_dict :: {-# UNPACK #-} !(Dict (GCompare k)) -- This is a Dict instead of an existential context because we only want to use it in certain circumstances
  , _multi2Contents_values :: {-# UNPACK #-} !(Map (Some k) (IntMap (RequesterData request)))
  }

newtype Entry request x = Entry { unEntry :: EntryContents request x }

{-# INLINE singleEntry #-}
singleEntry :: f a -> Entry f (Single a)
singleEntry = Entry

{-# INLINE multiEntry #-}
multiEntry :: IntMap (RequesterData f) -> Entry f Multi
multiEntry = Entry

{-# INLINE unMultiEntry #-}
unMultiEntry :: Entry f Multi -> IntMap (RequesterData f)
unMultiEntry = unEntry

-- | We use a hack here to pretend we have x ~ request a; we don't want to use a GADT, because GADTs (even with zero-size existential contexts) can't be newtypes
-- WARNING: This type should never be exposed.  In particular, this is extremely unsound if a MyTag from one run of runRequesterT is ever compared against a MyTag from another
newtype MyTag x = MyTag Int deriving (Show, Eq, Ord, Enum)

newtype MyTagWrap (f :: Type -> Type) x = MyTagWrap Int deriving (Show, Eq, Ord, Enum)

{-# INLINE castMyTagWrap #-}
castMyTagWrap :: MyTagWrap f (Entry f x) -> MyTagWrap g (Entry g x)
castMyTagWrap = coerce

instance GEq MyTag where
  (MyTag a) `geq` (MyTag b) =
    if a == b
    then Just $ unsafeCoerce Refl
    else Nothing

instance GCompare MyTag where
  (MyTag a) `gcompare` (MyTag b) =
    case a `compare` b of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

instance GEq (MyTagWrap f) where
  (MyTagWrap a) `geq` (MyTagWrap b) =
    if a == b
    then Just $ unsafeCoerce Refl
    else Nothing

instance GCompare (MyTagWrap f) where
  (MyTagWrap a) `gcompare` (MyTagWrap b) =
    case a `compare` b of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

data RequesterState t (request :: Type -> Type) = RequesterState
  { _requesterState_nextMyTag :: {-# UNPACK #-} !Int -- Starts at -4 and goes down by 4 each time, to accommodate two 'type' bits at the bottom
  , _requesterState_requests :: ![(Int, Event t Any)]
  }

-- | A basic implementation of 'Requester'.
newtype RequesterT t request (response :: Type -> Type) m a = RequesterT { unRequesterT :: StateT (RequesterState t request) (ReaderT (EventSelectorInt t Any) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
           , MonadAsyncException
           , MonadCatch
           , MonadThrow
           , MonadMask
           )

deriving instance MonadSample t m => MonadSample t (RequesterT t request response m)
deriving instance MonadHold t m => MonadHold t (RequesterT t request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m)

instance EventWriter t w m => EventWriter t w (RequesterT t request response m) where
  tellEvent = lift . tellEvent

instance PrimMonad m => PrimMonad (RequesterT t request response m) where
  type PrimState (RequesterT t request response m) = PrimState m
  primitive = lift . primitive

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (RequesterT t request response m a) where
  mempty = pure mempty

instance (Semigroup a, Monad m) => Semigroup (RequesterT t request response m a) where
  (<>) = liftA2 (<>)


-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.
runRequesterT :: (Reflex t, Monad m)
              => RequesterT t request response m a
              -> Event t (RequesterData response) --TODO: This DMap will be in reverse order, so we need to make sure the caller traverses it in reverse
              -> m (a, Event t (RequesterData request)) --TODO: we need to hide these 'MyTag's here, because they're unsafe to mix in the wild
runRequesterT (RequesterT a) responses = do
  (result, s) <- runReaderT (runStateT a $ RequesterState (-4) []) $ fanInt $
    coerceEvent responses
  return (result, fmapCheap (RequesterData . TagMap) $ mergeInt $ IntMap.fromDistinctAscList $ _requesterState_requests s)

-- | Map a function over the request and response of a 'RequesterT'
withRequesterT
  :: (Reflex t, MonadFix m)
  => (forall x. req x -> req' x) -- ^ The function to map over the request
  -> (forall x. rsp' x -> rsp x) -- ^ The function to map over the response
  -> RequesterT t req rsp m a -- ^ The internal 'RequesterT' whose input and output will be transformed
  -> RequesterT t req' rsp' m a -- ^ The resulting 'RequesterT'
withRequesterT freq frsp child = do
  rec let rsp = fmap (runIdentity . traverseRequesterData (Identity . frsp)) rsp'
      (a, req) <- lift $ runRequesterT child rsp
      rsp' <- fmap (flip selectInt 0 . fanInt . fmapCheap unMultiEntry) $ requesting' $
        fmapCheap (multiEntry . IntMap.singleton 0) $ fmap (runIdentity . traverseRequesterData (Identity . freq)) req
  return a

instance (Reflex t, Monad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  requesting = fmap coerceEvent . responseFromTag . castMyTagWrap <=< tagRequest . (coerceEvent :: Event t (request a) -> Event t (Entry request (Single a)))
  requesting_ = void . tagRequest . fmapCheap singleEntry

{-# INLINE tagRequest #-}
tagRequest :: forall m x t request response. (Monad m, MyTagTypeOffset x) => Event t (Entry request x) -> RequesterT t request response m (MyTagWrap request (Entry request x))
tagRequest req = do
  old <- RequesterT get
  let n = _requesterState_nextMyTag old .|. myTagTypeOffset (Proxy :: Proxy x)
      t = MyTagWrap n
  RequesterT $ put $ RequesterState
    { _requesterState_nextMyTag = _requesterState_nextMyTag old - 0x4
    , _requesterState_requests = (n, (unsafeCoerce :: Event t (Entry request x) -> Event t Any) req) : _requesterState_requests old
    }
  return t

{-# INLINE responseFromTag #-}
responseFromTag :: forall m t request response x. Monad m => MyTagWrap response (Entry response x) -> RequesterT t request response m (Event t (Entry response x))
responseFromTag (MyTagWrap t) = do
  responses :: EventSelectorInt t Any <- RequesterT ask
  return $ (unsafeCoerce :: Event t Any -> Event t (Entry response x)) $ selectInt responses t

instance MonadTrans (RequesterT t request response) where
  lift = RequesterT . lift . lift

instance MFunctor (RequesterT t request response) where
  hoist f = RequesterT . hoist (hoist f) . unRequesterT

instance PerformEvent t m => PerformEvent t (RequesterT t request response m) where
  type Performable (RequesterT t request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RequesterT t request response m) where
  type Ref (RequesterT t request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t request response m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReader r m => MonadReader r (RequesterT t request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapStateT (mapReaderT $ local f) a
  reader = lift . reader

instance (Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (RequesterT t request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm') patchIntMapNewElementsMap mergeIntIncremental
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith patchMapNewElementsMap mergeMapIncremental
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElementsMap mergeMapIncrementalWithMove

requesting' :: (MyTagTypeOffset x, Monad m) => Event t (Entry request x) -> RequesterT t request response m (Event t (Entry response x))
requesting' = responseFromTag . castMyTagWrap <=< tagRequest

{-# INLINABLE runWithReplaceRequesterTWith #-}
runWithReplaceRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m
                                                                 , MonadFix m
                                                                 )
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  rec na' <- numberOccurrencesFrom 1 a'
      responses <- fmap (fmapCheap unMultiEntry) $ requesting' $ fmapCheap multiEntry $ switchPromptlyDyn requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      let responses' = fanInt responses
      ((result0, requests0), v') <- f (runRequesterT a0 (selectInt responses' 0)) $ fmapCheap (\(n, a) -> fmap ((,) n) $ runRequesterT a $ selectInt responses' n) na'
      requests <- holdDyn (fmapCheap (IntMap.singleton 0) requests0) $ fmapCheap (\(n, (_, reqs)) -> fmapCheap (IntMap.singleton n) reqs) v'
  return (result0, fmapCheap (fst . snd) v')

{-# INLINE traverseIntMapWithKeyWithAdjustRequesterTWith #-}
traverseIntMapWithKeyWithAdjustRequesterTWith :: forall t request response m v v' p.
                                        ( Reflex t
                                        , MonadHold t m
                                        , PatchTarget (p (Event t (IntMap (RequesterData request)))) ~ IntMap (Event t (IntMap (RequesterData request)))
                                        , Patch (p (Event t (IntMap (RequesterData request))))
                                        , Functor p
                                        , MonadFix m
                                        )
                                     => (   (IntMap.Key -> (IntMap.Key, v) -> m (Event t (IntMap (RequesterData request)), v'))
                                         -> IntMap (IntMap.Key, v)
                                         -> Event t (p (IntMap.Key, v))
                                         -> RequesterT t request response m (IntMap (Event t (IntMap (RequesterData request)), v'), Event t (p (Event t (IntMap (RequesterData request)), v')))
                                        )
                                     -> (p (Event t (IntMap (RequesterData request))) -> IntMap (Event t (IntMap (RequesterData request))))
                                     -> (Incremental t (p (Event t (IntMap (RequesterData request)))) -> Event t (IntMap (IntMap (RequesterData request))))
                                     -> (IntMap.Key -> v -> RequesterT t request response m v')
                                     -> IntMap v
                                     -> Event t (p v)
                                     -> RequesterT t request response m (IntMap v', Event t (p v'))
traverseIntMapWithKeyWithAdjustRequesterTWith base patchNewElements mergePatchIncremental f dm0 dm' = do
  rec response <- requesting' $ fmapCheap pack $ promptRequests `mappend` mergePatchIncremental requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      let responses :: EventSelectorInt t (IntMap (RequesterData response))
          responses = fanInt $ fmapCheap unpack response
          unpack :: Entry response Multi3 -> IntMap (IntMap (RequesterData response))
          unpack = unEntry
          pack :: IntMap (IntMap (RequesterData request)) -> Entry request Multi3
          pack = Entry
          f' :: IntMap.Key -> (Int, v) -> m (Event t (IntMap (RequesterData request)), v')
          f' k (n, v) = do
            (result, myRequests) <- runRequesterT (f k v) $ mapMaybeCheap (IntMap.lookup n) $ selectInt responses k --TODO: Instead of doing mapMaybeCheap, can we share a fanInt across all instances of a given key, or at least the ones that are adjacent in time?
            return (fmapCheap (IntMap.singleton n) myRequests, result)
      ndm' <- numberOccurrencesFrom 1 dm'
      (children0, children') <- base f' (fmap ((,) 0) dm0) $ fmap (\(n, dm) -> fmap ((,) n) dm) ndm' --TODO: Avoid this somehow, probably by adding some sort of per-cohort information passing to Adjustable
      let result0 = fmap snd children0
          result' = fforCheap children' $ fmap snd
          requests0 :: IntMap (Event t (IntMap (RequesterData request)))
          requests0 = fmap fst children0
          requests' :: Event t (p (Event t (IntMap (RequesterData request))))
          requests' = fforCheap children' $ fmap fst
          promptRequests :: Event t (IntMap (IntMap (RequesterData request)))
          promptRequests = coincidence $ fmapCheap (mergeInt . patchNewElements) requests' --TODO: Create a mergeIncrementalPromptly, and use that to eliminate this 'coincidence'
      requests <- holdIncremental requests0 requests'
  return (result0, result')

{-# INLINE traverseDMapWithKeyWithAdjustRequesterTWith #-}
traverseDMapWithKeyWithAdjustRequesterTWith :: forall k t request response m v v' p p'.
                                        ( GCompare k
                                        , Reflex t
                                        , MonadHold t m
                                        , PatchTarget (p' (Some k) (Event t (IntMap (RequesterData request)))) ~ Map (Some k) (Event t (IntMap (RequesterData request)))
                                        , Patch (p' (Some k) (Event t (IntMap (RequesterData request))))
                                        , MonadFix m
                                        )
                                     => (forall k' v1 v2. GCompare k'
                                         => (forall a. k' a -> v1 a -> m (v2 a))
                                         -> DMap k' v1
                                         -> Event t (p k' v1)
                                         -> RequesterT t request response m (DMap k' v2, Event t (p k' v2))
                                        )
                                     -> (forall v1 v2. (forall a. v1 a -> v2 a) -> p k v1 -> p k v2)
                                     -> (forall v1 v2. (forall a. v1 a -> v2) -> p k v1 -> p' (Some k) v2)
                                     -> (forall v2. p' (Some k) v2 -> Map (Some k) v2)
                                     -> (forall a. Incremental t (p' (Some k) (Event t a)) -> Event t (Map (Some k) a))
                                     -> (forall a. k a -> v a -> RequesterT t request response m (v' a))
                                     -> DMap k v
                                     -> Event t (p k v)
                                     -> RequesterT t request response m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustRequesterTWith base mapPatch weakenPatchWith patchNewElements mergePatchIncremental f dm0 dm' = do
  rec response <- requesting' $ fmapCheap pack $ promptRequests `mappend` mergePatchIncremental requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      let responses :: EventSelector t (Const2 (Some k) (IntMap (RequesterData response)))
          responses = fanMap $ fmapCheap unpack response
          unpack :: Entry response (Multi2 k) -> Map (Some k) (IntMap (RequesterData response))
          unpack = _multi2Contents_values . unEntry
          pack :: Map (Some k) (IntMap (RequesterData request)) -> Entry request (Multi2 k)
          pack m = Entry $ Multi2Contents { _multi2Contents_values = m, _multi2Contents_dict = Dict }
          f' :: forall a. k a -> Compose ((,) Int) v a -> m (Compose ((,) (Event t (IntMap (RequesterData request)))) v' a)
          f' k (Compose (n, v)) = do
            (result, myRequests) <- runRequesterT (f k v) $ mapMaybeCheap (IntMap.lookup n) $ select responses (Const2 (Some k))
            return $ Compose (fmapCheap (IntMap.singleton n) myRequests, result)
      ndm' <- numberOccurrencesFrom 1 dm'
      (children0, children') <- base f' (DMap.map (\v -> Compose (0, v)) dm0) $ fmap (\(n, dm) -> mapPatch (\v -> Compose (n, v)) dm) ndm'
      let result0 = DMap.map (snd . getCompose) children0
          result' = fforCheap children' $ mapPatch $ snd . getCompose
          requests0 :: Map (Some k) (Event t (IntMap (RequesterData request)))
          requests0 = weakenDMapWith (fst . getCompose) children0
          requests' :: Event t (p' (Some k) (Event t (IntMap (RequesterData request))))
          requests' = fforCheap children' $ weakenPatchWith $ fst . getCompose
          promptRequests :: Event t (Map (Some k) (IntMap (RequesterData request)))
          promptRequests = coincidence $ fmapCheap (mergeMap . patchNewElements) requests' --TODO: Create a mergeIncrementalPromptly, and use that to eliminate this 'coincidence'
      requests <- holdIncremental requests0 requests'
  return (result0, result')

data Decoder rawResponse response =
  forall a. Decoder (RequesterDataKey a) (rawResponse -> response a)

matchResponsesWithRequests
  :: forall t rawRequest rawResponse request response m.
     ( MonadFix m
     , MonadHold t m
     , Reflex t
     )
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
  -- ^ Given a request (from 'Requester'), produces the wire format of the
  -- request and a function used to process the associated response
  -> Event t (RequesterData request)
  -- ^ The outgoing requests
  -> Event t (Int, rawResponse)
  -- ^ The incoming responses, tagged by an identifying key
  -> m ( Event t (Map Int rawRequest)
       , Event t (RequesterData response)
       )
  -- ^ A map of outgoing wire-format requests and an event of responses keyed
  -- by the 'RequesterData' key of the associated outgoing request
matchResponsesWithRequests f send recv = matchResponseMapWithRequests f send $ uncurry Map.singleton <$> recv

-- | Matches incoming responses with previously-sent requests
-- and uses the provided request "decoder" function to process
-- incoming responses.
matchResponseMapWithRequests
  :: forall t rawRequest rawResponse request response m.
     ( MonadFix m
     , MonadHold t m
     , Reflex t
     )
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
  -- ^ Given a request (from 'Requester'), produces the wire format of the
  -- request and a function used to process the associated response
  -> Event t (RequesterData request)
  -- ^ The outgoing requests
  -> Event t (Map Int rawResponse)
  -- ^ A map of incoming responses, tagged by an identifying key
  -> m ( Event t (Map Int rawRequest)
       , Event t (RequesterData response)
       )
  -- ^ A map of outgoing wire-format requests and an event of responses keyed
  -- by the 'RequesterData' key of the associated outgoing request
matchResponseMapWithRequests f send recv = do
  rec nextId <- hold 1 $ fmap (\(next, _, _) -> next) outgoing
      waitingFor :: Incremental t (PatchMap Int (Decoder rawResponse response)) <-
        holdIncremental mempty $ leftmost
          [ fmap (\(_, outstanding, _) -> outstanding) outgoing
          , snd <$> incoming
          ]
      let outgoing = processOutgoing nextId send
          incoming = processIncoming waitingFor recv
  return (fmap (\(_, _, rawReqs) -> rawReqs) outgoing, fst <$> incoming)
  where
    -- Tags each outgoing request with an identifying integer key
    -- and returns the next available key, a map of response decoders
    -- for requests for which there are outstanding responses, and the
    -- raw requests to be sent out.
    processOutgoing
      :: Behavior t Int
      -- The next available key
      -> Event t (RequesterData request)
      -- The outgoing request
      -> Event t ( Int
                 , PatchMap Int (Decoder rawResponse response)
                 , Map Int rawRequest )
      -- The new next-available-key, a map of requests expecting responses, and the tagged raw requests
    processOutgoing nextId out = flip pushAlways out $ \dm -> do
      oldNextId <- sample nextId
      let (result, newNextId) = flip runState oldNextId $ forM (requesterDataToList dm) $ \(k :=> v) -> do
            n <- get
            put $ succ n
            let (rawReq, rspF) = f v
            return (n, rawReq, Decoder k rspF)
          patchWaitingFor = PatchMap $ Map.fromList $
            (\(n, _, dec) -> (n, Just dec)) <$> result
          toSend = Map.fromList $ (\(n, rawReq, _) -> (n, rawReq)) <$> result
      return (newNextId, patchWaitingFor, toSend)
    -- Looks up the each incoming raw response in a map of response
    -- decoders and returns the decoded response and a patch that can
    -- be used to clear the ID of the consumed response out of the queue
    -- of expected responses.
    processIncoming
      :: Incremental t (PatchMap Int (Decoder rawResponse response))
      -- A map of outstanding expected responses
      -> Event t (Map Int rawResponse)
      -- A incoming response paired with its identifying key
      -> Event t (RequesterData response, PatchMap Int v)
      -- The decoded response and a patch that clears the outstanding responses queue
    processIncoming waitingFor inc = flip push inc $ \rspMap -> do
      wf <- sample $ currentIncremental waitingFor
      let match rawRsp (Decoder k rspF) =
            let rsp = rspF rawRsp
            in singletonRequesterData k rsp
          matches = Map.intersectionWith match rspMap wf
      pure $ if Map.null matches then Nothing else Just
        (Map.foldl' mergeRequesterData emptyRequesterData matches, PatchMap $ Nothing <$ matches)
