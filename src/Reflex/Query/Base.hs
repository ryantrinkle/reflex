{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Reflex.Query.Base
  ( QueryT (..)
  , runQueryT
  , mapQuery
  , mapQueryResult
  , dynWithQueryT
  , withQueryT
  , mapQueryT
  ) where

import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum(..))
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Misc
import Data.GADT.Compare (GCompare)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Semigroup as S
import Data.Semigroup.Commutative
import Data.Some (Some(Some))
import Data.These

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
#endif

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.DynamicWriter.Class
import Reflex.EventWriter.Base
import Reflex.EventWriter.Class
import Reflex.Host.Class
import qualified Data.Patch.MapWithMove as MapWithMove
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Query.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

newtype QueryT t q m a = QueryT { unQueryT :: StateT [Behavior t q] (EventWriterT t q (ReaderT (Dynamic t (QueryResult q)) m)) a }
  deriving (Functor, Applicative, Monad, MonadException, MonadFix, MonadIO, MonadAtomicRef, MonadCatch, MonadThrow, MonadMask)

deriving instance MonadHold t m => MonadHold t (QueryT t q m)
deriving instance MonadSample t m => MonadSample t (QueryT t q m)

runQueryT :: (MonadFix m, Commutative q, Group q, Reflex t) => QueryT t q m a -> Dynamic t (QueryResult q) -> m (a, Incremental t (AdditivePatch q))
runQueryT (QueryT a) qr = do
  ((r, bs), es) <- runReaderT (runEventWriterT (runStateT a mempty)) qr
  return (r, unsafeBuildIncremental (foldlM (\b c -> (b <>) <$> sample c) mempty bs) (fmapCheap AdditivePatch es))

newtype QueryTLoweredResult t q v = QueryTLoweredResult (v, [Behavior t q])

getQueryTLoweredResultValue :: QueryTLoweredResult t q v -> v
getQueryTLoweredResultValue (QueryTLoweredResult (v, _)) = v

getQueryTLoweredResultWritten :: QueryTLoweredResult t q v -> [Behavior t q]
getQueryTLoweredResultWritten (QueryTLoweredResult (_, w)) = w

maskMempty :: (Eq a, Monoid a) => a -> Maybe a
maskMempty x = if x == mempty then Nothing else Just x

instance (Reflex t, MonadFix m, Group q, Commutative q, Query q, Eq q, MonadHold t m, Adjustable t m) => Adjustable t (QueryT t q m) where
  runWithReplace (QueryT a0) a' = do
    ((r0, bs0), r') <- QueryT $ lift $ runWithReplace (runStateT a0 []) $ fmapCheap (flip runStateT [] . unQueryT) a'
    let sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
        sampleBs = foldlM (\b a -> (b <>) <$> sample a) mempty
        bs' = fmapCheap snd $ r'
    bbs <- hold bs0 bs'
    let patches = flip pushCheap bs' $ \newBs -> do
          oldBs <- sample bbs
          maskMempty <$> ((~~) <$> sampleBs newBs <*> sampleBs oldBs)
    QueryT $ modify $ (:) $ pull $ sampleBs =<< sample bbs
    QueryT $ lift $ tellEvent patches
    return (r0, fmapCheap fst r')

  traverseIntMapWithKeyWithAdjust :: forall v v'. (IntMap.Key -> v -> QueryT t q m v') -> IntMap v -> Event t (PatchIntMap v) -> QueryT t q m (IntMap v', Event t (PatchIntMap v'))
  traverseIntMapWithKeyWithAdjust f im0 im' = do
    let f' :: IntMap.Key -> v -> EventWriterT t q (ReaderT (Dynamic t (QueryResult q)) m) (QueryTLoweredResult t q v')
        f' k v = fmap QueryTLoweredResult $ flip runStateT [] $ unQueryT $ f k v
    (result0, result') <- QueryT $ lift $ traverseIntMapWithKeyWithAdjust f' im0 im'
    let liftedResult0 = IntMap.map getQueryTLoweredResultValue result0
        liftedResult' = fforCheap result' $ \(PatchIntMap p) -> PatchIntMap $
          IntMap.map (fmap getQueryTLoweredResultValue) p
        liftedBs0 :: IntMap [Behavior t q]
        liftedBs0 = IntMap.map getQueryTLoweredResultWritten result0
        liftedBs' :: Event t (PatchIntMap [Behavior t q])
        liftedBs' = fforCheap result' $ \(PatchIntMap p) -> PatchIntMap $
          IntMap.map (fmap getQueryTLoweredResultWritten) p
        sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
        sampleBs = foldlM (\b a -> (b <>) <$> sample a) mempty
        accumBehaviors :: forall m'. MonadHold t m'
                       => IntMap [Behavior t q]
                       -> PatchIntMap [Behavior t q]
                       -> m' ( Maybe (IntMap [Behavior t q])
                             , Maybe (AdditivePatch q))
        -- f accumulates the child behavior state we receive from running traverseIntMapWithKeyWithAdjust for the underlying monad.
        -- When an update occurs, it also computes a patch to communicate to the parent QueryT state.
        -- bs0 is a Map denoting the behaviors of the current children.
        -- pbs is a PatchMap denoting an update to the behaviors of the current children
        accumBehaviors bs0 pbs@(PatchIntMap bs') = do
          let p k bs = case IntMap.lookup k bs0 of
                Nothing -> case bs of
                  -- If the update is to delete the state for a child that doesn't exist, the patch is mempty.
                  Nothing -> return mempty
                  -- If the update is to update the state for a child that doesn't exist, the patch is the sample of the new state.
                  Just newBs -> sampleBs newBs
                Just oldBs -> case bs of
                  -- If the update is to delete the state for a child that already exists, the patch is the negation of the child's current state
                  Nothing -> negateG <$> sampleBs oldBs
                  -- If the update is to update the state for a child that already exists, the patch is the negation of sampling the child's current state
                  -- composed with the sampling the child's new state.
                  Just newBs -> (~~) <$> sampleBs newBs <*> sampleBs oldBs
          -- we compute the patch by iterating over the update PatchMap and proceeding by cases. Then we fold over the
          -- child patches and wrap them in AdditivePatch.
          patch <- AdditivePatch . fold <$> IntMap.traverseWithKey p bs'
          return (apply pbs bs0, Just patch)
    (qpatch :: Event t (AdditivePatch q)) <- mapAccumMaybeM_ accumBehaviors liftedBs0 liftedBs'
    tellQueryIncremental $ unsafeBuildIncremental (fold <$> mapM sampleBs liftedBs0) qpatch
    return (liftedResult0, liftedResult')

  traverseDMapWithKeyWithAdjust :: forall (k :: Type -> Type) v v'. (GCompare k) => (forall a. k a -> v a -> QueryT t q m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> QueryT t q m (DMap k v', Event t (PatchDMap k v'))
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    let f' :: forall a. k a -> v a -> EventWriterT t q (ReaderT (Dynamic t (QueryResult q)) m) (Compose (QueryTLoweredResult t q) v' a)
        f' k v = fmap (Compose . QueryTLoweredResult) $ flip runStateT [] $ unQueryT $ f k v
    (result0, result') <- QueryT $ lift $ traverseDMapWithKeyWithAdjust f' dm0 dm'
    let liftedResult0 = mapKeyValuePairsMonotonic (\(k :=> Compose r) -> k :=> getQueryTLoweredResultValue r) result0
        liftedResult' = fforCheap result' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(k :=> ComposeMaybe mr) -> k :=> ComposeMaybe (fmap (getQueryTLoweredResultValue . getCompose) mr)) p
        liftedBs0 :: Map (Some k) [Behavior t q]
        liftedBs0 = Map.fromDistinctAscList $ (\(k :=> Compose r) -> (Some k, getQueryTLoweredResultWritten r)) <$> DMap.toList result0
        liftedBs' :: Event t (PatchMap (Some k) [Behavior t q])
        liftedBs' = fforCheap result' $ \(PatchDMap p) -> PatchMap $
          Map.fromDistinctAscList $ (\(k :=> ComposeMaybe mr) -> (Some k, fmap (getQueryTLoweredResultWritten . getCompose) mr)) <$> DMap.toList p
        sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
        sampleBs = foldlM (\b a -> (b <>) <$> sample a) mempty
        accumBehaviors :: forall m'. MonadHold t m'
                       => Map (Some k) [Behavior t q]
                       -> PatchMap (Some k) [Behavior t q]
                       -> m' ( Maybe (Map (Some k) [Behavior t q])
                             , Maybe (AdditivePatch q))
        -- f accumulates the child behavior state we receive from running traverseDMapWithKeyWithAdjust for the underlying monad.
        -- When an update occurs, it also computes a patch to communicate to the parent QueryT state.
        -- bs0 is a Map denoting the behaviors of the current children.
        -- pbs is a PatchMap denoting an update to the behaviors of the current children
        accumBehaviors bs0 pbs@(PatchMap bs') = do
          let p k bs = case Map.lookup k bs0 of
                Nothing -> case bs of
                  -- If the update is to delete the state for a child that doesn't exist, the patch is mempty.
                  Nothing -> return Nothing
                  -- If the update is to update the state for a child that doesn't exist, the patch is the sample of the new state.
                  Just newBs -> maskMempty <$> sampleBs newBs
                Just oldBs -> case bs of
                  -- If the update is to delete the state for a child that already exists, the patch is the negation of the child's current state
                  Nothing -> maskMempty . negateG <$> sampleBs oldBs
                  -- If the update is to update the state for a child that already exists, the patch is the negation of sampling the child's current state
                  -- composed with the sampling the child's new state.
                  Just newBs -> maskMempty <$> ((~~) <$> sampleBs newBs <*> sampleBs oldBs)
          -- we compute the patch by iterating over the update PatchMap and proceeding by cases. Then we fold over the
          -- child patches and wrap them in AdditivePatch.
          patch <- fold <$> Map.traverseWithKey p bs'
          return (apply pbs bs0, AdditivePatch <$> patch)
    (qpatch :: Event t (AdditivePatch q)) <- mapAccumMaybeM_ accumBehaviors liftedBs0 liftedBs'
    tellQueryIncremental $ unsafeBuildIncremental (fold <$> mapM sampleBs liftedBs0) qpatch
    return (liftedResult0, liftedResult')

  traverseDMapWithKeyWithAdjustWithMove :: forall (k :: Type -> Type) v v'. (GCompare k) => (forall a. k a -> v a -> QueryT t q m (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> QueryT t q m (DMap k v', Event t (PatchDMapWithMove k v'))
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    let f' :: forall a. k a -> v a -> EventWriterT t q (ReaderT (Dynamic t (QueryResult q)) m) (Compose (QueryTLoweredResult t q) v' a)
        f' k v = fmap (Compose . QueryTLoweredResult) $ flip runStateT [] $ unQueryT $ f k v
    (result0, result') <- QueryT $ lift $ traverseDMapWithKeyWithAdjustWithMove f' dm0 dm'
    let liftedResult0 = mapKeyValuePairsMonotonic (\(k :=> Compose r) -> k :=> getQueryTLoweredResultValue r) result0
        liftedResult' = fforCheap result' $ mapPatchDMapWithMove (getQueryTLoweredResultValue . getCompose)
        liftedBs0 :: Map (Some k) [Behavior t q]
        liftedBs0 = Map.fromDistinctAscList $ (\(k :=> Compose r) -> (Some k, getQueryTLoweredResultWritten r)) <$> DMap.toList result0
        liftedBs' :: Event t (PatchMapWithMove (Some k) [Behavior t q])
        liftedBs' = fforCheap result' $ weakenPatchDMapWithMoveWith (getQueryTLoweredResultWritten . getCompose) {- \(PatchDMap p) -> PatchMapWithMove $
          Map.fromDistinctAscList $ (\(k :=> mr) -> (Some k, fmap (fmap (getQueryTLoweredResultWritten . getCompose)) mr)) <$> DMap.toList p -}
        sampleBs :: forall m'. MonadSample t m' => [Behavior t q] -> m' q
        sampleBs = foldlM (\b a -> (b <>) <$> sample a) mempty
        accumBehaviors' :: forall m'. MonadHold t m'
                        => Map (Some k) [Behavior t q]
                        -> PatchMapWithMove (Some k) [Behavior t q]
                        -> m' ( Maybe (Map (Some k) [Behavior t q])
                              , Maybe (AdditivePatch q))
        -- f accumulates the child behavior state we receive from running traverseDMapWithKeyWithAdjustWithMove for the underlying monad.
        -- When an update occurs, it also computes a patch to communicate to the parent QueryT state.
        -- bs0 is a Map denoting the behaviors of the current children.
        -- pbs is a PatchMapWithMove denoting an update to the behaviors of the current children
        accumBehaviors' bs0 pbs = do
          let bs' = unPatchMapWithMove pbs
              p k bs = case Map.lookup k bs0 of
                Nothing -> case MapWithMove._nodeInfo_from bs of
                  -- If the update is to delete the state for a child that doesn't exist, the patch is mempty.
                  MapWithMove.From_Delete -> return Nothing
                  -- If the update is to update the state for a child that doesn't exist, the patch is the sample of the new state.
                  MapWithMove.From_Insert newBs -> maskMempty <$> sampleBs newBs
                  MapWithMove.From_Move k' -> case Map.lookup k' bs0 of
                    Nothing -> return Nothing
                    Just newBs -> maskMempty <$> sampleBs newBs
                Just oldBs -> case MapWithMove._nodeInfo_from bs of
                  -- If the update is to delete the state for a child that already exists, the patch is the negation of the child's current state
                  MapWithMove.From_Delete -> maskMempty . negateG <$> sampleBs oldBs
                  -- If the update is to update the state for a child that already exists, the patch is the negation of sampling the child's current state
                  -- composed with the sampling the child's new state.
                  MapWithMove.From_Insert newBs -> maskMempty <$> ((~~) <$> sampleBs newBs <*> sampleBs oldBs)
                  MapWithMove.From_Move k'
                    | k' == k -> return Nothing
                    | otherwise -> case Map.lookup k' bs0 of
                  -- If we are moving from a non-existent key, that is a delete
                        Nothing -> maskMempty . negateG <$> sampleBs oldBs
                        Just newBs -> maskMempty <$> ((~~) <$> sampleBs newBs <*> sampleBs oldBs)
          -- we compute the patch by iterating over the update PatchMap and proceeding by cases. Then we fold over the
          -- child patches and wrap them in AdditivePatch.
          patch <- fold <$> Map.traverseWithKey p bs'
          return (apply pbs bs0, AdditivePatch <$> patch)
    (qpatch :: Event t (AdditivePatch q)) <- mapAccumMaybeM_ accumBehaviors' liftedBs0 liftedBs'
    tellQueryIncremental $ unsafeBuildIncremental (fold <$> mapM sampleBs liftedBs0) qpatch
    return (liftedResult0, liftedResult')

instance MonadTrans (QueryT t q) where
  lift = QueryT . lift . lift . lift

instance MFunctor (QueryT t q) where
  hoist = mapQueryT

instance PrimMonad m => PrimMonad (QueryT t q m) where
  type PrimState (QueryT t q m) = PrimState m
  primitive = lift . primitive

instance PostBuild t m => PostBuild t (QueryT t q m) where
  getPostBuild = lift getPostBuild

instance (MonadAsyncException m) => MonadAsyncException (QueryT t q m) where
  mask f = QueryT $ mask $ \unMask -> unQueryT $ f $ QueryT . unMask . unQueryT

instance TriggerEvent t m => TriggerEvent t (QueryT t q m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PerformEvent t m => PerformEvent t (QueryT t q m) where
  type Performable (QueryT t q m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (QueryT t q m) where
  type Ref (QueryT t q m) = Ref m
  newRef = QueryT . newRef
  readRef = QueryT . readRef
  writeRef r = QueryT . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (QueryT t q m) where
  newEventWithTrigger = QueryT . newEventWithTrigger
  newFanEventWithTrigger a = QueryT . lift $ newFanEventWithTrigger a

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (QueryT t q m a) where
  mempty = pure mempty
  mappend = (<>)

instance (S.Semigroup a, Monad m) => S.Semigroup (QueryT t q m a) where
  (<>) = liftA2 (S.<>)

-- | withQueryT's QueryMorphism argument needs to be a group homomorphism in order to behave correctly
withQueryT :: (MonadFix m, PostBuild t m, Group q, Group q', Commutative q, Commutative q', Query q')
           => QueryMorphism q q'
           -> QueryT t q m a
           -> QueryT t q' m a
withQueryT f a = do
  r' <- askQueryResult
  (result, q) <- lift $ runQueryT a $ mapQueryResult f <$> r'
  tellQueryIncremental $ unsafeBuildIncremental
    (fmap (mapQuery f) (sample (currentIncremental q)))
    (fmapCheap (AdditivePatch . mapQuery f . unAdditivePatch) $ updatedIncremental q)
  return result

-- | Maps a function over a 'QueryT' that can change the underlying monad
mapQueryT :: (forall b. m b -> n b) -> QueryT t q m a -> QueryT t q n a
mapQueryT f (QueryT a) = QueryT $ mapStateT (mapEventWriterT (mapReaderT f)) a

-- | dynWithQueryT's (Dynamic t QueryMorphism) argument needs to be a group homomorphism at all times in order to behave correctly
dynWithQueryT :: (MonadFix m, PostBuild t m, Group q, Commutative q, Group q', Commutative q', Query q')
           => Dynamic t (QueryMorphism q q')
           -> QueryT t q m a
           -> QueryT t q' m a
dynWithQueryT f q = do
  r' <- askQueryResult
  (result, q') <- lift $ runQueryT q $ zipDynWith mapQueryResult f r'
  tellQueryIncremental $ zipDynIncrementalWith mapQuery f q'
  return result
 where zipDynIncrementalWith g da ib =
         let eab = align (updated da) (updatedIncremental ib)
             ec = flip push eab $ \case
                 This a -> do
                   aOld <- sample $ current da
                   b <- sample $ currentIncremental ib
                   return $ Just $ AdditivePatch (g a b ~~ g aOld b)
                 That (AdditivePatch b) -> do
                   a <- sample $ current da
                   return $ Just $ AdditivePatch $ g a b
                 These a (AdditivePatch b) -> do
                   aOld <- sample $ current da
                   bOld <- sample $ currentIncremental ib
                   return $ Just $ AdditivePatch $ mconcat [ g a bOld, negateG (g aOld bOld), g a b]
         in unsafeBuildIncremental (g <$> sample (current da) <*> sample (currentIncremental ib)) ec

instance (Monad m, Group q, Commutative q, Query q, Reflex t) => MonadQuery t q (QueryT t q m) where
  tellQueryIncremental q = do
    QueryT (modify (currentIncremental q:))
    QueryT (lift (tellEvent (fmapCheap unAdditivePatch (updatedIncremental q))))
  askQueryResult = QueryT ask
  queryIncremental q = do
    tellQueryIncremental q
    zipDynWith crop (incrementalToDynamic q) <$> askQueryResult

instance Requester t m => Requester t (QueryT t q m) where
  type Request (QueryT t q m) = Request m
  type Response (QueryT t q m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance EventWriter t w m => EventWriter t w (QueryT t q m) where
  tellEvent = lift . tellEvent

instance DynamicWriter t w m => DynamicWriter t w (QueryT t q m) where
  tellDyn = lift . tellDyn
