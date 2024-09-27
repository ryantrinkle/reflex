{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- |
-- Module:
--   Reflex.Adjustable.Class
-- Description:
--   A class for actions that can be "adjusted" over time based on some 'Event'
--   such that, when observed after the firing of any such 'Event', the result
--   is as though the action was originally run with the 'Event's value.
module Reflex.Adjustable.Class
  (
  -- * The Adjustable typeclass
    Adjustable(..)
  , sequenceDMapWithAdjust
  , sequenceDMapWithAdjustWithMove
  , mapMapWithAdjustWithMove
  -- * Deprecated aliases
  , MonadAdjust
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare(..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)

import Reflex.Class
import Data.Patch.DMapWithMove

-- | A 'Monad' that supports adjustment over time.  After an action has been
-- run, if the given events fire, it will adjust itself so that its net effect
-- is as though it had originally been run with the new value.  Note that there
-- is some issue here with persistent side-effects: obviously, IO (and some
-- other side-effects) cannot be undone, so it is up to the instance implementer
-- to determine what the best meaning for this class is in such cases.
class (Reflex t, Monad m) => Adjustable t m | m -> t where
  runWithReplace
    :: m a
    -> Event t (m b)
    -> m (a, Event t b)

  traverseIntMapWithKeyWithAdjust
    :: (IntMap.Key -> v -> m v')
    -> IntMap v
    -> Event t (PatchIntMap v)
    -> m (IntMap v', Event t (PatchIntMap v'))

  traverseDMapWithKeyWithAdjust
    :: GCompare k
    => (forall a. k a -> v a -> m (v' a))
    -> DMap k v
    -> Event t (PatchDMap k v)
    -> m (DMap k v', Event t (PatchDMap k v'))
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = fmap (fmap (fmap fromPatchWithMove)) $
    traverseDMapWithKeyWithAdjustWithMove f dm0 $ fmap toPatchWithMove dm'
   where
    toPatchWithMove (PatchDMap m) = PatchDMapWithMove $ DMap.map toNodeInfoWithMove m
    toNodeInfoWithMove = \case
      ComposeMaybe (Just v) -> NodeInfo (From_Insert v) $ ComposeMaybe Nothing
      ComposeMaybe Nothing -> NodeInfo From_Delete $ ComposeMaybe Nothing
    fromPatchWithMove (PatchDMapWithMove m) = PatchDMap $ DMap.map fromNodeInfoWithMove m
    fromNodeInfoWithMove (NodeInfo from _) = ComposeMaybe $ case from of
      From_Insert v -> Just v
      From_Delete -> Nothing
      From_Move _ -> error "traverseDMapWithKeyWithAdjust: implementation of traverseDMapWithKeyWithAdjustWithMove inserted spurious move"

  traverseDMapWithKeyWithAdjustWithMove
    :: GCompare k
    => (forall a. k a -> v a -> m (v' a))
    -> DMap k v
    -> Event t (PatchDMapWithMove k v)
    -> m (DMap k v', Event t (PatchDMapWithMove k v'))

instance Adjustable t m => Adjustable t (ReaderT r m) where
  runWithReplace a0 a' = do
    r <- ask
    lift $ runWithReplace (runReaderT a0 r) $ fmap (`runReaderT` r) a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runReaderT (f k v) r) dm0 dm'

-- | Traverse a 'DMap' of 'Adjustable' actions, running each of them. The provided 'Event' of patches
-- to the 'DMap' can add, remove, or update values.
sequenceDMapWithAdjust
  :: (GCompare k, Adjustable t m)
  => DMap k m
  -> Event t (PatchDMap k m)
  -> m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjust = traverseDMapWithKeyWithAdjust $ \_ -> fmap Identity

-- | Traverses a 'DMap' of 'Adjustable' actions, running each of them. The provided 'Event' of patches
-- to the 'DMap' can add, remove, update, move, or swap values.
sequenceDMapWithAdjustWithMove
  :: (GCompare k, Adjustable t m)
  => DMap k m
  -> Event t (PatchDMapWithMove k m)
  -> m (DMap k Identity, Event t (PatchDMapWithMove k Identity))
sequenceDMapWithAdjustWithMove = traverseDMapWithKeyWithAdjustWithMove $ \_ -> fmap Identity

-- | Traverses a 'Map', running the provided 'Adjustable' action. The provided 'Event' of patches to the 'Map'
-- can add, remove, update, move, or swap values.
mapMapWithAdjustWithMove
  :: forall t m k v v'. (Adjustable t m, Ord k)
  => (k -> v -> m v')
  -> Map k v
  -> Event t (PatchMapWithMove k v)
  -> m (Map k v', Event t (PatchMapWithMove k v'))
mapMapWithAdjustWithMove f m0 m' = do
  (out0 :: DMap (Const2 k v) (Constant v'), out') <- traverseDMapWithKeyWithAdjustWithMove (\(Const2 k) (Identity v) -> Constant <$> f k v) (mapToDMap m0) (const2PatchDMapWithMoveWith Identity <$> m')
  return (dmapToMapWith (\(Constant v') -> v') out0, patchDMapWithMoveToPatchMapWithMoveWith (\(Constant v') -> v') <$> out')

--------------------------------------------------------------------------------
-- Deprecated functions
--------------------------------------------------------------------------------

{-# DEPRECATED MonadAdjust "Use Adjustable instead" #-}
-- | Synonym for 'Adjustable'
type MonadAdjust = Adjustable
