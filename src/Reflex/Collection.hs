{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- |
-- Module:
--   Reflex.Collection
module Reflex.Collection
  (
  -- * Widgets on Collections
    listHoldWithKey
  , listWithKey
  , listWithKeyShallowDiff
  , listViewWithKey
  , selectViewListWithKey
  , selectViewListWithKey_
  -- * List Utils
  , list
  , simpleList
  ) where

#ifdef MIN_VERSION_semialign
import Prelude hiding (zip, zipWith)
#if MIN_VERSION_semialign(1,1,0)
import Data.Zip (Zip (..))
#endif
#endif

import Control.Monad
import Control.Monad.Fix
import Data.Align
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Misc
import Data.These

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Dynamic
import Reflex.PostBuild.Class

-- | Create a set of widgets based on the provided 'Map'. When the
-- input 'Event' fires, remove widgets for keys with the value 'Nothing'
-- and add/replace widgets for keys with 'Just' values.
listHoldWithKey
  :: forall t m k v a
   . (Ord k, Adjustable t m, MonadHold t m)
  => Map k v
  -> Event t (Map k (Maybe v))
  -> (k -> v -> m a)
  -> m (Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap
        (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey
          (\k v -> ComposeMaybe $ fmap (f k) v)
        )
        m'
  (a0, a') <- sequenceDMapWithAdjust dm0 dm'

  --TODO: Move the dmapToMap to the righthand side so it doesn't get
  --fully redone every time
  fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a'

--TODO: Something better than Dynamic t (Map k v) - we want something
--where the Events carry diffs, not the whole value
listWithKey
  :: forall t k v m a
   . (Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m, Eq v)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m a)
  -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  postBuild <- getPostBuild
  let childValChangedSelector = fanMap $ updated vals

      -- We keep track of changes to children values in the mkChild
      -- function we pass to listHoldWithKey The other changes we need
      -- to keep track of are child insertions and
      -- deletions. diffOnlyKeyChanges keeps track of insertions and
      -- deletions but ignores value changes, since they're already
      -- accounted for.
      diffOnlyKeyChanges olds news =
        flip Map.mapMaybe (align olds news) $ \case
          This _    -> Just Nothing
          These _ _ -> Nothing
          That new  -> Just $ Just new
  rec sentVals :: Dynamic t (Map k v) <- foldDyn applyMap Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals =
            attachWith diffOnlyKeyChanges (current sentVals) $ leftmost
              [ updated vals

              -- TODO: This should probably be added to the
              -- attachWith, not to the updated; if we were using
              -- diffMap instead of diffMapNoEq, I think it might not
              -- work
              , tag (current vals) postBuild
              ]
  listHoldWithKey Map.empty changeVals $ \k v ->
    mkChild k =<< holdUniqDyn =<< holdDyn v (select childValChangedSelector $ Const2 k)

-- | Display the given map of items (in key order) using the builder
-- function provided, and update it with the given event.  'Nothing'
-- update entries will delete the corresponding children, and 'Just'
-- entries will create them if they do not exist or send an update
-- event to them if they do.
listWithKeyShallowDiff
  :: (Ord k, Adjustable t m, MonadFix m, MonadHold t m)
  => Map k v
  -> Event t (Map k (Maybe v))
  -> (k -> v -> Event t v -> m a)
  -> m (Dynamic t (Map k a))
listWithKeyShallowDiff initialVals valsChanged mkChild = do
  let childValChangedSelector = fanMap $ fmap (Map.mapMaybe id) valsChanged
  sentVals <- foldDyn applyMap (void initialVals) $ fmap (fmap void) valsChanged
  let relevantPatch patch _ = case patch of

        -- Even if we let a Nothing through when the element doesn't
        -- already exist, this doesn't cause a problem because it is
        -- ignored
        Nothing -> Just Nothing

        -- We don't want to let spurious re-creations of items through
        Just _  -> Nothing
  listHoldWithKey
      initialVals
      (attachWith (flip (Map.differenceWith relevantPatch))
                  (current sentVals)
                  valsChanged
      )
    $ \k v -> mkChild k v $ select childValChangedSelector $ Const2 k

--TODO: Something better than Dynamic t (Map k v) - we want something
--where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of Event-valued widgets.  This
--   is like 'listWithKey', specialized for widgets returning @/Event t a/@.
--   'listWithKey' would return @/Dynamic t (Map k (Event t a))/@ in
--   this scenario, but 'listViewWithKey' flattens this to
--   @/Event t (Map k a)/@ via 'switch'.
listViewWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, Eq v)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m (Event t a))
  -> m (Event t (Map k a))
listViewWithKey vals mkChild =
  switch . fmap mergeMap <$> listViewWithKey' vals mkChild

listViewWithKey'
  :: (Ord k, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, Eq v)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m a)
  -> m (Behavior t (Map k a))
listViewWithKey' vals mkChild = current <$> listWithKey vals mkChild

-- | Create a dynamically-changing set of widgets, one of which is
-- selected at any time.
selectViewListWithKey
  :: forall t m k v a
   . (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m, Eq v)
  => Dynamic t k
  -- ^ Current selection key
  -> Dynamic t (Map k v)
  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -- ^ Function to create a widget for a given key from Dynamic value
  -- and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t (k, a))
  -- ^ Event that fires when any child's return Event fires.  Contains
  -- key of an arbitrary firing widget.
selectViewListWithKey selection vals mkChild = do
  -- For good performance, this value must be shared across all children
  let selectionDemux = demux selection
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    selectSelf <- mkChild k v selected
    return $ fmap ((,) k) selectSelf
  return $ switchPromptlyDyn $ leftmost . Map.elems <$> selectChild

-- | Like 'selectViewListWithKey' but discards the value of the list
-- item widget's output 'Event'.
selectViewListWithKey_
  :: forall t m k v a
   . (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m, Eq v)
  => Dynamic t k
  -- ^ Current selection key
  -> Dynamic t (Map k v)
  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -- ^ Function to create a widget for a given key from Dynamic value
  -- and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t k)
  -- ^ Event that fires when any child's return Event fires.  Contains
  -- key of an arbitrary firing widget.
selectViewListWithKey_ selection vals mkChild =
  fmap fst <$> selectViewListWithKey selection vals mkChild

-- | Create a dynamically-changing set of widgets from a Dynamic
--   key/value map.  Unlike the 'withKey' variants, the child widgets
--   are insensitive to which key they're associated with.
list
  :: (Ord k, Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m, Eq v)
  => Dynamic t (Map k v)
  -> (Dynamic t v -> m a)
  -> m (Dynamic t (Map k a))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

-- | Create a dynamically-changing set of widgets from a Dynamic list.
simpleList
  :: (Adjustable t m, MonadHold t m, PostBuild t m, MonadFix m, Eq v)
  => Dynamic t [v]
  -> (Dynamic t v -> m a)
  -> m (Dynamic t [a])
simpleList xs mkChild =
  fmap (fmap (map snd . Map.toList)) $ flip list mkChild $ fmap
    (Map.fromList . zip [(1 :: Int) ..])
    xs
