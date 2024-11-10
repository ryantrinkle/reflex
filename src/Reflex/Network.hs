{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- |
-- Module:
--   Reflex.Network
-- Description:
--   This module provides combinators for building FRP graphs/networks and modifying them dynamically.
module Reflex.Network
  ( networkView
  , networkHold
  , networkHoldFactorableSum
  , untilReady
  ) where

import Control.Lens (Prism')
import Control.Monad (join)
import Control.Monad.Fix (MonadFix)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | A 'Dynamic' "network": Takes a 'Dynamic' of network-creating actions and replaces the network whenever the 'Dynamic' updates.
-- The returned Event of network results fires at post-build time and when the 'Dynamic' updates.
-- Note:  Often, the type 'a' is an Event, in which case the return value is an Event-of-Events, where the outer 'Event' fires
-- when switching networks. Such an 'Event' would typically be flattened (via 'switchHoldPromptly').
networkView :: (NotReady t m, Adjustable t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
networkView child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) postBuild]
  snd <$> runWithReplace notReady newChild

-- | Given an initial "network" and an 'Event' of network-creating actions, create a network that is recreated whenever the Event fires.
-- The returned Dynamic of network results occurs when the Event does.
-- Note:  Often, the type 'a' is an Event, in which case the return value is a Dynamic-of-Events that would typically be flattened (via 'switchPromptlyDyn').
networkHold :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
networkHold child0 newChild = do
  (result0, newResult) <- runWithReplace child0 newChild
  holdDyn result0 newResult

-- | 'runFactorableSumWithReplace' specialized to 'Dynamic', and then held and
-- joined down to one 'Dynamic'. This is to 'runFactorableSumWithReplace', as
-- 'networkHold' is to 'runWithReplace'.
--
-- N.B. A similar outcome could be achieved with some combination of 'factorDyn'
-- and 'networkHold', but this way avoids the initial 'sample' in 'factorDyn'
-- which is a bit too easy to use improperly and get diverging cycles.
networkHoldFactorableSum
  :: forall sum sing t m
  .  (Reflex t, MonadHold t m, MonadFix m, Adjustable t m)
  => (forall a. sum -> (forall tp. sing tp -> tp -> a) -> a)
  -- ^ eliminator for the sum type using the 'sing' singleton to constrain the
  -- 'tp' parameter over valid variant types
  -> (forall tp. sing tp -> Prism' sum tp)
  -- ^ family of ''Prism''s per variant
  -> (forall tp. sing tp -> tp -> Event t tp -> m (Dynamic t tp))
  -- ^ family of constructors of adjustable actions, one constructor per variant
  -> sum
  -- ^ initial value to be held
  -> Event t sum
  -- ^ updates to be held
  -> m (Dynamic t sum)
networkHoldFactorableSum wV pFV f iv ev = do
  (result0, newResult) <- traverseFactorableSumWithAdjust wV pFV f iv ev
  Control.Monad.join <$> holdDyn result0 newResult

-- | Render a placeholder network to be shown while another network is not yet
-- done building
untilReady :: (Adjustable t m, PostBuild t m) => m a -> m b -> m (a, Event t b)
untilReady a b = do
  postBuild <- getPostBuild
  runWithReplace a $ b <$ postBuild
