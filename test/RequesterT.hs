{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Lens hiding (has)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Constraint.Forall
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Misc
import Data.List (words)
import Data.Map (Map)
import qualified Data.Map as M
#if !MIN_VERSION_these(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import Data.These
import Text.Read (readMaybe)

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif

import Reflex
import Reflex.Requester.Base
import Reflex.Requester.Class
import Reflex.Test.SimpleHost

data RequestInt a where
  RequestInt :: Int -> RequestInt Int

data TestRequest a where
  TestRequest_Reverse :: String -> TestRequest String
  TestRequest_Increment :: Int -> TestRequest Int

deriveArgDict ''TestRequest

instance Show (TestRequest a) where
  show = \case
    TestRequest_Reverse str -> "reverse " <> str
    TestRequest_Increment i -> "increment " <> show i

main :: IO ()
main = do
  os1 <- runApp' (unwrapApp testOrdering) $
    [ Just ()
    ]
  print os1
  os2 <- runApp' (unwrapApp testSimultaneous) $ map Just $
    [ This ()
    , That ()
    , This ()
    , These () ()
    ]
  print os2
  os3 <- runApp' (unwrapApp testMoribundRequest) [Just ()]
  print os3
  os4 <- runApp' (unwrapApp testMoribundRequestDMap) [Just ()]
  print os4
  os5 <- runApp' (unwrapApp testLiveRequestDMap) [Just ()]
  print os5
  os6 <- runApp' (unwrapApp delayedPulse) [Just ()]
  print os6
  os7 <- runApp' testMatchRequestsWithResponses [ Just $ TestRequest_Increment 1 ]
  print os7
  os8 <- runApp' testMatchRequestsWithResponses [ Just $ TestRequest_Reverse "yoyo" ]
  print os8
  let ![[Just [1,2,3,4,5,6,7,8,9,10]]] = os1 -- The order is reversed here: see the documentation for 'runRequesterT'
  let ![[Just [9,7,5,3,1]],[Nothing,Nothing],[Just [10,8,6,4,2]],[Just [10,8,6,4,2],Nothing]] = os2
  let ![[Nothing, Just [2]]] = os3
  let ![[Nothing, Just [2]]] = os4
  let ![[Nothing, Just [1, 2]]] = os5
  -- let ![[Nothing, Nothing]] = os6 -- TODO re-enable this test after issue #233 has been resolved
  let !(Just [(1,"2")]) = M.toList <$> head (head os7)
  let !(Just [(1,"oyoy")]) = M.toList <$> head (head os8)

  return ()

unwrapRequest :: DSum tag RequestInt -> Int
unwrapRequest (_ :=> RequestInt i) = i

unwrapApp :: ( Reflex t, Monad m )
          => (a -> RequesterT t RequestInt Identity m ())
          -> a
          -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runRequesterT (x appIn) never
  return $ fmap (map unwrapRequest . requesterDataToList) e

testOrdering :: ( Response m ~ Identity
                , Request m ~ RequestInt
                , Requester t m
                , Adjustable t m)
             => Event t ()
             -> m ()
testOrdering pulse = forM_ [10,9..1] $ \i ->
  requestingIdentity (RequestInt i <$ pulse)

testSimultaneous :: ( Response m ~ Identity
                    , Request m ~ RequestInt
                    , Requester t m
                    , Adjustable t m)
                 => Event t (These () ())
                 -> m ()
testSimultaneous pulse = do
  let tellE = fmapMaybe (^? here) pulse
      switchE = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> runWithReplace (requestingIdentity (RequestInt i <$ tellE)) $ ffor switchE $ \_ ->
    requestingIdentity (RequestInt (i+1) <$ tellE)

-- | Test that a widget requesting and event which fires at the same time it has been replaced
-- doesn't count along with the new widget.
testMoribundRequest
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testMoribundRequest pulse = do
  rec let requestIntOnReplace x = requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished) <- runWithReplace (requestIntOnReplace 1) $ requestIntOnReplace 2 <$ pulse
  return ()

-- | The equivalent of 'testMoribundRequest' for 'traverseDMapWithKeyWithAdjust'.
testMoribundRequestDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testMoribundRequestDMap pulse = do
  rec let requestIntOnReplace :: Int -> m ()
          requestIntOnReplace x = void $ requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 () Int) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 ()) (Identity v) -> Identity . const v <$> requestIntOnReplace v)
          (mapToDMap $ M.singleton () 1)
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton () 2) <$ pulse)
  return ()

-- | Ensures that elements which are _not_ removed can still fire requests
-- during the same frame as other elements are updated.
testLiveRequestDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , Requester t m
     )
  => Event t ()
  -> m ()
testLiveRequestDMap pulse = do
  rec let requestIntOnReplace :: Int -> m ()
          requestIntOnReplace x = void $ requestingIdentity $ RequestInt x <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 Int ()) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 k) (Identity ()) -> Identity <$> requestIntOnReplace k)
          (mapToDMap $ M.singleton 1 ())
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton 2 ()) <$ pulse)
  return ()

delayedPulse
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     , Response m ~ Identity
     , Request m ~ RequestInt
     , PerformEvent t m
     , Requester t m
     )
  => Event t ()
  -> m ()
delayedPulse pulse = void $ flip runWithReplace (pure () <$ pulse) $ do
    -- This has the effect of delaying pulse' from pulse
    (_, pulse') <- runWithReplace (pure ()) $ pure (RequestInt 1) <$ pulse
    requestingIdentity pulse'

testMatchRequestsWithResponses
  :: forall m t req a
   . ( MonadFix m
     , MonadHold t m
     , Reflex t
     , PerformEvent t m
     , MonadIO (Performable m)
     , ForallF Show req
     , Has Read req
     )
  => Event t (req a) -> m (Event t (Map Int String))
testMatchRequestsWithResponses pulse = mdo
  (_, requests) <- runRequesterT (requesting pulse) responses
  let rawResponses = M.map (\v ->
        case words v of
          ["reverse", str] -> reverse str
          ["increment", i] -> show $ succ $ (read i :: Int)
        ) <$> rawRequestMap
  (rawRequestMap, responses) <- matchResponsesWithRequests reqEncoder requests (head . M.toList <$> rawResponses)
  pure rawResponses
  where
    reqEncoder :: forall a. req a -> (String, String -> Maybe a)
    reqEncoder r =
      ( whichever @Show @req @a $ show r
      , \x -> has @Read r $ readMaybe x
      )
