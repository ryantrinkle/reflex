{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
-- |
-- Module:
--   Reflex.Time
-- Description:
--   Clocks, timers, and other time-related functions.
module Reflex.Time where

import Reflex.Class
import Reflex.Dynamic
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import qualified Control.Concurrent.Thread.Delay as Concurrent
import Control.Lens hiding ((|>))
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Align
import Data.Data (Data)
import Data.Fixed
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.These
import Data.Time.Clock
import GHC.Generics (Generic)
import System.Random

-- | Metadata associated with a timer "tick"
data TickInfo
  = TickInfo { _tickInfo_lastUTC :: UTCTime
             -- ^ UTC time immediately after the last tick.
             , _tickInfo_n :: Integer
             -- ^ Number of time periods or ticks since the start of the timer
             , _tickInfo_alreadyElapsed :: NominalDiffTime
             -- ^ Amount of time that has elapsed in the current tick period.
             }
  deriving (Eq, Ord, Show)

-- | Fires an 'Event' once every time provided interval elapses, approximately.
-- The provided 'UTCTime' is used bootstrap the determination of how much time has elapsed with each tick.
-- This is a special case of 'tickLossyFrom' that uses the post-build event to start the tick thread.
tickLossy :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m) => NominalDiffTime -> UTCTime -> m (Event t TickInfo)
tickLossy dt t0 = tickLossyFrom dt t0 =<< getPostBuild

-- | Fires an 'Event' once every time provided interval elapses, approximately.
-- This is a special case of 'tickLossyFrom' that uses the post-build event to start the tick thread and the time of the post-build as the tick basis time.
tickLossyFromPostBuildTime :: (PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m) => NominalDiffTime -> m (Event t TickInfo)
tickLossyFromPostBuildTime dt = do
  postBuild <- getPostBuild
  postBuildTime <- performEvent $ liftIO getCurrentTime <$ postBuild
  tickLossyFrom' $ (dt,) <$> postBuildTime

-- | Fires an 'Event' approximately each time the provided interval elapses. If the system starts running behind, occurrences will be dropped rather than buffered.
-- Each occurrence of the resulting event will contain the index of the current interval, with 0 representing the provided initial time.
tickLossyFrom
    :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m)
    => NominalDiffTime
    -- ^ The length of a tick interval
    -> UTCTime
    -- ^ The basis time from which intervals count and with which the initial calculation of elapsed time will be made.
    -> Event t a
    -- ^ Event that starts a tick generation thread.  Usually you want this to
    -- be something like the result of getPostBuild that only fires once.  But
    -- there could be uses for starting multiple timer threads.
    -> m (Event t TickInfo)
tickLossyFrom dt t0 e = tickLossyFrom' $ (dt, t0) <$ e

-- | Generalization of tickLossyFrom that takes the delay and initial time as an 'Event'.
tickLossyFrom'
    :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m)
    => Event t (NominalDiffTime, UTCTime)
    -- ^ Event that starts a tick generation thread.  Usually you want this to
    -- be something like the result of 'getPostBuild' that only fires once.  But
    -- there could be uses for starting multiple timer threads.
    -> m (Event t TickInfo)
tickLossyFrom' e = do
  rec result <- performEventAsync $ callAtNextInterval <$> leftmost [e, snd <$> result]
  return $ fst <$> result
  where callAtNextInterval pair cb = void $ liftIO $ forkIO $ do
          tick <- uncurry getCurrentTick pair
          Concurrent.delay $ ceiling $ (fst pair - _tickInfo_alreadyElapsed tick) * 1000000
          cb (tick, pair)

-- | Like 'tickLossy', but immediately calculates the first tick and provides a 'Dynamic' that is updated as ticks fire.
clockLossy :: (MonadIO m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), PostBuild t m, MonadHold t m, MonadFix m) => NominalDiffTime -> UTCTime -> m (Dynamic t TickInfo)
clockLossy dt t0 = do
  initial <- liftIO $ getCurrentTick dt t0
  e <- tickLossy dt t0
  holdDyn initial e

-- | Generates a 'TickInfo', given the specified interval and timestamp. The 'TickInfo' will include the
-- current time, the number of ticks that have elapsed since the timestamp, and the amount of time that
-- has elapsed since the start time of this tick.
getCurrentTick :: NominalDiffTime -> UTCTime -> IO TickInfo
getCurrentTick dt t0 = do
  t <- getCurrentTime
  let offset = t `diffUTCTime` t0
      (n, alreadyElapsed) = offset `divMod'` dt
  return $ TickInfo t n alreadyElapsed

-- | Delay an Event's occurrences by a given amount in seconds.
delay :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t a)
delay dt e = performEventAsync $ ffor e $ \a cb -> liftIO $ void $ forkIO $ do
  Concurrent.delay $ ceiling $ dt * 1000000
  cb a

-- | Send events with Poisson timing with the given basis and rate
--   Each occurrence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time
poissonLossyFrom
  :: (RandomGen g, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> Event t a
  -- ^ Event that starts a tick generation thread. Usually you want this to
  -- be something like the result of getPostBuild that only fires once. But
  -- there could be uses for starting multiple timer threads.
  -- Start sending events in response to the event parameter.
  -> m (Event t TickInfo)
poissonLossyFrom rnd rate = inhomogeneousPoissonFrom rnd (constant rate) rate


-- | Send events with Poisson timing with the given basis and rate
--   Each occurrence of the resulting event will contain the index of
--   the current interval, with 0 representing the basis time.
--   Automatically begin sending events when the DOM is built
poissonLossy
  :: (RandomGen g, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => g
  -> Double
  -- ^ Poisson event rate (Hz)
  -> UTCTime
  -- ^ Baseline time for events
  -> m (Event t TickInfo)
poissonLossy rnd rate t0 = poissonLossyFrom rnd rate t0 =<< getPostBuild

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support.
inhomogeneousPoissonFrom
  :: (RandomGen g, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> Event t a
  -> m (Event t TickInfo)
inhomogeneousPoissonFrom rnd rate maxRate t0 e = do

  -- Create a thread for producing homogeneous poisson events
  -- along with random Doubles (usage of Double's explained below)
  ticksWithRateRand <- performEventAsync $
                       fmap callAtNextInterval e

  -- Filter homogeneous events according to associated
  -- random values and the current rate parameter
  return $ attachWithMaybe filterFun rate ticksWithRateRand

  where

    -- Inhomogeneous poisson processes are built from faster
    -- homogeneous ones by randomly dropping events from the
    -- fast process. For each fast homogeneous event, choose
    -- a uniform random sample from (0, rMax). If the
    -- inhomogeneous rate at this moment is greater than the
    -- random sample, then keep this event, otherwise drop it
    filterFun :: Double -> (TickInfo, Double) -> Maybe TickInfo
    filterFun r (tInfo, p)
      | r >= p    = Just tInfo
      | otherwise = Nothing

    callAtNextInterval _ cb = void $ liftIO $ forkIO $ go t0 rnd cb 0

    go tTargetLast lastGen cb lastN = do
      t <- getCurrentTime

      -- Generate random numbers for this poisson interval (u)
      -- and sample-retention likelihood (p)
      let (u, nextGen)            = randomR (0,1) lastGen
          (p :: Double, nextGen') = randomR (0,maxRate) nextGen

      -- Inter-event interval is drawn from exponential
      -- distribution accourding to u
      let dt             = realToFrac $ (-1) * log u / maxRate :: NominalDiffTime
          nEvents        = lastN + 1
          alreadyElapsed = diffUTCTime t tTargetLast
          tTarget        = addUTCTime dt tTargetLast
          thisDelay      = realToFrac $ diffUTCTime tTarget t :: Double
      Concurrent.delay $ ceiling $ thisDelay * 1000000
      _ <- cb (TickInfo t nEvents alreadyElapsed, p)
      go tTarget nextGen' cb nEvents

-- | Send events with inhomogeneous Poisson timing with the given basis
--   and variable rate. Provide a maxRate that you expect to support
inhomogeneousPoisson
  :: (RandomGen g, MonadIO (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => g
  -> Behavior t Double
  -> Double
  -> UTCTime
  -> m (Event t TickInfo)
inhomogeneousPoisson rnd rate maxRate t0 =
  inhomogeneousPoissonFrom rnd rate maxRate t0 =<< getPostBuild

-- | Block occurrences of an Event until the given number of seconds elapses without
--   the Event firing, at which point the last occurrence of the Event will fire.
debounce :: (MonadFix m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t a)
debounce dt e = do
  n :: Dynamic t Integer <- count e
  let tagged = attachPromptlyDynWith (,) n e
  delayed <- delay dt tagged
  return $ attachWithMaybe (\n' (t, v) -> if n' == t then Just v else Nothing) (current n) delayed

-- | When the given 'Event' occurs, wait the given amount of time and collect
-- all occurrences during that time.  Then, fire the output 'Event' with the
-- collected output.
batchOccurrences :: (MonadFix m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t (Seq a))
batchOccurrences t newValues = do
  let f s x = (Just newState, out)
        where newState = case x of
                This a -> s |> a
                That _ -> mempty
                These a _ -> Seq.singleton a
              out = case x of
                This _ -> if Seq.null s then Just () else Nothing
                That _ -> Nothing
                These _ _ -> Just ()
  rec (buffer, toDelay) <- mapAccumMaybe f mempty $ align newValues delayed
      delayed <- delay t toDelay
  return $ tag buffer delayed

-- | Throttle an input event, ensuring that at least a given amount of time passes between occurrences of the output event. If the input event occurs too
-- frequently, the output event occurs with the most recently seen input value after the given delay passes since the last occurrence of the output.
-- If the output event has not occurred recently, occurrences of the input event will cause the output event to fire immediately.
throttle :: (MonadFix m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => NominalDiffTime -> Event t a -> m (Event t a)
throttle t e = do
  let f (immediate, buffer) x = case x of -- (Just newState, out)
        This a -- If only the input event fires
          | immediate -> -- and we're in immediate mode
            -- Immediate mode turns off, and the buffer is empty.
            -- We fire the output event with the input event value immediately.
            (Just (False, Nothing), Just a)
          | otherwise -> -- and we're not in immediate mode
            -- Immediate mode remains off, and we replace the contents of the buffer (if any) with the input value.
            -- We don't fire the output event.
            (Just (False, Just a), Nothing)
        That _ -> -- If only the delayed output event fires,
          case buffer of
            Nothing -> -- and the buffer is empty:
              -- Immediate mode turns back on, and the buffer remains empty.
              -- We don't fire.
              (Just (True, Nothing), Nothing)
            Just b -> -- and the buffer is full:
              -- Immediate mode remains off, and the buffer is cleared.
              -- We fire with the buffered value.
              (Just (False, Nothing), Just b)
        These a _ -> -- If both the input and delayed output event fire simultaneously:
          -- Immediate mode turns off, and the buffer is empty.
          -- We fire with the input event's value, as it is the most recent we have seen at this moment.
          (Just (False, Nothing), Just a)
  rec (_, outE) <- mapAccumMaybeDyn f (True, Nothing) $ align e delayed -- We start in immediate mode with an empty buffer.
      delayed <- delay t outE
  return outE

data ThrottleState b
  = ThrottleState_Immediate
  | ThrottleState_Buffered (ThrottleBuffer b)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Data)

data ThrottleBuffer b
  = ThrottleBuffer_Empty -- Empty conflicts with lens, and hiding it would require turning
                         -- on PatternSynonyms
  | ThrottleBuffer_Full b
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Data)

instance Semigroup b => Semigroup (ThrottleBuffer b) where
  x <> y = case x of
    ThrottleBuffer_Empty -> y
    ThrottleBuffer_Full b1 -> case y of
      ThrottleBuffer_Empty -> x
      ThrottleBuffer_Full b2 -> ThrottleBuffer_Full $ b1 <> b2
  {-# INLINE (<>) #-}

instance Semigroup b => Monoid (ThrottleBuffer b) where
  mempty = ThrottleBuffer_Empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Throttle an input event, ensuring that the output event doesn't occur more often than you are ready for it. If the input event occurs too
-- frequently, the output event will contain semigroup-based summaries of the input firings that happened since the last output firing.
-- If the output event has not occurred recently, occurrences of the input event will cause the output event to fire immediately.
-- The first parameter is a function that receives access to the output event, and should construct an event that fires when the receiver is
-- ready for more input.  For example, using @delay 20@ would give a simple time-based throttle.
--
-- NB: The provided lag function must *actually* delay the event.
throttleBatchWithLag :: (MonadFix m, MonadHold t m, PerformEvent t m, Semigroup a) => (Event t () -> m (Event t ())) -> Event t a -> m (Event t a)
-- Invariants:
-- * Immediate mode must turn off whenever output is produced.
-- * Output must be produced whenever immediate mode turns from on to off.
-- * Immediate mode can only go from off to on when the delayed event fires.
-- * Every input firing must go into either an immediate output firing or the
--   buffer, but not both.
-- * An existing full buffer must either stay in the buffer or go to output,
--   but not both.
throttleBatchWithLag lag e = do
  let f state x = case x of -- (Just $ newState, out)
        This a -> -- If only the input event fires
          case state of
            ThrottleState_Immediate -> -- and we're in immediate mode
              -- Immediate mode turns off, and the buffer is empty.
              -- We fire the output event with the input event value immediately.
              (Just $ ThrottleState_Buffered $ ThrottleBuffer_Empty, Just a)
            ThrottleState_Buffered b -> -- and we're not in immediate mode
              -- Immediate mode remains off, and we accumulate the input value.
              -- We don't fire the output event.
              (Just $ ThrottleState_Buffered $ b <> ThrottleBuffer_Full a, Nothing)
        That _ -> -- If only the delayed output event fires,
          case state of
            ThrottleState_Immediate -> -- and we're in immediate mode
              -- Nothing happens.
              (Nothing, Nothing)
            ThrottleState_Buffered ThrottleBuffer_Empty -> -- and the buffer is empty:
              -- Immediate mode turns back on, and the buffer remains empty.
              -- We don't fire.
              (Just ThrottleState_Immediate, Nothing)
            ThrottleState_Buffered (ThrottleBuffer_Full b) -> -- and the buffer is full:
              -- Immediate mode remains off, and the buffer is cleared.
              -- We fire with the buffered value.
              (Just $ ThrottleState_Buffered ThrottleBuffer_Empty, Just b)
        These a _ -> -- If both the input and delayed output event fire simultaneously:
          case state of
            ThrottleState_Immediate -> -- and we're in immediate mode
              -- Immediate mode turns off, and the buffer is empty.
              -- We fire with the input event's value, as it is the most recent we have seen at this moment.
              (Just $ ThrottleState_Buffered ThrottleBuffer_Empty, Just a)
            ThrottleState_Buffered ThrottleBuffer_Empty -> -- and the buffer is empty:
              -- Immediate mode stays off, and the buffer remains empty.
              -- We fire with the input event's value.
              (Just $ ThrottleState_Buffered ThrottleBuffer_Empty, Just a)
            ThrottleState_Buffered (ThrottleBuffer_Full b) -> -- and the buffer is full:
              -- Immediate mode remains off, and the buffer is cleared.
              -- We fire with everything including the buffered value.
              (Just $ ThrottleState_Buffered ThrottleBuffer_Empty, Just (b <> a))
  rec (_stateDyn, outE) <- mapAccumMaybeDyn f
        ThrottleState_Immediate -- We start in immediate mode with an empty buffer.
        (align e delayed)
      delayed <- lag (void outE)
  return outE

#ifdef USE_TEMPLATE_HASKELL
makeLensesWith (lensRules & simpleLenses .~ True) ''TickInfo
#else
tickInfo_lastUTC :: Lens' TickInfo UTCTime
tickInfo_lastUTC f (TickInfo x1 x2 x3) = (\y -> TickInfo y x2 x3) <$> f x1
{-# INLINE tickInfo_lastUTC #-}

tickInfo_n :: Lens' TickInfo Integer
tickInfo_n f (TickInfo x1 x2 x3) = (\y -> TickInfo x1 y x3) <$> f x2
{-# INLINE tickInfo_n #-}

tickInfo_alreadyElapsed :: Lens' TickInfo NominalDiffTime
tickInfo_alreadyElapsed f (TickInfo x1 x2 x3) = (\y -> TickInfo x1 x2 y) <$> f x3
{-# INLINE tickInfo_alreadyElapsed #-}
#endif
