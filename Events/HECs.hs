{-# LANGUAGE CPP #-}
module Events.HECs (
    HECs(..),
    Event,
    CapEvent,
    Timestamp,

    eventIndexToTimestamp,
    timestampToEventIndex,
    extractUserMarkers,
    histogram,
    histogramCounts,
  ) where

import Events.EventTree
import Events.SparkTree
import GHC.RTS.Events

import Data.Array
import qualified Data.IntMap as IM
import qualified Data.List as L

-----------------------------------------------------------------------------

-- all the data from a .eventlog file
data HECs = HECs {
       hecCount         :: Int,
       hecTrees         :: [(DurationTree, EventTree, SparkTree)],
       hecEventArray    :: Array Int CapEvent,
       hecLastEventTime :: Timestamp,
       maxSparkPool     :: Double,
       minXHistogram    :: Int,
       maxXHistogram    :: Int,
       maxYHistogram    :: Timestamp,
       durHistogram     :: [(Timestamp, Int, Timestamp)],
       perfNames        :: IM.IntMap String
     }

-----------------------------------------------------------------------------

eventIndexToTimestamp :: HECs -> Int -> Timestamp
eventIndexToTimestamp HECs{hecEventArray=arr} n =
  time (ce_event (arr ! n))

timestampToEventIndex :: HECs -> Timestamp -> Int
timestampToEventIndex HECs{hecEventArray=arr} ts =
    search l (r+1)
  where
    (l,r) = bounds arr

    search !l !r
      | (r - l) <= 1 = if ts > time (ce_event (arr!l)) then r else l
      | ts < tmid    = search l mid
      | otherwise    = search mid r
      where
        mid  = l + (r - l) `quot` 2
        tmid = time (ce_event (arr!mid))

extractUserMarkers :: HECs -> [(Timestamp, String)]
extractUserMarkers hecs =
  [ (ts, mark)
  | CapEvent _ (Event ts (UserMarker mark)) <- elems (hecEventArray hecs) ]

-- | Sum durations in the same buckets to form a histogram.
histogram :: [(Int, Timestamp)] -> [(Int, Timestamp)]
histogram durs = IM.toList $ fromListWith' (+) durs

-- | Sum durations and spark counts in the same buckets to form a histogram.
histogramCounts :: [(Int, (Timestamp, Int))] -> [(Int, (Timestamp, Int))]
histogramCounts durs =
  let agg (dur1, count1) (dur2, count2) =
        -- bangs needed to avoid stack overflow
        let !dur = dur1 + dur2
            !count = count1 + count2
        in (dur, count)
  in IM.toList $ fromListWith' agg durs

fromListWith' :: (a -> a -> a) -> [(Int, a)] -> IM.IntMap a
fromListWith' f xs =
    L.foldl' ins IM.empty xs
  where
#if MIN_VERSION_containers(0,4,1)
    ins t (k,x) = IM.insertWith' f k x t
#else
    ins t (k,x) =
      let r = IM.insertWith f k x t
          v = r IM.! k
      in v `seq` r
#endif
