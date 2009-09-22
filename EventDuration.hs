-------------------------------------------------------------------------------
--- $Id: EventDuration.hs#4 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/EventDuration.hs $
-------------------------------------------------------------------------------

-- This module supports a duration-based data-type to represent thread
-- execution and GC information.

module EventDuration
where

import Data.Array

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import EventlogViewerCommon

-------------------------------------------------------------------------------

eventArrayToDurationArray :: Array Int GHCEvents.Event -> EventArray
eventArrayToDurationArray eventArray
  = listArray (0, length durationList-1)   durationList
    where
    durationList =  eventArrayToDuration eventArray

-------------------------------------------------------------------------------

eventArrayToDuration :: Array Int GHCEvents.Event -> [EventDuration]
eventArrayToDuration = eventArrayToDuration' 0

-------------------------------------------------------------------------------

eventArrayToDuration' :: Int -> Array Int GHCEvents.Event -> [EventDuration]
eventArrayToDuration' idx eventArray 
  = if idx > lastIdx then
      []
    else
      case spec event of
        RunThread{thread=t} -> runDuration t : rest
        StopThread{}  -> rest
        StartGC       -> gcDuration : rest
        EndGC{}       -> rest
        _otherEvent   -> EV event : rest
    where
    event = eventArray!idx
    rest = eventArrayToDuration' (idx+1) eventArray
    (_, lastIdx) = bounds eventArray

    runDuration t = ThreadRun t s (time event) endTime
       where (endTime, s) = findRunThreadTime eventArray (idx+1)

    gcDuration = GC (time event) endTime
       where endTime = findGCEndTime eventArray (idx+1)

-------------------------------------------------------------------------------

findRunThreadTime :: Array Int GHCEvents.Event -> Int
		  -> (Timestamp, ThreadStopStatus)
findRunThreadTime eventArray idx
  | idx >= lastIdx = error "findRunThreadTime"
  | otherwise
  = case spec (eventArray!idx) of
      StopThread{status=s} -> (time (eventArray!idx), s)
      _                    -> findRunThreadTime eventArray (idx+1)
  where
   (_, lastIdx) = bounds eventArray

-------------------------------------------------------------------------------

findGCEndTime :: Array Int GHCEvents.Event -> Int -> Timestamp
findGCEndTime eventArray idx
  | idx >= lastIdx = error "findRunThreadTime"
  | otherwise
  = case spec (eventArray!idx) of
      EndGC -> time (eventArray!idx)
      _     -> findGCEndTime eventArray (idx+1)
  where
   (_, lastIdx) = bounds eventArray

-------------------------------------------------------------------------------
