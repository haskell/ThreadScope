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
import StartTimes

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
        StopThread{thread=t, GHC.RTS.Events.status=s}
                      -> runBar t s : rest
        EndGC         -> GC gcStartTime (time event) : rest
        RunThread _   -> rest
        StartGC       -> rest
        otherEvent    -> EV event : rest
    where
    event = eventArray!idx
    rest = eventArrayToDuration' (idx+1) eventArray
    (_, lastIdx) = bounds eventArray
    startTime = findRunThreadTime eventArray (idx-1)
    runBar t s = ThreadRun t s startTime (time event)
    gcStartTime = findStartGCTime eventArray (idx-1)

-------------------------------------------------------------------------------
