-------------------------------------------------------------------------------
--- $Id: EventDuration.hs#4 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/EventDuration.hs $
-------------------------------------------------------------------------------

-- This module supports a duration-based data-type to represent thread
-- execution and GC information.

module EventDuration ( eventsToDurations ) where

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import EventlogViewerCommon

-------------------------------------------------------------------------------

eventsToDurations :: [GHCEvents.Event] -> [EventDuration]
eventsToDurations []     = []
eventsToDurations (event : events) =
  case spec event of
     RunThread{thread=t} -> runDuration t : rest
     StopThread{}  -> rest
     StartGC       -> GC (time event) (findGCEndTime events) : rest
     EndGC{}       -> rest
     _otherEvent   -> EV event : rest
  where
    rest = eventsToDurations events

    runDuration t = ThreadRun t s (time event) endTime
       where (endTime, s) = findRunThreadTime events

-------------------------------------------------------------------------------

findRunThreadTime :: [GHCEvents.Event] -> (Timestamp, ThreadStopStatus)
findRunThreadTime [] = error "findRunThreadTime"
findRunThreadTime (e : es)
  = case spec e of
      StopThread{status=s} -> (time e, s)
      _                    -> findRunThreadTime es

-------------------------------------------------------------------------------

findGCEndTime :: [GHCEvents.Event] -> Timestamp
findGCEndTime [] = error "findRunThreadTime"
findGCEndTime (e : es)
  = case spec e of
      EndGC -> time e
      _     -> findGCEndTime es

-------------------------------------------------------------------------------
