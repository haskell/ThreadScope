-- This module supports a duration-based data-type to represent thread
-- execution and GC information.

module EventDuration ( 
    EventDuration(..),
    timeOfEventDuration,
    endTimeOfEventDuration,
    eventsToDurations
  ) where

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------
-- This datastructure is a duration-based representation of the event
-- loginformation where thread-runs and GCs are explicitly represented
-- by a single constructor identifying their start and end points.

data EventDuration
  = ThreadRun {-#UNPACK#-}!ThreadId 
              ThreadStopStatus
              {-#UNPACK#-}!Timestamp
              {-#UNPACK#-}!Timestamp

  | GC {-#UNPACK#-}!Timestamp
       {-#UNPACK#-}!Timestamp

  | EV GHCEvents.Event
  deriving Show

-------------------------------------------------------------------------------
-- The start time of an event.

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ startTime _ -> startTime
      GC startTime _            -> startTime
      EV event                  -> time event

-------------------------------------------------------------------------------
-- The emd time of an event.

endTimeOfEventDuration :: EventDuration -> Timestamp
endTimeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ endTime -> endTime
      GC _ endTime            -> endTime
      EV event                -> time event

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
