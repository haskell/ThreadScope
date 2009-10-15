-- This module supports a duration-based data-type to represent thread
-- execution and GC information.

module EventDuration ( 
    EventDuration(..),
    isGCDuration,
    timeOfEventDuration,
    endTimeOfEventDuration,
    eventsToDurations
  ) where

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event,GCWork,GCIdle)

-------------------------------------------------------------------------------
-- This datastructure is a duration-based representation of the event
-- loginformation where thread-runs and GCs are explicitly represented
-- by a single constructor identifying their start and end points.

data EventDuration
  = ThreadRun {-#UNPACK#-}!ThreadId 
              ThreadStopStatus
              {-#UNPACK#-}!Timestamp
              {-#UNPACK#-}!Timestamp

  | GCStart {-#UNPACK#-}!Timestamp
            {-#UNPACK#-}!Timestamp

  | GCWork  {-#UNPACK#-}!Timestamp
            {-#UNPACK#-}!Timestamp

  | GCIdle  {-#UNPACK#-}!Timestamp
            {-#UNPACK#-}!Timestamp

  | GCEnd   {-#UNPACK#-}!Timestamp
            {-#UNPACK#-}!Timestamp

  | EV GHC.Event
  deriving Show

{-
           GCStart     GCWork      GCIdle      GCEnd
  gc start -----> work -----> idle ------+> done -----> gc end
                   |                     | 
                   `-------<-------<-----'
-}

isGCDuration :: EventDuration -> Bool
isGCDuration GCStart{} = True
isGCDuration GCWork{}  = True
isGCDuration GCIdle{}  = True
isGCDuration GCEnd{}   = True
isGCDuration _         = False

-------------------------------------------------------------------------------
-- The start time of an event.

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ startTime _ -> startTime
      GCStart startTime _       -> startTime
      GCWork  startTime _       -> startTime
      GCIdle  startTime _       -> startTime
      GCEnd   startTime _       -> startTime
      EV event                  -> time event

-------------------------------------------------------------------------------
-- The emd time of an event.

endTimeOfEventDuration :: EventDuration -> Timestamp
endTimeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ endTime -> endTime
      GCStart _ endTime       -> endTime
      GCWork  _ endTime       -> endTime
      GCIdle  _ endTime       -> endTime
      GCEnd   _ endTime       -> endTime
      EV event                -> time event

-------------------------------------------------------------------------------

eventsToDurations :: [GHC.Event] -> [EventDuration]
eventsToDurations []     = []
eventsToDurations (event : events) =
  case spec event of
     RunThread{thread=t} -> runDuration t : rest
     StopThread{}  -> rest
     StartGC       -> gcStart (time event) events
     EndGC{}       -> rest
     _otherEvent   -> EV event : rest
  where
    rest = eventsToDurations events

    runDuration t = ThreadRun t s (time event) endTime
       where (endTime, s) = findRunThreadTime events

gcStart :: Timestamp -> [GHC.Event] -> [EventDuration]
gcStart t0 [] = []
gcStart t0 (event : events) =
  case spec event of
    GHC.GCWork{} -> GCStart t0 t1 : gcWork t1 events
    GHC.GCIdle{} -> GCStart t0 t1 : gcIdle t1 events
    GHC.GCDone{} -> GCStart t0 t1 : gcDone t1 events
    GHC.EndGC{}  -> GCStart t0 t1 : eventsToDurations events
    RunThread{}  -> GCStart t0 t1 : eventsToDurations (event : events)
    _other       -> EV event : gcStart t0 events
 where 
        t1 = time event

gcWork :: Timestamp -> [GHC.Event] -> [EventDuration]
gcWork t0 [] = []
gcWork t0 (event : events) =
  case spec event of
    GHC.GCWork{} -> gcWork t0 events
    GHC.GCIdle{} -> GCWork t0 t1 : gcIdle t1 events
    GHC.GCDone{} -> GCWork t0 t1 : gcDone t1 events
    GHC.EndGC{}  -> GCWork t0 t1 : eventsToDurations events
    RunThread{}  -> GCWork t0 t1 : eventsToDurations (event : events)
    _other       -> EV event : gcStart t0 events
 where 
        t1 = time event

gcIdle :: Timestamp -> [GHC.Event] -> [EventDuration]
gcIdle t0 [] = []
gcIdle t0 (event : events) =
  case spec event of
    GHC.GCIdle{} -> gcIdle t0 events
    GHC.GCWork{} -> GCIdle t0 t1 : gcWork t1 events
    GHC.GCDone{} -> GCIdle t0 t1 : gcDone t1 events
    GHC.EndGC{}  -> GCIdle t0 t1 : eventsToDurations events
    RunThread{}  -> GCIdle t0 t1 : eventsToDurations (event : events)
    _other       -> EV event : gcStart t0 events
 where 
        t1 = time event

gcDone :: Timestamp -> [GHC.Event] -> [EventDuration]
gcDone t0 [] = []
gcDone t0 (event : events) =
  case spec event of
    GHC.GCDone{} -> gcDone t0 events
    GHC.GCWork{} -> GCEnd t0 t1 : gcWork t1 events
    GHC.GCIdle{} -> GCEnd t0 t1 : gcIdle t1 events
    GHC.EndGC{}  -> GCEnd t0 t1 : eventsToDurations events
    RunThread{}  -> GCEnd t0 t1 : eventsToDurations (event : events)
    _other       -> EV event : gcStart t0 events
 where 
        t1 = time event

-------------------------------------------------------------------------------

findRunThreadTime :: [GHC.Event] -> (Timestamp, ThreadStopStatus)
findRunThreadTime [] = error "findRunThreadTime"
findRunThreadTime (e : es)
  = case spec e of
      StopThread{status=s} -> (time e, s)
      _                    -> findRunThreadTime es

-------------------------------------------------------------------------------
