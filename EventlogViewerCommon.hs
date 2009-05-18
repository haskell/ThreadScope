module EventlogViewerCommon
where
import Data.Array
import Data.IORef
import Data.List

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------
-- This datastructure is a duration-based representation of the event
-- loginformation where thread-runs and GCs are explicitly represented
-- by a single constructor identifying their start and end points.

data EventDuration
  = ThreadRun ThreadId Int ThreadStopStatus Timestamp Timestamp
  | GC Int Timestamp Timestamp
  | EV GHCEvents.Event

-------------------------------------------------------------------------------
-- The start time of an event.

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ startTime _ -> startTime
      GC _ startTime _ -> startTime
      EV event -> time event

-------------------------------------------------------------------------------
-- The emd time of an event.

endTimeOfEventDuration :: EventDuration -> Timestamp
endTimeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ _ endTime -> endTime
      GC _ _ endTime -> endTime
      EV event -> time event

-------------------------------------------------------------------------------
-- This is a tree-based view of the event information to allow
-- abstracted representations of the running events and the GC events.
-- Each split node will record all the events in the first half of the
-- time span represented by the node in the LHS sub-tree and the
-- remainder in the RHS sub-tree i.e. the data-structure represents
-- a binary-tree split on the time axis.
-- This node also record the average amount of time during the entire 
-- run-span for which a HEC is running
-- a thread and also the the average amount of time spent in GC.
-- The EventtTree information is used to organize events for a single HEC.

data EventTree
  = EventSplit Timestamp -- The start time of this run-span
               Timestamp -- The time used to split the events into two parts
               Timestamp -- The end time of this run-span
               EventTree -- The LHS split <= split-time
               EventTree -- The RHS split > split-time
               Int       -- The number of events under this node
               Timestamp -- The total amount of time spent running a thread
               Timestamp -- The total amount of time spend in GC
  | EventTreeLeaf [EventDuration]

-------------------------------------------------------------------------------

splitEvents :: [EventDuration] -> EventTree
splitEvents [] = EventTreeLeaf [] -- The case for an empty list of events
splitEvents [e1] = EventTreeLeaf [e1] -- The case for a singleton list
splitEvents eventList
  = if duration > 0 && len > 1000 then -- threshold for leaf size 
      EventSplit startTime
                 splitTime 
                 endTime 
                 leftSplit
                 rightSplit
                 len -- Number of events under this node
                 runTime
                 gcTime
    else -- All events at the same time so don't split or leaf
         -- size threshold has been reached
      EventTreeLeaf eventList
    where
    startTime = timeOfEventDuration (head eventList)
    endTime = timeOfEventDuration (last eventList)
    splitTime = startTime + (endTime - startTime) `div` 2
    duration = endTime - startTime
    len = length eventList
    lhs = [e | e <- eventList, timeOfEventDuration e <= splitTime]
    rhs = [e | e <- eventList, timeOfEventDuration e > splitTime]
    leftSplit = splitEvents lhs
    rightSplit = splitEvents rhs
    runTime = runTimeOf leftSplit + runTimeOf rightSplit
    gcTime = gcTimeOf leftSplit + gcTimeOf rightSplit

-- Splitting:
-- [0]         -> [[0], []]
-- [0,1]       -> [[0], [1]]
-- [0,1,2]     -> [[0,1], [2]]
-- [0,1,2,3]   -> [[0,1], [2,3]]
-- [0,1,2,3,4] -> [[0,1,2], [3,4]]

-------------------------------------------------------------------------------

runTimeOf :: EventTree -> Timestamp
runTimeOf (EventSplit _ _ _ _ _ _ runTime _) = runTime
runTimeOf (EventTreeLeaf eventList)
  = sum [e - s | ThreadRun _ _ _ s e <- eventList]

-------------------------------------------------------------------------------

gcTimeOf :: EventTree -> Timestamp
gcTimeOf (EventSplit _ _ _ _ _ _ _ gcTime) = gcTime
gcTimeOf (EventTreeLeaf eventList)
  = sum [e - s | GC _ s e <- eventList]

-------------------------------------------------------------------------------

lastEventTime :: EventTree -> Timestamp
lastEventTime (EventSplit _ _ endTime _ _ _ _ _) = endTime
lastEventTime (EventTreeLeaf eventList)
  = endTimeOfEventDuration (last eventList)

-------------------------------------------------------------------------------


-- An EventArray stores events for a single HEC
type EventArray = Array Int EventDuration

-- The HEC data structure is a list of pairs where each pair records
-- the unqiue ID of a HEC and its event information represented
-- using the EventTree data-structure.
type HECs = [(Int, EventTree)]

type MaybeHECsIORef = IORef  (Maybe HECs)

-------------------------------------------------------------------------------

ennumerateCapabilities :: [GHCEvents.Event] -> [Int]
ennumerateCapabilities events
  = sort (nub (map (cap . spec) events))

-------------------------------------------------------------------------------

-- Find the last timestamp value 
findLastTxValue :: HECs -> Timestamp
findLastTxValue hecs
  = maximum (map (lastEventTime . snd) hecs)

-------------------------------------------------------------------------------

arrayLast a 
  = a!lastIdx
    where
    (_, lastIdx) = bounds a

-------------------------------------------------------------------------------


-- Origin for graph

ox :: Int
ox = 10

oy :: Int
oy = 30

-- Origin for capability bars

oycap :: Int
oycap = 60

-- Gap betweem capabilities

gapcap :: Int
gapcap = 55

-- Bar sizes

barHeight :: Int
barHeight = 20

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a micro-second value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0

-------------------------------------------------------------------------------
-- Scale a timetamp value by a value s giving an Int

tsScale :: Timestamp -> Double -> Int
tsScale t s = scaleIntegerBy (toInteger t) s

-------------------------------------------------------------------------------

scaleIntegerBy :: Integer -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------
