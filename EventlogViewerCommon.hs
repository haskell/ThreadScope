-------------------------------------------------------------------------------
--- $Id: EventlogViewerCommon.hs#3 2009/03/23 17:11:32 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/EventlogViewerCommon.hs $
-------------------------------------------------------------------------------

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

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ startTime _ -> startTime
      GC _ startTime _ -> startTime
      EV event -> time event

-------------------------------------------------------------------------------
-- This is a tree-based view of the event information to allow
-- abstracted representations of the running events and the GC events.
-- Each split node will split half of the events down the LHS sub-tree
-- and the other half of the events down the RHS sub-tree. The timestamp
-- of the last event in the LHS subtree is also recorded as the the total
-- number of events under this node. This node also record the average
-- amount of time during the entire run-span for which a HEC is running
-- a thread and also the the average amount of time spent in GC.

data EventTree
  = EventSplit Timestamp -- The start time of this run-span
               Timestamp -- The time used to split the events into two parts
               Timestamp -- The end time of this run-span
               EventTree -- The LHS split <= split-time
               EventTree -- The RHS split > split-time
               Int       -- The number of events under this node
  | EventTreeLeaf [EventDuration]

-------------------------------------------------------------------------------

splitEvents :: [EventDuration] -> EventTree
splitEvents [] = EventTreeLeaf [] -- The case for an empty list of events
splitEvents [e1] = EventTreeLeaf [e1] -- The case for a singleton list
splitEvents eventList
  = if duration > 0 then
      EventSplit startTime
                 splitTime 
                 endTime 
                 (splitEvents lhs)
                 (splitEvents rhs)
                 len -- Number of events under this node
    else -- All events at the same time so don't split
      EventTreeLeaf eventList
    where
    startTime = timeOfEventDuration (head eventList)
    endTime = timeOfEventDuration (last eventList)
    duration = endTime - startTime
    len = length eventList
    splitIndex = len `div` 2
    splitValue = eventList!!splitIndex
    splitTime = timeOfEventDuration splitValue
    lhs = take (splitIndex+1) eventList
    rhs = drop (splitIndex+1) eventList    

-- Splitting:
-- [0]         -> [[0], []]
-- [0,1]       -> [[0], [1]]
-- [0,1,2]     -> [[0,1], [2]]
-- [0,1,2,3]   -> [[0,1], [2,3]]
-- [0,1,2,3,4] -> [[0,1,2], [3,4]]

-------------------------------------------------------------------------------


-- An EventArray stores events for a single HEC
type EventArray = Array Int EventDuration

-- HECs is a list of events for each HEC
type HECs = [(Int, EventArray)]

type MaybeHECsIORef = IORef  (Maybe HECs)

-------------------------------------------------------------------------------

ennumerateCapabilities :: [GHCEvents.Event] -> [Int]
ennumerateCapabilities events
  = sort (nub (map (cap . spec) events))

-------------------------------------------------------------------------------

-- Find the last timestamp value (in microseconds)
findLastTxValue :: HECs -> Integer
findLastTxValue hecs
  = maximum (map (eventDuration2ms . arrayLast) (map snd hecs))

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

-- Scaling down from ts ns 1e-9 values to microsecond 1e-6 values
scaleDown :: Integer
scaleDown = 1000

-------------------------------------------------------------------------------

eScale :: GHCEvents.Event -> Double -> Int
eScale e s = scaleIntegerBy (event2ms e) s

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a micro-second value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0

-------------------------------------------------------------------------------
-- Scale a timetamp value (in microsecond) by a value s giving an Int

tsScale :: Timestamp -> Double -> Int
tsScale t s = scaleIntegerBy (ts2ms t) s

-------------------------------------------------------------------------------
-- Extact a timestamp value from an event and return as microseconds

event2ms :: GHCEvents.Event -> Integer
event2ms event = ts2ms (time event)

-------------------------------------------------------------------------------

eventDuration2ms :: EventDuration -> Integer
eventDuration2ms ed
  = case ed of
      ThreadRun _ _ _ startTime _ -> ts2ms startTime
      GC _ startTime _ -> ts2ms startTime
      EV ev -> event2ms ev

-------------------------------------------------------------------------------

endTime2ms :: EventDuration -> Integer
endTime2ms ed
  = case ed of
      ThreadRun _ _ _ _ endTime -> ts2ms endTime
      GC _ _ endTime -> ts2ms endTime
      EV ev -> event2ms ev

-------------------------------------------------------------------------------

ts2ms :: Timestamp -> Integer
ts2ms t = (fromIntegral t) `div` scaleDown

-------------------------------------------------------------------------------

ts2msInt :: Timestamp -> Int
ts2msInt t = fromIntegral (ts2ms t)

-------------------------------------------------------------------------------

scaleIntegerBy :: Integer -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------
