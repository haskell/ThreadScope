module EventlogViewerCommon
where
import Data.Array
import Data.IORef
import Data.List
import Debug.Trace
import Text.Printf

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------
-- This datastructure is a duration-based representation of the event
-- loginformation where thread-runs and GCs are explicitly represented
-- by a single constructor identifying their start and end points.

data EventDuration
  = ThreadRun ThreadId ThreadStopStatus Timestamp Timestamp
  | GC Timestamp Timestamp
  | EV GHCEvents.Event

-------------------------------------------------------------------------------
-- The start time of an event.

timeOfEventDuration :: EventDuration -> Timestamp
timeOfEventDuration ed
  = case ed of
      ThreadRun _ _ startTime _ -> startTime
      GC startTime _ -> startTime
      EV event -> time event

-------------------------------------------------------------------------------
-- The emd time of an event.

endTimeOfEventDuration :: EventDuration -> Timestamp
endTimeOfEventDuration ed
  = case ed of
      ThreadRun _ _ _ endTime -> endTime
      GC _ endTime -> endTime
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
splitEvents es = splitEvents' es (length es) (endTimeOfEventDuration (last es))

splitEvents' :: [EventDuration] -- events
             -> Int             -- length of list above
             -> Timestamp       -- end time of last event in the list
             -> EventTree
splitEvents' []   len endTime = 
  -- if len /= 0 then error "splitEvents'0" else
  EventTreeLeaf []   -- The case for an empty list of events
splitEvents' es   len endTime
  | duration == 0 || len < 2
  = -- if len /= length es then error (printf "splitEvents'3; %d %d" len (length es))  else 
    -- trace (printf "leaf: len = %d, startTime = %d\n" len startTime) $ 
    EventTreeLeaf es
  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    -- if len /= length es || length lhs + length rhs /= len then error (printf "splitEvents'3; %d %d %d %d %d" len (length es) (length lhs) lhs_len (length rhs))  else 
    EventSplit startTime
               lhs_end
               endTime 
               ltree
               rtree
               len -- Number of events under this node
               runTime
               gcTime
    where
    startTime = timeOfEventDuration (head es)
    splitTime = startTime + (endTime - startTime) `div` 2
    duration  = endTime - startTime

    (lhs, lhs_len, lhs_end, rhs) = splitEventList es [] splitTime splitTime 0

    ltree = splitEvents' lhs lhs_len lhs_end
    rtree = splitEvents' rhs (len - lhs_len) endTime

    runTime = runTimeOf ltree + runTimeOf rtree
    gcTime  = gcTimeOf  ltree + gcTimeOf  rtree

-- Splitting:
-- [0]         -> [[0], []]
-- [0,1]       -> [[0], [1]]
-- [0,1,2]     -> [[0,1], [2]]
-- [0,1,2,3]   -> [[0,1], [2,3]]
-- [0,1,2,3,4] -> [[0,1,2], [3,4]]

splitEventList :: [EventDuration]
               -> [EventDuration]
               -> Timestamp
               -> Timestamp
               -> Int
               -> ([EventDuration], Int, Timestamp, [EventDuration])
splitEventList []     acc tsplit tlast len = error "splitEventList"
splitEventList [e]    acc tsplit tlast len
  = (reverse acc, len, tlast, [e])
  -- if there's just one event left in the list, put it on the rhs.  This
  -- ensures that we make some progress; otherwise we can get situations
  -- where the left subtree is exactly the same as the current subtree,
  -- and splitting will not terminate.
splitEventList (e:es) acc tsplit tlast len
  | timeOfEventDuration e <= tsplit
  = splitEventList es (e:acc) tsplit (endTimeOfEventDuration e) (len+1)
  | otherwise
  = (reverse acc, len, tlast, e:es)

-------------------------------------------------------------------------------

runTimeOf :: EventTree -> Timestamp
runTimeOf (EventSplit _ _ _ _ _ _ runTime _) = runTime
runTimeOf (EventTreeLeaf eventList)
  = sum [e - s | ThreadRun _ _ s e <- eventList]

-------------------------------------------------------------------------------

eventTreeEvents :: EventTree -> Int
eventTreeEvents (EventSplit _ _ _ _ _ len _ _) = len
eventTreeEvents (EventTreeLeaf eventList) = length eventList

-------------------------------------------------------------------------------

gcTimeOf :: EventTree -> Timestamp
gcTimeOf (EventSplit _ _ _ _ _ _ _ gcTime) = gcTime
gcTimeOf (EventTreeLeaf eventList)
  = sum [e - s | GC s e <- eventList]

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
ennumerateCapabilities events = trace ("ennumerateCapabilities: " ++ show n_caps) $ [0.. n_caps-1]
  where n_caps = head [ caps | GHCEvents.Event _ _ (GHCEvents.Startup caps) <- events ]

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

-- Bar height

barHeight :: Int
barHeight = 20

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
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

scaleIntegerBy :: Integral int => int -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------

