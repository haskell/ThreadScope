module Events.EventTree (
     DurationTree(..),
     mkDurationTree,

     runTimeOf, gcTimeOf,
     reportDurationTree,
     durationTreeCountNodes,
     durationTreeMaxDepth,

     EventTree(..), EventNode(..),
     mkEventTree,
     reportEventTree, eventTreeMaxDepth,
  ) where

import Events.EventDuration

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event)

import Text.Printf
import Control.Exception (assert)

-------------------------------------------------------------------------------

-- We map the events onto a binary search tree, so that we can easily
-- find the events that correspond to a particular view of the
-- timeline.  Additionally, each node of the tree contains a summary
-- of the information below it, so that we can render views at various
-- levels of resolution.  For example, if a tree node would represent
-- less than one pixel on the display, there is no point is descending
-- the tree further.

-- We only split at event boundaries; we never split an event into
-- multiple pieces.  Therefore, the binary tree is only roughly split
-- by time, the actual split depends on the distribution of events
-- below it.

data DurationTree
  = DurationSplit
        {-#UNPACK#-}!Timestamp -- The start time of this run-span
        {-#UNPACK#-}!Timestamp -- The time used to split the events into two parts
        {-#UNPACK#-}!Timestamp -- The end time of this run-span
        DurationTree -- The LHS split; all events lie completely between
                     -- start and split
        DurationTree -- The RHS split; all events lie completely between
                     -- split and end
        {-#UNPACK#-}!Timestamp -- The total amount of time spent running a thread
        {-#UNPACK#-}!Timestamp -- The total amount of time spend in GC

  | DurationTreeLeaf
        EventDuration

  | DurationTreeEmpty

  deriving Show

-------------------------------------------------------------------------------

mkDurationTree :: [EventDuration] -> Timestamp -> DurationTree
mkDurationTree es endTime =
  -- trace (show tree) $
  tree
 where
  tree = splitDurations es endTime

splitDurations :: [EventDuration] -- events
               -> Timestamp       -- end time of last event in the list
               -> DurationTree
splitDurations [] _endTime =
  -- if len /= 0 then error "splitDurations0" else
  DurationTreeEmpty  -- The case for an empty list of events.

splitDurations [e] _entTime =
  DurationTreeLeaf e

splitDurations es endTime
  | null rhs
  = splitDurations es lhs_end

  | null lhs
  = error $
    printf "splitDurations: null lhs: len = %d, startTime = %d, endTime = %d\n"
      (length es) startTime endTime
    ++ '\n': show es

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    assert (length lhs + length rhs == length es) $
    DurationSplit startTime
               lhs_end
               endTime
               ltree
               rtree
               runTime
               gcTime
    where
    startTime = startTimeOf (head es)
    splitTime = startTime + (endTime - startTime) `div` 2

    (lhs, lhs_end, rhs) = splitDurationList es [] splitTime 0

    ltree = splitDurations lhs lhs_end
    rtree = splitDurations rhs endTime

    runTime = runTimeOf ltree + runTimeOf rtree
    gcTime  = gcTimeOf  ltree + gcTimeOf  rtree


splitDurationList :: [EventDuration]
                  -> [EventDuration]
                  -> Timestamp
                  -> Timestamp
                  -> ([EventDuration], Timestamp, [EventDuration])
splitDurationList []  acc !_tsplit !tmax
  = (reverse acc, tmax, [])
splitDurationList [e] acc !_tsplit !tmax
  -- Just one event left: put it on the right. This ensures that we
  -- have at least one event on each side of the split.
  = (reverse acc, tmax, [e])
splitDurationList (e:es) acc !tsplit !tmax
  | tstart <= tsplit  -- pick all events that start at or before the split
  = splitDurationList es (e:acc) tsplit (max tmax tend)
  | otherwise
  = (reverse acc, tmax, e:es)
  where
    tstart = startTimeOf e
    tend   = endTimeOf e

-------------------------------------------------------------------------------

runTimeOf :: DurationTree -> Timestamp
runTimeOf (DurationSplit _ _ _ _ _ runTime _) = runTime
runTimeOf (DurationTreeLeaf e) | ThreadRun{} <- e = durationOf e
runTimeOf _ = 0

-------------------------------------------------------------------------------

gcTimeOf :: DurationTree -> Timestamp
gcTimeOf (DurationSplit _ _ _ _ _ _ gcTime) = gcTime
gcTimeOf (DurationTreeLeaf e) | isGCDuration e = durationOf e
gcTimeOf _ = 0

-------------------------------------------------------------------------------

reportDurationTree :: Int -> DurationTree -> IO ()
reportDurationTree hecNumber eventTree
  = putStrLn ("HEC " ++ show hecNumber ++ reportText)
    where
    reportText = " nodes = " ++ show (durationTreeCountNodes eventTree) ++
                 " max depth = " ++ show (durationTreeMaxDepth eventTree)

-------------------------------------------------------------------------------

durationTreeCountNodes :: DurationTree -> Int
durationTreeCountNodes (DurationSplit _ _ _ lhs rhs _ _)
   = 1 + durationTreeCountNodes lhs + durationTreeCountNodes rhs
durationTreeCountNodes _ = 1

-------------------------------------------------------------------------------

durationTreeMaxDepth :: DurationTree -> Int
durationTreeMaxDepth (DurationSplit _ _ _ lhs rhs _ _)
  = 1 + durationTreeMaxDepth lhs `max` durationTreeMaxDepth rhs
durationTreeMaxDepth _ = 1

-------------------------------------------------------------------------------

data EventTree
    = EventTree
        {-#UNPACK#-}!Timestamp -- The start time of this run-span
        {-#UNPACK#-}!Timestamp -- The end   time of this run-span
        EventNode

data EventNode
  = EventSplit
        {-#UNPACK#-}!Timestamp -- The time used to split the events into two parts
        EventNode -- The LHS split; all events lie completely between
                  -- start and split
        EventNode -- The RHS split; all events lie completely between
                  -- split and end

  | EventTreeLeaf [GHC.Event]
        -- sometimes events happen "simultaneously" (at the same time
        -- given the resolution of our clock source), so we can't
        -- separate them.

  | EventTreeOne GHC.Event
        -- This is a space optimisation for the common case of
        -- EventTreeLeaf [e].

mkEventTree :: [GHC.Event] -> Timestamp -> EventTree
mkEventTree es endTime =
  EventTree s e $
  -- trace (show tree) $
  tree
 where
  tree = splitEvents es endTime
  (s,e) = if null es then (0,0) else (time (head es), endTime)

splitEvents :: [GHC.Event] -- events
            -> Timestamp       -- end time of last event in the list
            -> EventNode
splitEvents []  !_endTime =
  -- if len /= 0 then error "splitEvents0" else
  EventTreeLeaf []   -- The case for an empty list of events

splitEvents [e] !_endTime =
  EventTreeOne e

splitEvents es !endTime
  | duration == 0
  = EventTreeLeaf es

  | null rhs
  = splitEvents es lhs_end

  | null lhs
  = error $
    printf "splitEvents: null lhs: len = %d, startTime = %d, endTime = %d\n"
      (length es) startTime endTime
    ++ '\n': show es

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    assert (length lhs + length rhs == length es) $
    EventSplit (time (head rhs))
               ltree
               rtree
    where
    -- | Integer division, rounding up.
    divUp :: Timestamp -> Timestamp -> Timestamp
    divUp n k = (n + k - 1) `div` k
    startTime = time (head es)
    splitTime = startTime + (endTime - startTime) `divUp` 2
    duration  = endTime - startTime

    (lhs, lhs_end, rhs) = splitEventList es [] splitTime 0

    ltree = splitEvents lhs lhs_end
    rtree = splitEvents rhs endTime


splitEventList :: [GHC.Event]
               -> [GHC.Event]
               -> Timestamp
               -> Timestamp
               -> ([GHC.Event], Timestamp, [GHC.Event])
splitEventList []  acc !_tsplit !tmax
  = (reverse acc, tmax, [])
splitEventList [e] acc !_tsplit !tmax
  -- Just one event left: put it on the right. This ensures that we
  -- have at least one event on each side of the split.
  = (reverse acc, tmax, [e])
splitEventList (e:es) acc !tsplit !tmax
  | t <= tsplit  -- pick all events that start at or before the split
  = splitEventList es (e:acc) tsplit (max tmax t)
  | otherwise
  = (reverse acc, tmax, e:es)
  where
    t = time e

-------------------------------------------------------------------------------

reportEventTree :: Int -> EventTree -> IO ()
reportEventTree hecNumber (EventTree _ _ eventTree)
  = putStrLn ("HEC " ++ show hecNumber ++ reportText)
    where
    reportText = " nodes = " ++ show (eventTreeCountNodes eventTree) ++
                 " max depth = " ++ show (eventNodeMaxDepth eventTree)

-------------------------------------------------------------------------------

eventTreeCountNodes :: EventNode -> Int
eventTreeCountNodes (EventSplit _ lhs rhs)
   = 1 + eventTreeCountNodes lhs + eventTreeCountNodes rhs
eventTreeCountNodes _ = 1

-------------------------------------------------------------------------------

eventTreeMaxDepth :: EventTree -> Int
eventTreeMaxDepth (EventTree _ _ t) = eventNodeMaxDepth t

eventNodeMaxDepth :: EventNode -> Int
eventNodeMaxDepth (EventSplit _ lhs rhs)
  = 1 + eventNodeMaxDepth lhs `max` eventNodeMaxDepth rhs
eventNodeMaxDepth _ = 1
