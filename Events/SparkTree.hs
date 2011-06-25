module Events.SparkTree (
     SparkTree(..), SparkNode(..),
     eventsToSparkDurations,
     mkSparkTree,
     sparkProfile,
     sparkTreeMaxDepth,
  ) where

import qualified Events.SparkCounters as SparkCounters

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events (Timestamp)

import Text.Printf
-- import Debug.Trace

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

data SparkDuration =
  SparkDuration { startT, endT :: Timestamp,
                  startCount, endCount :: SparkCounters.SparkCounters }
  deriving Show

data SparkTree
  = SparkTree
      {-#UNPACK#-}!Timestamp  -- start time of the span represented by the tree
      {-#UNPACK#-}!Timestamp  -- end time of the span represented by the tree
      SparkNode
  deriving Show

data SparkNode
  = SparkSplit
      {-#UNPACK#-}!Timestamp  -- time used to split the span into two parts
      SparkNode  -- the LHS split; all data lies completely between
                 -- start and split
      SparkNode  -- the RHS split; all data lies completely between
                 -- split and end
      SparkCounters.SparkCounters  -- the delta of spark stats at end and start
  | SparkTreeLeaf
      SparkCounters.SparkCounters  -- spark stats at the start
      SparkCounters.SparkCounters  -- spark stats at the end
  | SparkTreeEmpty   -- after the last GC

  deriving Show


-- Warning: cannot be applied to a suffix of the log (assumes start at time 0).
eventsToSparkDurations :: [GHC.Event] -> (Double, [SparkDuration])
eventsToSparkDurations es =
  let aux _startTime _startCounters [] = (0, [])
      aux startTime startCounters (event : events) =
        case GHC.spec event of
          GHC.SparkCounters crt dud ovf cnv fiz gcd rem ->
            let endTime = GHC.time event
                endCounters =
                  SparkCounters.SparkCounters crt dud ovf cnv fiz gcd rem
                subC = SparkCounters.sub endCounters startCounters
                duration = endTime - startTime
                newMaxSparkValue = maxSparkRenderedValue subC duration
                sd = SparkDuration
                       { startT = startTime,
                         endT = endTime,
                         startCount = startCounters,
                         endCount = endCounters }
                (oldMaxSparkValue, l) = aux endTime endCounters events
            in (max oldMaxSparkValue newMaxSparkValue, sd : l)
          _otherEvent -> aux startTime startCounters events
  in aux 0 SparkCounters.zero es

-- This is the maximal raw value, to be displayed at total zoom in.
-- It's smoothed out (so lower values) at lower zoom levels.
maxSparkRenderedValue :: SparkCounters.SparkCounters -> Timestamp -> Double
maxSparkRenderedValue c duration =
  fromIntegral (max (SparkCounters.sparksDud c +
                     SparkCounters.sparksCreated c +
                     SparkCounters.sparksOverflowed c)
                    (SparkCounters.sparksFizzled c +
                     SparkCounters.sparksConverted c +
                     SparkCounters.sparksGCd c))
  / fromIntegral duration


mkSparkTree :: [SparkDuration] -> Timestamp -> SparkTree
mkSparkTree es endTime =
  SparkTree s e $
  -- trace (show tree) $
  tree
 where
  tree = splitSparks es endTime
  (s,e) = if null es then (0, 0) else (startT (head es), endTime)


splitSparks :: [SparkDuration]  -- events
            -> Timestamp        -- end time of last event in the list
            -> SparkNode
splitSparks [] !_endTime =
  -- if len /= 0 then error "splitSparks0" else
  SparkTreeEmpty

splitSparks [e] !_endTime =
  SparkTreeLeaf (startCount e) (endCount e)

splitSparks es !endTime
  | null rhs
  = splitSparks es lhs_end

  | null lhs
  = error (printf "null lhs: len = %d, startTime = %d, endTime = %d\n" (length es) startTime endTime ++ '\n': show es)

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d\n" (length es) startTime endTime) $
    if length lhs + length rhs /= length es then error (printf "splitSparks3; %d %d %d" (length es) (length lhs) (length rhs)) else
    SparkSplit (startT $ head rhs)
               ltree
               rtree
               (SparkCounters.sub endCounter startCounter)
    where
    startCounter = startCount $ head es
    endCounter = endCount $ last rhs
    startTime = startT $ head es
    splitTime = startTime + (endTime - startTime) `div` 2

    (lhs, lhs_end, rhs) = splitSparkList es [] splitTime 0

    ltree = splitSparks lhs lhs_end
    rtree = splitSparks rhs endTime


splitSparkList :: [SparkDuration]
               -> [SparkDuration]
               -> Timestamp
               -> Timestamp
               -> ([SparkDuration], Timestamp, [SparkDuration])
splitSparkList [] acc !_tsplit !tmax
  = (reverse acc, tmax, [])
splitSparkList (e:es) acc !tsplit !tmax
  | t < tsplit -- pick all events that start before the split
  = splitSparkList es (e:acc) tsplit (max tmax t)
  | otherwise
  = (reverse acc, tmax, e:es)
  where
    t = startT e


-- For each timeslice, gives the number of spark transitions during that period.
-- Approximated from the aggregated data at the level of the spark tree
-- covering intervals of the size similar to the timeslice size.
sparkProfile :: Timestamp -> Timestamp -> Timestamp -> SparkTree
                -> [SparkCounters.SparkCounters]
sparkProfile slice start0 end0 t
  = {- trace (show flat) $ -} chopped

  where
   -- do an extra slice at both ends
   start = if start0 < slice then start0 else start0 - slice
   end   = end0 + slice

   flat = flatten start t []
   chopped0 = chop SparkCounters.zero start flat

   chopped | start0 < slice = SparkCounters.zero : chopped0
           | otherwise      = chopped0

   flatten :: Timestamp -> SparkTree -> [SparkTree] -> [SparkTree]
   flatten _start (SparkTree _s _e SparkTreeEmpty) rest = rest
   flatten start t@(SparkTree s e (SparkSplit split l r _)) rest
     | e   <= start   = rest
     | end <= s       = rest
     | start >= split = flatten start (SparkTree split e r) rest
     | end   <= split = flatten start (SparkTree s split l) rest
     | e - s > slice  = flatten start (SparkTree s split l) $
                        flatten start (SparkTree split e r) rest
     -- A rule of thumb: if a node is narrower than slice, don't drill down,
     -- even if the node sits astride slice boundaries and so the readings
     -- for each of the two neigbouring slices will not be accurate,
     -- but for the pair as a whole, they will be. Smooths the curve down.
     | otherwise      = t : rest
   flatten _start t@(SparkTree _s _e (SparkTreeLeaf _ _)) rest
     = t : rest

   chop :: SparkCounters.SparkCounters -> Timestamp -> [SparkTree]
           -> [SparkCounters.SparkCounters]
   chop sofar start _ts
     | start >= end = if sofar /= SparkCounters.zero then [sofar] else []
   chop sofar start []
     = sofar : chop SparkCounters.zero (start+slice) []
   chop sofar start (t : ts)
     | e <= start
     = if sofar /= SparkCounters.zero
       then error "chop"
       else chop sofar start ts
     | s >= start + slice
     = sofar : chop SparkCounters.zero (start + slice) (t : ts)
     | e > start + slice
     = (SparkCounters.add sofar created_in_this_slice) :
       chop SparkCounters.zero (start + slice) (t : ts)
     | otherwise
     = chop (SparkCounters.add sofar created_in_this_slice) start ts
     where
       (s, e) | SparkTree s e _ <- t  = (s, e)

       duration = min (start + slice) e - max start s
       scale = fromIntegral duration / fromIntegral (e - s)

       created_in_this_slice
         | SparkTree _ _ (SparkTreeLeaf sc ec)     <- t  =
           SparkCounters.rescale scale (SparkCounters.sub ec sc)
         | SparkTree _ _ (SparkTreeEmpty)          <- t  =
           SparkCounters.zero
         | SparkTree _ _ (SparkSplit _ _ _ cDelta) <- t  =
           SparkCounters.rescale scale cDelta


sparkTreeMaxDepth :: SparkTree -> Int
sparkTreeMaxDepth (SparkTree _ _ t) = sparkNodeMaxDepth t

sparkNodeMaxDepth :: SparkNode -> Int
sparkNodeMaxDepth (SparkSplit _ lhs rhs _)
  = 1 + sparkNodeMaxDepth lhs `max` sparkNodeMaxDepth rhs
sparkNodeMaxDepth _ = 1
