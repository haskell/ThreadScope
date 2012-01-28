module Events.SparkTree (
  SparkTree,
  sparkTreeMaxDepth,
  emptySparkTree,
  eventsToSparkDurations,
  mkSparkTree,
  sparkProfile,
  ) where

import qualified Events.SparkStats as SparkStats

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events (Timestamp)

import Control.Exception (assert)
import Text.Printf
-- import Debug.Trace

-- | Sparks change state. Each state transition process has a duration.
-- SparkDuration is a condensed description of such a process,
-- containing a start time of the duration interval,
-- spark stats that record the spark transition rate
-- and the absolute number of sparks in the spark pool within the duration.
data SparkDuration =
  SparkDuration { startT :: {-#UNPACK#-}!Timestamp,
                  deltaC :: {-#UNPACK#-}!SparkStats.SparkStats }
  deriving Show

-- | Calculates durations and maximal rendered values from the event log.
-- Warning: cannot be applied to a suffix of the log (assumes start at time 0).
eventsToSparkDurations :: [GHCEvents.Event] -> (Double, [SparkDuration])
eventsToSparkDurations es =
  let aux _startTime _startCounters [] = (0, [])
      aux startTime startCounters (event : events) =
        case GHCEvents.spec event of
          GHCEvents.SparkCounters crt dud ovf cnv fiz gcd rem ->
            let endTime = GHCEvents.time event
                endCounters = (crt, dud, ovf, cnv, fiz, gcd, rem)
                delta = SparkStats.create startCounters endCounters
                newMaxSparkPool = SparkStats.maxPool delta
                sd = SparkDuration { startT = startTime,
                                     deltaC = delta }
                (oldMaxSparkPool, l) = aux endTime endCounters events
            in (max oldMaxSparkPool newMaxSparkPool, sd : l)
          _otherEvent -> aux startTime startCounters events
  in aux 0 (0,0,0,0,0,0,0) es


-- | We map the spark transition durations (intervals) onto a binary
-- search tree, so that we can easily find the durations
-- that correspond to a particular view of the timeline.
-- Additionally, each node of the tree contains a summary
-- of the information below it, so that we can render views at various
-- levels of resolution. For example, if a tree node would represent
-- less than one pixel on the display, there is no point is descending
-- the tree further.
data SparkTree
  = SparkTree
      {-#UNPACK#-}!Timestamp  -- ^ start time of span represented by the tree
      {-#UNPACK#-}!Timestamp  -- ^ end time of the span represented by the tree
      SparkNode
  deriving Show

data SparkNode
  = SparkSplit
      {-#UNPACK#-}!Timestamp  -- ^ time used to split the span into two parts
      SparkNode
        -- ^ the LHS split; all data lies completely between start and split
      SparkNode
        -- ^ the RHS split; all data lies completely between split and end
      {-#UNPACK#-}!SparkStats.SparkStats
        -- ^ aggregate of the spark stats within the span
  | SparkTreeLeaf
      {-#UNPACK#-}!SparkStats.SparkStats
        -- ^ the spark stats for the base duration
  | SparkTreeEmpty
      -- ^ represents a span that no data referts to, e.g., after the last GC
  deriving Show

sparkTreeMaxDepth :: SparkTree -> Int
sparkTreeMaxDepth (SparkTree _ _ t) = sparkNodeMaxDepth t

sparkNodeMaxDepth :: SparkNode -> Int
sparkNodeMaxDepth (SparkSplit _ lhs rhs _)
  = 1 + sparkNodeMaxDepth lhs `max` sparkNodeMaxDepth rhs
sparkNodeMaxDepth _ = 1

emptySparkTree :: SparkTree
emptySparkTree = SparkTree 0 0 SparkTreeEmpty

-- | Create spark tree from spark durations.
-- Note that the last event may be not a spark event, in which case
-- there is no data about sparks for the last time interval
-- (the subtree for the interval will have SparkTreeEmpty node).
mkSparkTree :: [SparkDuration]  -- ^ spark durations calculated from events
            -> Timestamp        -- ^ end time of last event in the list
            -> SparkTree
mkSparkTree es endTime =
  SparkTree s e $
  -- trace (show tree) $
  tree
    where
      tree = splitSparks es endTime
      (s, e) = if null es then (0, 0) else (startT (head es), endTime)

-- | Construct spark tree, by recursively splitting time intervals..
-- We only split at spark transition duration boundaries;
-- we never split a duration into multiple pieces.
-- Therefore, the binary tree is only roughly split by time,
-- the actual split depends on the distribution of sample points below it.
splitSparks :: [SparkDuration] -> Timestamp -> SparkNode
splitSparks [] !_endTime =
  SparkTreeEmpty

splitSparks [e] !_endTime =
  SparkTreeLeaf (deltaC e)

splitSparks es !endTime
  | null rhs
  = splitSparks es lhs_end
  | null lhs
  = error $
    printf "splitSparks: null lhs: len = %d, startTime = %d, endTime = %d\n"
      (length es) startTime endTime
    ++ '\n' : show es
  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d\n" (length es) startTime endTime) $
    assert (length lhs + length rhs == length es) $
    SparkSplit (startT $ head rhs)
               ltree
               rtree
               (SparkStats.aggregate (subDelta rtree ++ subDelta ltree))
  where
    -- | Integer division, rounding up.
    divUp :: Timestamp -> Timestamp -> Timestamp
    divUp n k = (n + k - 1) `div` k
    startTime = startT $ head es
    splitTime = startTime + (endTime - startTime) `divUp` 2

    (lhs, lhs_end, rhs) = splitSparkList es [] splitTime 0

    ltree = splitSparks lhs lhs_end
    rtree = splitSparks rhs endTime

    subDelta (SparkSplit _ _ _ delta) = [delta]
    subDelta (SparkTreeLeaf delta)    = [delta]
    subDelta SparkTreeEmpty           = []


splitSparkList :: [SparkDuration]
               -> [SparkDuration]
               -> Timestamp
               -> Timestamp
               -> ([SparkDuration], Timestamp, [SparkDuration])
splitSparkList [] acc !_tsplit !tmax
  = (reverse acc, tmax, [])
splitSparkList [e] acc !_tsplit !tmax
  -- Just one event left: put it on the right. This ensures that we
  -- have at least one event on each side of the split.
  = (reverse acc, tmax, [e])
splitSparkList (e:es) acc !tsplit !tmax
  | startT e <= tsplit  -- pick all durations that start at or before the split
  = splitSparkList es (e:acc) tsplit (max tmax (startT e))
  | otherwise
  = (reverse acc, tmax, e:es)


-- | For each timeslice, give the spark stats calculated for that interval.
-- The spark stats are Approximated from the aggregated data
-- at the level of the spark tree covering intervals of the size
-- similar to the timeslice size.
sparkProfile :: Timestamp -> Timestamp -> Timestamp -> SparkTree
             -> [SparkStats.SparkStats]
sparkProfile slice start0 end0 t
  = {- trace (show flat) $ -} chopped

  where
   -- do an extra slice at both ends
   start = if start0 < slice then start0 else start0 - slice
   end   = end0 + slice

   flat = flatten start t []
   -- TODO: redefine chop so that it's obvious this error will not happen
   -- e.g., catch pathological cases, like a tree with only SparkTreeEmpty
   -- inside and/or make it tail-recursive instead of
   -- taking the 'previous' argument
   chopped0 = chop (error "Fatal error in sparkProfile.") [] start flat

   chopped | start0 < slice = SparkStats.initial : chopped0
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
     -- for each of the two neigbouring slices will not be accurate
     -- (but for the pair as a whole, they will be). Smooths the curve down
     -- even more than averaging over the timeslice already does.
     | otherwise      = t : rest
   flatten _start t@(SparkTree _s _e (SparkTreeLeaf _)) rest
     = t : rest

   chop :: SparkStats.SparkStats -> [SparkStats.SparkStats]
           -> Timestamp -> [SparkTree] -> [SparkStats.SparkStats]
   chop _previous sofar start1 _ts
     | start1 >= end
     = case sofar of
       _ : _ -> [SparkStats.aggregate sofar]
       [] -> []
   chop _previous sofar _start1 []  -- data too short for the redrawn area
     | null sofar  -- no data at all in the redrawn area
     = []
     | otherwise
     = [SparkStats.aggregate sofar]
   chop previous sofar start1 (t : ts)
     | e <= start1  -- skipping data left of the slice
     = case sofar of
       _ : _ -> error "chop"
       [] -> chop previous sofar start1 ts
     | s >= start1 + slice  -- postponing data right of the slice
     = let (c, p) = SparkStats.agEx sofar previous
       in c : chop p [] (start1 + slice) (t : ts)
     | e > start1 + slice
     = let (c, p) = SparkStats.agEx (created_in_this_slice t ++ sofar) previous
       in c : chop p [] (start1 + slice) (t : ts)
     | otherwise
     = chop previous (created_in_this_slice t ++ sofar) start1 ts
     where
       (s, e) | SparkTree s e _ <- t  = (s, e)

       -- The common part of the slice and the duration.
       mi = min (start1 + slice) e
       ma = max start1 s
       common = if mi < ma then 0 else mi - ma
       -- Instead of drilling down the tree (unless it's a leaf),
       -- we approximate by taking a proportion of the aggregate value,
       -- depending on how much of the spark duration corresponding
       -- to the tree node is covered by our timeslice.
       proportion = if e > s
                    then fromIntegral common / fromIntegral (e - s)
                    else assert (e == s && common == 0) $ 0

       -- Spark transitions in the tree are in units spark/duration.
       -- Here the numbers are rescaled so that the units are spark/ms.
       created_in_this_slice (SparkTree _ _ node) = case node of
         SparkTreeLeaf delta    -> [SparkStats.rescale proportion delta]
         SparkTreeEmpty         -> []
         SparkSplit _ _ _ delta -> [SparkStats.rescale proportion delta]
