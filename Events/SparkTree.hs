module Events.SparkTree (
     SparkTree(..),
     mkSparkTree,
  ) where

import Data.Word (Word64)

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event)

import Text.Printf

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
                  startCount, endCount :: SparkCounters }
  deriving Show

data SparkCounters =
  SparkCount { sparksCreated, sparksDud, sparksOverflowed,
               sparksConverted, sparksFizzled, sparksGCd,
               sparksRemaining :: {-# UNPACK #-}! Word64 }
  deriving Show

data SparkTree
  = SparkTree
      {-#UNPACK#-}!Timestamp  -- start time of this span
      {-#UNPACK#-}!Timestamp  -- end time of this span
      SparkNode
  deriving Show

data SparkNode
  = SparkSplit
      {-#UNPACK#-}!Timestamp  -- time used to split the span into two parts
      SparkNode  -- the LHS split; all data lies completely between
                 -- start and split
      SparkNode  -- the RHS split; all data lies completely between
                 -- split and end
      SparkCounters  -- the delta of spark stats at the end and at the start
  | SparkTreeLeaf
      SparkCounters  -- spark stats at the start
      SparkCounters  -- spark stats at the end
  | SparkTreeEmpty   -- before the first GC and after the last GC

  deriving Show


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
  = error (printf "null lhs: len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" (length es) startTime endTime ++ '\n': show es)

  | otherwise
  = -- trace (printf "len = %d, startTime = %d, endTime = %d, lhs_len = %d\n" len startTime endTime lhs_len) $
    -- if len /= length es || length lhs + length rhs /= len then error (printf "splitSparks3; %d %d %d %d %d" len (length es) (length lhs) lhs_len (length rhs))  else
    SparkSplit (startT $ head rhs)
               ltree
               rtree
               (subtractSparkCounters endCounter startCounter)
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


subtractSparkCounters :: SparkCounters -> SparkCounters -> SparkCounters
subtractSparkCounters
  (SparkCount sparksCreated1 sparksDud1 sparksOverflowed1
              sparksConverted1 sparksFizzled1 sparksGCd1
              sparksRemaining1)
  (SparkCount sparksCreated2 sparksDud2 sparksOverflowed2
              sparksConverted2 sparksFizzled2 sparksGCd2
              sparksRemaining2)
  = SparkCount
      (sparksCreated2 - sparksCreated1)
      (sparksDud2 - sparksDud1)
      (sparksOverflowed2 - sparksOverflowed1)
      (sparksConverted2 -  sparksConverted1)
      (sparksFizzled2 - sparksFizzled1)
      (sparksGCd2 - sparksGCd1)
      (sparksRemaining2 - sparksRemaining1)
