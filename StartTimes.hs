-------------------------------------------------------------------------------
--- $Id: StartTimes.hs#1 2009/03/20 17:44:01 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/StartTimes.hs $
-------------------------------------------------------------------------------

module StartTimes
where

import Data.Array

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import EventlogViewerCommon

-------------------------------------------------------------------------------

findRunThreadTime :: EventArray -> Int -> Timestamp
findRunThreadTime eventArray idx
  = case spec (eventArray!idx) of
      RunThread cap thr -> ts (eventArray!idx)
      _ -> findRunThreadTime eventArray (idx-1)

-------------------------------------------------------------------------------

findStartGCTime :: EventArray -> Int -> Int -> Timestamp
findStartGCTime eventArray c idx
  = case spec (eventArray!idx) of
      StartGC cap -> if cap == c then
                       ts (eventArray!idx)
                     else
                       findStartGCTime eventArray c (idx-1)
      _ -> findStartGCTime eventArray c (idx-1)

-------------------------------------------------------------------------------
