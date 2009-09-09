-------------------------------------------------------------------------------
--- $Id: StartTimes.hs#3 2009/07/18 22:48:30 REDMOND\\satnams $
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

findRunThreadTime :: Array Int GHCEvents.Event -> Int -> Timestamp
findRunThreadTime eventArray idx
  = case spec (eventArray!idx) of
      RunThread cap thr -> time (eventArray!idx)
      _ -> findRunThreadTime eventArray (idx-1)

-------------------------------------------------------------------------------

findStartGCTime :: Array Int GHCEvents.Event -> Int -> Int -> Timestamp
findStartGCTime eventArray c idx
  = case spec (eventArray!idx) of
      StartGC cap -> if cap == c then
                       time (eventArray!idx)
                     else
                       findStartGCTime eventArray c (idx-1)
      _ -> findStartGCTime eventArray c (idx-1)

-------------------------------------------------------------------------------
