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
  | idx < 0 = error "findRunThreadTime"
  | otherwise
  = case spec (eventArray!idx) of
      RunThread thr -> time (eventArray!idx)
      _             -> findRunThreadTime eventArray (idx-1)

-------------------------------------------------------------------------------

findStartGCTime :: Array Int GHCEvents.Event -> Int -> Timestamp
findStartGCTime eventArray idx
  | idx < 0 = error "findStartGCTime"
  | otherwise
  = case spec (eventArray!idx) of
      StartGC -> time (eventArray!idx)
      _       -> findStartGCTime eventArray (idx-1)

-------------------------------------------------------------------------------
