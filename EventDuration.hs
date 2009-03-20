-------------------------------------------------------------------------------
--- $Id: EventDuration.hs#1 2009/03/20 17:44:01 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/EventDuration.hs $
-------------------------------------------------------------------------------

-- This module supports a duration-based data-type to represent thread
-- execution and GC information.

module EventDuration
where

import Data.Array

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import EventlogViewerCommon
import StartTimes

-------------------------------------------------------------------------------

data EventDuration
  = ThreadRun ThreadId Timestamp Timestamp
  | GC Timestamp Timestamp
    deriving (Eq, Show)

-------------------------------------------------------------------------------

eventArrayToDuration :: EventArray -> [EventDuration]
eventArrayToDuration = eventArrayToDuration' 0

-------------------------------------------------------------------------------

eventArrayToDuration' :: Int -> EventArray -> [EventDuration]
eventArrayToDuration' idx eventArray 
  = if idx > lastIdx then
      []
    else
      case spec event of
        StopThread{cap=c, thread=t, GHC.RTS.Events.status=s} -> runBar t : rest
        _ -> rest
    where
    event = eventArray!idx
    rest = eventArrayToDuration' (idx+1) eventArray
    (_, lastIdx) = bounds eventArray
    startTime = findRunThreadTime eventArray (idx-1)
    runBar t = ThreadRun t startTime (ts event)

-------------------------------------------------------------------------------
