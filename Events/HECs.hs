module Events.HECs (
    HECs(..),
    Event,
    CapEvent,
    Timestamp,
  ) where

import Events.EventTree
import GHC.RTS.Events

import Data.Array

-----------------------------------------------------------------------------

-- all the data from a .eventlog file
data HECs = HECs {
       hecCount         :: Int,
       hecTrees         :: [(DurationTree,EventTree)],
       hecEventArray    :: Array Int CapEvent,
       hecLastEventTime :: Timestamp
     }

-----------------------------------------------------------------------------
