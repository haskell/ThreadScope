{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module ReadEvents ( 
    registerEventsFromFile, registerEventsFromTrace
  ) where

import State
import EventlogViewerCommon
import ReportEventTree
import TestEvents
import EventDuration

import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Data.Array
import qualified Data.Function
import Data.IORef
import Data.List
import Text.Printf
import System.FilePath
import Control.Monad
import Debug.Trace

-------------------------------------------------------------------------------
-- The GHC.RTS.Events library returns the profile information
-- in a data-streucture which contains a list data structure
-- representing the events i.e. [GHCEvents.Event]
-- ThreadScope transforms this list into an alternative representation
-- which (for each HEC) records event *durations* which are ordered in time.
-- The durations represent the run-lengths for thread execution and
-- run-lengths for garbage colleciton. This data-structure is called
-- EventDuration. 
-- ThreadScope then transformations this data-structure into another
-- data-structure which gives a binary-tree view of the event information
-- by performing a binary split on the time domain i.e. the EventTree
-- data structure.

-- GHCEvents.Event => [EventDuration] => EventTree

-------------------------------------------------------------------------------

rawEventsToHECs :: [(Maybe Int, [GHCEvents.Event])] -> HECs
rawEventsToHECs eventList
  = -- trace (show eventList) $
    r
  where r= [ (hec, eventsToTree events) | (Just hec, events) <- eventList ]
    
-------------------------------------------------------------------------------

eventsToTree :: [GHCEvents.Event] -> EventTree
eventsToTree events
  = trace ("events: " ++ show (length events) ++ "\ndurations: " ++ show (length durations)) $ tree
    where
    tree = splitEvents durations
    durations = eventsToDurations events

-------------------------------------------------------------------------------

registerEventsFromFile :: String
		       -> ViewerState
		       -> ContextId
		       -> IO ()

registerEventsFromFile filename state summary_ctx
  = do eitherFmt <- readEventLogFromFile filename 
       registerEvents filename eitherFmt state summary_ctx
       
-------------------------------------------------------------------------------

registerEventsFromTrace :: String
			-> ViewerState
			-> ContextId
			-> IO ()

registerEventsFromTrace traceName
  = registerEvents traceName (Right (testTrace traceName)) 
       
-------------------------------------------------------------------------------

registerEvents :: String
	       -> Either String EventLog
	       -> ViewerState
	       -> ContextId
	       -> IO ()

registerEvents name eitherFmt ViewerState{..} summary_ctx
  =   case eitherFmt of
        Right fmt -> 
         do let pes = events (dat fmt)
                groups = groupEvents (events (dat fmt))
                hecs = rawEventsToHECs groups
                lastTx = maximum (map (time.last.snd) groups) -- Last event time i
                capabilities = ennumerateCapabilities pes
            -- Debugging information
            when debug $ reportEventTrees hecs
            -- Update the IORefs used for drawing callbacks
            writeIORef capabilitiesIORef (Just capabilities)
            writeIORef hecsIORef (Just hecs)
            writeIORef lastTxIORef lastTx
            writeIORef scaleIORef defaultScaleValue
            let duration = lastTx 

            -- sort the events by time and put them in an array
            let sorted    = sortGroups groups
		n_events  = length sorted
		event_arr = listArray (0, n_events-1) sorted
            writeIORef eventArrayIORef event_arr

            -- Adjust height to fit capabilities
            (width, _) <- widgetGetSize mainWindow
            widgetSetSizeRequest mainWindow width ((length capabilities)*gapcap+oycap+120)

            -- Set the status bar
            statusbarPush summaryBar summary_ctx (show n_events ++ " events. Duration " ++ (printf "%.3f" (((fromIntegral duration)::Double) * 1.0e-9)) ++ " seconds.")   
            windowSetTitle mainWindow ("ThreadScope - " ++ takeFileName name)

        Left msg -> putStrLn msg
       
-------------------------------------------------------------------------------
