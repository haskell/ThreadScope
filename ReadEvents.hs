module ReadEvents
where

import Data.Array
import qualified Data.Function
import Data.IORef
import Data.List
import EventDuration
import Text.Printf

import EventlogViewerCommon
import ReportEventTree
import TestEvents

import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-- Imports from Haskell library
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
  = trace (show (map fst r)) $ r
  where r= [ (hec, eventsToTree events) | (Just hec, events) <- eventList ]
    
-------------------------------------------------------------------------------

eventsToTree :: [GHCEvents.Event] -> EventTree
eventsToTree events
  = trace ("durations: " ++ show (length durations) ++ "\n" ++ "countNodes: " ++ show (countNodes tree)) $ tree
    where
    tree = splitEvents durations
    durations = eventArrayToDuration eventsAsArray
    eventsAsArray = listArray (0, nrEvents-1) events
    nrEvents = length events

-------------------------------------------------------------------------------

eventFromHEC :: Int -> GHCEvents.Event -> Bool
eventFromHEC hec event 
  = case spec event of
      UnknownEvent -> False
      Message{}    -> False
      _            -> cap (spec event) == hec

-------------------------------------------------------------------------------

data ViewerState = ViewerState {
  capabilitiesIORef :: IORef (Maybe [Int]),
  hecsIORef         :: MaybeHECsIORef,
  scaleIORef        :: IORef Double,
  lastTxIORef       :: IORef Timestamp,
  eventArrayIORef   :: IORef (Array Int GHCEvents.CapEvent)
  }

registerEventsFromFile :: Bool
		       -> String
		       -> ViewerState
		       -> Window
		       -> Label
		       -> Statusbar
		       -> ContextId
		       -> IO ()

registerEventsFromFile debug filename state window 
                       profileNameLabel summarybar
                       summary_ctx
  = do eitherFmt <- readEventLogFromFile filename 
       registerEvents debug filename eitherFmt 
                   state window 
                   profileNameLabel summarybar
                   summary_ctx
       
-------------------------------------------------------------------------------

registerEventsFromTrace :: Bool
			-> String
			-> ViewerState
			-> Window
			-> Label
			-> Statusbar
			-> ContextId
			-> IO ()

registerEventsFromTrace debug traceName
  = registerEvents debug traceName (Right (testTrace traceName)) 
       
-------------------------------------------------------------------------------

registerEvents :: Bool
	       -> String
	       -> Either String EventLog
	       -> ViewerState
	       -> Window
	       -> Label
	       -> Statusbar
	       -> ContextId
	       -> IO ()

registerEvents debug name eitherFmt state window
                       profileNameLabel summarybar
                       summary_ctx
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
            writeIORef (capabilitiesIORef state) (Just capabilities)
            writeIORef (hecsIORef state) (Just hecs)
            writeIORef (lastTxIORef state) lastTx
            writeIORef (scaleIORef state) defaultScaleValue
            let duration = lastTx 

            -- sort the events by time and put them in an array
            let sorted    = sortGroups groups
		n_events  = length sorted
		event_arr = listArray (0, n_events-1) sorted
            writeIORef (eventArrayIORef state) event_arr

            -- Adjust height to fit capabilities
            (width, _) <- widgetGetSize window
            widgetSetSizeRequest window width ((length capabilities)*gapcap+oycap+120)

            -- Set the status bar
            statusbarPush summarybar summary_ctx (show n_events ++ " events. Duration " ++ (printf "%.3f" (((fromIntegral duration)::Double) * 1.0e-9)) ++ " seconds.")   

            --- Set the label for the name of the event log
            profileNameLabel `labelSetText` name

        Left msg -> putStrLn msg
       
-------------------------------------------------------------------------------
