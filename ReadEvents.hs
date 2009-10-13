module ReadEvents ( 
    registerEventsFromFile, registerEventsFromTrace
  ) where

import EventTree
import State
import TestEvents
import EventDuration
import Timeline

import Graphics.UI.Gtk hiding (on)

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
import Data.Function

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

rawEventsToHECs :: [(Maybe Int, [GHCEvents.Event])] -> [EventTree]
rawEventsToHECs eventList
  = map (toTree . flip lookup heclists)  [0 .. maximum (map fst heclists)]
  where
    heclists = [ (h,events) | (Just h,events) <- eventList ]

    toTree Nothing    = EventTreeLeaf []
    toTree (Just evs) = eventsToTree evs
    
-------------------------------------------------------------------------------

eventsToTree :: [GHCEvents.Event] -> EventTree
eventsToTree events
  = trace ("events: " ++ show (length events) ++ "\ndurations: " ++ show (length durations)) $ tree
    where
    tree = mkEventTree durations
    durations = eventsToDurations events

-------------------------------------------------------------------------------

registerEventsFromFile :: String
		       -> ViewerState
		       -> IO ()

registerEventsFromFile filename state
  = do eitherFmt <- readEventLogFromFile filename 
       registerEvents filename eitherFmt state
       
-------------------------------------------------------------------------------

registerEventsFromTrace :: String
			-> ViewerState
			-> IO ()

registerEventsFromTrace traceName
  = registerEvents traceName (Right (testTrace traceName)) 
       
-------------------------------------------------------------------------------

registerEvents :: String
	       -> Either String EventLog
	       -> ViewerState
	       -> IO ()

registerEvents _name (Left msg) _ = 
  putStrLn msg
registerEvents name (Right fmt) state@ViewerState{..} = do
  let 
      groups = groupEvents (events (dat fmt))
      trees = rawEventsToHECs groups
      lastTx = maximum (map (time.last.snd) groups) -- Last event time i

      -- sort the events by time and put them in an array
      sorted    = sortGroups groups
      n_events  = length sorted
      event_arr = listArray (0, n_events-1) sorted

      hecs = HECs {
               hecCount         = length trees,
               hecTrees         = trees,
               hecEventArray    = event_arr,
               hecLastEventTime = lastTx
            }
  --
  writeIORef hecsIORef (Just hecs)

  -- Debugging information
  when debug $ zipWithM_ reportEventTree [0..] trees

  -- Update the IORefs used for drawing callbacks
  writeIORef scaleIORef defaultScaleValue

  -- Adjust height to fit capabilities
--  (width, _) <- widgetGetSize mainWindow
--  widgetSetSizeRequest mainWindow width ((length capabilities)*gapcap+oycap+120)

  -- Set the status bar
  ctx <- statusbarGetContextId statusBar "file"
  statusbarPush statusBar ctx $
    printf "%s (%d events, %.3fs)" name n_events
                              ((fromIntegral lastTx :: Double) * 1.0e-9)

  windowSetTitle mainWindow ("ThreadScope - " ++ takeFileName name)

  updateTimelines state [ TraceHEC n | n <- [0..hecCount hecs-1] ]
       
-------------------------------------------------------------------------------
