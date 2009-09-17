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
  = trace ("durations: " ++ show (length durations)) $ splitEvents durations
    where
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

registerEventsFromFile :: Bool ->
                          String -> IORef (Maybe [Int]) -> MaybeHECsIORef ->
                          IORef Double -> IORef Timestamp ->
                          Window -> DrawingArea -> HScrollbar ->
                          Label -> Statusbar -> ContextId -> IO ()
registerEventsFromFile debug filename capabilitiesIORef eventArrayIORef scale
                       lastTxIORef window profileDrawingArea profileHScrollbar
                       profileNameLabel summarybar
                       summary_ctx
  = do eitherFmt <- readEventLogFromFile filename 
       registerEvents debug filename eitherFmt 
                   capabilitiesIORef eventArrayIORef scale
                   lastTxIORef window profileDrawingArea profileHScrollbar
                   profileNameLabel summarybar
                   summary_ctx
       
-------------------------------------------------------------------------------


registerEventsFromTrace :: Bool ->
                          String -> IORef (Maybe [Int]) -> MaybeHECsIORef ->
                          IORef Double -> IORef Timestamp ->
                          Window -> DrawingArea -> HScrollbar ->
                          Label -> Statusbar -> 
                          ContextId -> IO ()
registerEventsFromTrace debug traceName capabilitiesIORef eventArrayIORef scale
                       lastTxIORef window profileDrawingArea profileHScrollbar
                       profileNameLabel summarybar
                       summary_ctx
  = registerEvents debug traceName (Right (testTrace traceName)) 
                   capabilitiesIORef eventArrayIORef scale
                   lastTxIORef window profileDrawingArea profileHScrollbar
                   profileNameLabel summarybar
                   summary_ctx
       
-------------------------------------------------------------------------------

registerEvents :: Bool -> String -> Either String EventLog ->
                  IORef (Maybe [Int]) -> MaybeHECsIORef ->
                  IORef Double -> IORef Timestamp ->
                  Window -> DrawingArea -> HScrollbar ->
                  Label -> Statusbar -> ContextId -> IO ()
registerEvents debug name eitherFmt capabilitiesIORef eventArrayIORef scale
                       lastTxIORef window profileDrawingArea profileHScrollbar
                       profileNameLabel summarybar
                       summary_ctx
  =   case eitherFmt of
        Right fmt -> 
         do let pes = events (dat fmt)
                sorted = groupEvents (events (dat fmt))
                hecs = rawEventsToHECs sorted
                lastTx = maximum (map (time.last.snd) sorted) -- Last event time i
                capabilities = ennumerateCapabilities pes
            -- Debugging information
            when debug $ reportEventTrees hecs
            -- Update the IORefs used for drawing callbacks
            writeIORef capabilitiesIORef (Just capabilities)
            writeIORef eventArrayIORef (Just hecs)
            writeIORef lastTxIORef lastTx
            writeIORef scale defaultScaleValue
            let duration = lastTx 
                nrEvents = length sorted

            -- Adjust height to fit capabilities
            (width, _) <- widgetGetSize window
            widgetSetSizeRequest window width ((length capabilities)*gapcap+oycap+120)

            -- Set the status bar
            statusbarPush summarybar summary_ctx (show nrEvents ++ " events. Duration " ++ (printf "%.3f" (((fromIntegral duration)::Double) * 1.0e-9)) ++ " seconds.")   

            --- Set the label for the name of the event log
            profileNameLabel `labelSetText` name

        Left msg -> putStrLn msg
       
-------------------------------------------------------------------------------
