module Events.ReadEvents (
    registerEventsFromFile, registerEventsFromTrace
  ) where

import Events.EventTree
import GUI.State
import Events.TestEvents
import Events.EventDuration
import GUI.Timeline
import GUI.EventsWindow
import GUI.Traces
import qualified GUI.ConcurrencyControl as ConcurrencyControl

import Graphics.UI.Gtk hiding (on)

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import System.IO
import Data.Array
import Data.IORef
import Data.List
import Text.Printf
import System.FilePath
import Control.Monad
import Control.Concurrent
import Control.Exception

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

rawEventsToHECs :: [(Maybe Int, [GHCEvents.Event])] -> Timestamp
                -> [(DurationTree,EventTree)]
rawEventsToHECs eventList endTime
  = map (toTree . flip lookup heclists)  [0 .. maximum0 (map fst heclists)]
  where
    heclists = [ (h,events) | (Just h,events) <- eventList ]

    toTree Nothing    = (DurationTreeEmpty, EventTree 0 0 (EventTreeLeaf []))
    toTree (Just evs) =
       ( mkDurationTree (eventsToDurations nondiscrete) endTime,
         mkEventTree discrete endTime )
       where (discrete,nondiscrete) = partition isDiscreteEvent evs

-------------------------------------------------------------------------------

-- XXX: what's this for?
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 [] = -1
maximum0 x = maximum x

-------------------------------------------------------------------------------

registerEventsFromFile :: String -> ViewerState -> TimelineWindow -> EventsWindow -> IO ()
registerEventsFromFile filename = registerEvents (Left filename)

registerEventsFromTrace :: String -> ViewerState -> TimelineWindow -> EventsWindow -> IO ()
registerEventsFromTrace traceName = registerEvents (Right traceName)

registerEvents :: Either FilePath String
                  --TODO: eliminate both of these, return the loaded state
                  -- instead and have the main interaction module update its
                  -- own private state.
               -> ViewerState -> TimelineWindow -> EventsWindow
               -> IO ()

registerEvents from state@ViewerState{..} timelineWin eventsWin = do

  let msg = case from of
              Left filename -> filename
              Right test    -> test

--  dialog <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsCancel msg

  dialog <- dialogNew
  dialogAddButton dialog "gtk-cancel" ResponseCancel
  widgetSetSizeRequest dialog 400 (-1)
  upper <- dialogGetUpper dialog
  hbox <- hBoxNew True 0
  label <- labelNew Nothing
  miscSetAlignment label 0 0.5
  miscSetPadding label 20 0
  labelSetMarkup label $
       printf "<big><b>Loading %s</b></big>"  (takeFileName msg)
  boxPackStart upper label PackGrow 10
  boxPackStart upper hbox PackNatural 10
  progress <- progressBarNew
  boxPackStart hbox progress PackGrow 20
  widgetShowAll upper
  progressBarSetText progress msg
  set dialog [ dialogHasSeparator := False ]
  timeout <- timeoutAdd (do progressBarPulse progress; return True) 50

  windowSetTitle dialog "ThreadScope"

  -- This is a cpu-intensive background task
  ConcurrencyControl.fullSpeed concCtl $ do

  t <- forkIO $ buildEventLog from dialog progress state timelineWin eventsWin
                `onException` dialogResponse dialog (ResponseUser 1)

  r <- dialogRun dialog
  case r of
    ResponseUser 1 -> return ()
    _ -> killThread t
  widgetDestroy dialog
  timeoutRemove timeout

-------------------------------------------------------------------------------

-- Runs in a background thread
--
buildEventLog :: DialogClass dialog => Either FilePath String
              -> dialog
              -> ProgressBar -> ViewerState -> TimelineWindow -> EventsWindow -> IO ()
buildEventLog from dialog progress state@ViewerState{..} timelineWin eventsWin =
  case from of
    Right test     -> build test (testTrace test)
    Left filename  -> do
      progressBarSetText progress $ "Reading " ++ filename
      fmt <- readEventLogFromFile filename
      case fmt of
        Left  err -> hPutStr stderr err
        Right evs -> build filename evs

  where
    build name evs = do
       let
         eventBlockEnd e | EventBlock{ end_time=t } <- spec e = t
         eventBlockEnd e = time e

         lastTx = maximum (0 : map eventBlockEnd (events (dat evs)))

         groups = groupEvents (events (dat evs))
         trees = rawEventsToHECs groups lastTx

         -- sort the events by time and put them in an array
         sorted    = sortGroups groups
         n_events  = length sorted
         event_arr = listArray (0, n_events-1) sorted
         hec_count = length trees

         hecs = HECs {
                  hecCount         = hec_count,
                  hecTrees         = trees,
                  hecEventArray    = event_arr,
                  hecLastEventTime = lastTx
               }

         treeProgress :: ProgressBar -> Int -> (DurationTree,EventTree) -> IO ()
         treeProgress progress hec (tree1,tree2) = do
            progressBarSetText progress $
                     printf "Building HEC %d/%d" (hec+1) hec_count
            progressBarSetFraction progress $
                     fromIntegral hec / fromIntegral hec_count
            evaluate tree1
            evaluate (eventTreeMaxDepth tree2)
            return ()

       zipWithM_ (treeProgress progress) [0..] trees

       do
         windowSetTitle mainWindow ("ThreadScope - " ++ takeFileName name)
         ctx <- statusbarGetContextId statusBar "file"
         statusbarPush statusBar ctx $
            printf "%s (%d events, %.3fs)" name n_events
                                ((fromIntegral lastTx :: Double) * 1.0e-9)
         newHECs state hecs
         timelineParamsChanged state timelineWin
         when debug $ zipWithM_ reportDurationTree [0..] (map fst trees)
         when debug $ zipWithM_ reportEventTree [0..] (map snd trees)

         --FIXME: the following is is a bad pattern. It updates shared IORefs
         -- directly, followed by calling updates. It is too easy to forget
         -- the update (indeed an earlier version of this code updated one
         -- view component but not the other!).
         --
         -- We should eliminate the shared mutable state. Instead, we should
         -- send the new values directly to the view components.
         --
         writeIORef hecsIORef (Just hecs)
         writeIORef scaleIORef defaultScaleValue
         eventsWindowResize eventsWin
         timelineParamsChanged state timelineWin

         dialogResponse dialog (ResponseUser 1)

-------------------------------------------------------------------------------

