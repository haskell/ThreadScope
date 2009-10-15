
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports from Haskell library
import System.Environment
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Function
import Data.List

import Paths_threadscope

-- Imports for ThreadScope
import State
import About
import FileDialog
import Options
import ReadEvents
import EventsWindow
import Timeline

-------------------------------------------------------------------------------

main :: IO ()
main 
  = do -- Deal with command line argument processing.
       -- This application accepts one optional argument specifying 
       -- the event log.
       args <- getArgs
       let options = parseOptions args

       unsafeInitGUIForThreadedRTS
       state <- buildInitialState options

       startup options state

startup :: [Option] -> ViewerState -> IO ()
startup options state@ViewerState{..} 
  = do
       let
           filenames = [filename | Filename filename <- options]
           tracenames = [name | TestTrace name <- options]
       when (length filenames > 1)
         (putStrLn "usage: threadscope [eventlog_filename]")
       let filename = if filenames == [] then
                       ""
                      else
                        head filenames
           traceName = if tracenames == [] then
                         ""
                       else
                         head tracenames

       widgetSetAppPaintable mainWindow True
       logoPath <- getDataFileName "threadscope.png"
       windowSetIconFromFile mainWindow logoPath

       ------------------------------------------------------------------------
       -- Status bar functionality
       ctx <- statusbarGetContextId statusBar "state"
       statusbarPush statusBar ctx "No eventlog loaded."

       ------------------------------------------------------------------------
       --- Get the label for the name of the event log

       -- B&W toggle button
       bwToggle `onToggle` timelineParamsChanged state

       -- No Labels toggle button
       showLabelsToggle `onToolButtonToggled` timelineParamsChanged state

       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for 
       -- the capabilities and event array.
       when (filename /= "") $ registerEventsFromFile filename state

       -- Likewise for test traces
       when (traceName /= "") $ registerEventsFromTrace traceName state

       -- B&W toggle button
                      
       -- The File:Open menu option can be used to specify an
       -- eventlog file.
       openMenuItem `onActivateLeaf` do
         filename <- openFileDialog mainWindow
         when (isJust filename) $
           registerEventsFromFile (fromJust filename) state
                                     
       ------------------------------------------------------------------------
       -- Save as PDF functionality
       saveMenuItem `onActivateLeaf` do
         (width, height) <- widgetGetSize timelineDrawingArea
         scaleValue <- readIORef scaleIORef
         maybeEventArray <- readIORef hecsIORef
         hadj_value <- adjustmentGetValue timelineAdj
         hadj_pagesize <- adjustmentGetPageSize timelineAdj
         mfn <- readIORef filenameIORef
         case (mfn, maybeEventArray) of
           (Just fn, Just hecs) -> do
             bw_mode <- checkMenuItemGetActive bwToggle
             labels_mode <- toggleToolButtonGetActive showLabelsToggle

             let params = ViewParameters {
                                width     = width,
                                height    = height,
                                hadjValue = hadj_value,
                                scaleValue = scaleValue,
                                detail = 2, -- for now
                                bwMode = bw_mode,
                                labelsMode = labels_mode
                            }

                 rect = Rectangle 0 0 width height

             traces <- readIORef timelineTraces
             withPDFSurface (fn++".pdf") (fromIntegral width) (fromIntegral height)
               (flip renderWith $ (translate (-hadj_value) 0 >> 
                                   renderTraces state params traces hecs rect) >> showPage)
             withImageSurface C.FormatARGB32 (fromIntegral width) (fromIntegral height) $ \ result ->
             
               do  renderWith result (translate (-hadj_value) 0 >> 
                                      renderTraces state params traces hecs rect)
                   surfaceWriteToPNG result (fn++".png")
             statusbarPush statusBar ctx ("Saved " ++ fn ++ ".pdf")
             return ()

           _other -> return ()

       ------------------------------------------------------------------------
       -- Reload functionality
       onActivateLeaf reloadMenuItem $
          do mb_filename <- readIORef filenameIORef
             case mb_filename of
               Nothing -> return ()
               Just filename -> registerEventsFromFile filename state

       ------------------------------------------------------------------------
       -- CPUs view

       setupTimelineView state

       ------------------------------------------------------------------------
       -- Event view

       setupEventsWindow state

       ------------------------------------------------------------------------
       -- Quit
       quitMenuItem `onActivateLeaf` mainQuit

       ------------------------------------------------------------------------
       -- About dialog
       aboutMenuItem `onActivateLeaf` showAboutDialog mainWindow

       ------------------------------------------------------------------------
       -- Quit behaviour
       onDestroy mainWindow mainQuit

       ------------------------------------------------------------------------
       -- Show all windows
       widgetShowAll mainWindow

       ------------------------------------------------------------------------
       -- Enter main event loop for GUI.
       mainGUI

-------------------------------------------------------------------------------

buildInitialState :: [Option] -> IO ViewerState
buildInitialState options = do

       gladePath <- getDataFileName "threadscope.glade"
       Just xml <- xmlNew gladePath
       
       let debug = Debug `elem` options


       filenameIORef <- newIORef Nothing

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       capabilitiesIORef <- newIORef Nothing
       hecsIORef         <- newIORef Nothing
       lastTxIORef       <- newIORef 0
       eventArrayIORef   <- newIORef (error "eventArrayIORef")
       scaleIORef        <- newIORef defaultScaleValue
       cursorIORef       <- newIORef 0

       mainWindow         <- xmlGetWidget xml castToWindow "main_window"
       statusBar          <- xmlGetWidget xml castToStatusbar "statusbar"
       progressBar        <- progressBarNew
       containerAdd statusBar progressBar

       bwToggle           <- xmlGetWidget xml castToCheckMenuItem "black_and_white"
       sidebarToggle      <- xmlGetWidget xml castToCheckMenuItem "view_sidebar"
       openMenuItem       <- xmlGetWidget xml castToMenuItem "openMenuItem"
       saveMenuItem       <- xmlGetWidget xml castToMenuItem "saveMenuItem"
       saveAsMenuItem     <- xmlGetWidget xml castToMenuItem "saveAsMenuItem"
       reloadMenuItem     <- xmlGetWidget xml castToMenuItem "view_reload"
       quitMenuItem       <- xmlGetWidget xml castToMenuItem "quitMenuItem"
       aboutMenuItem      <- xmlGetWidget xml castToMenuItem "aboutMenuItem"

       timelineDrawingArea      <- xmlGetWidget xml castToDrawingArea
                                        "timeline_drawingarea"
       timelineLabelDrawingArea <- xmlGetWidget xml castToDrawingArea
                                        "timeline_labels_drawingarea"
       timelineKeyDrawingArea   <- xmlGetWidget xml castToDrawingArea
                                        "timeline_key_drawingarea"
       timelineHScrollbar       <- xmlGetWidget xml castToHScrollbar
                                        "timeline_hscroll"
       timelineVScrollbar       <- xmlGetWidget xml castToVScrollbar
                                        "timeline_vscroll"
       timelineAdj         <- rangeGetAdjustment timelineHScrollbar
       timelineVAdj        <- rangeGetAdjustment timelineVScrollbar

       timelineTraces     <- newIORef []
       timelinePrevView   <- newIORef Nothing

       zoomInButton       <- xmlGetWidget xml castToToolButton "cpus_zoomin"
       zoomOutButton      <- xmlGetWidget xml castToToolButton "cpus_zoomout"
       zoomFitButton      <- xmlGetWidget xml castToToolButton "cpus_zoomfit"

       showLabelsToggle   <- xmlGetWidget xml castToToggleToolButton "cpus_showlabels"
       firstButton        <- xmlGetWidget xml castToToolButton "cpus_first"
       lastButton         <- xmlGetWidget xml castToToolButton "cpus_last"
       centreButton       <- xmlGetWidget xml castToToolButton "cpus_centre"

       eventsFontExtents  <- newIORef (error "eventsFontExtents")
       eventsCursorIORef  <- newIORef Nothing
       eventsVScrollbar   <- xmlGetWidget xml castToVScrollbar "eventsVScroll"
       eventsAdj          <- rangeGetAdjustment eventsVScrollbar
       eventsDrawingArea  <- xmlGetWidget xml castToDrawingArea "eventsDrawingArea"
       eventsTextEntry    <- xmlGetWidget xml castToEntry "events_entry"
       eventsFindButton   <- xmlGetWidget xml castToToolButton "events_find"
       eventsFirstButton  <- xmlGetWidget xml castToToolButton "events_first"
       eventsHomeButton   <- xmlGetWidget xml castToToolButton "events_home"
       eventsLastButton   <- xmlGetWidget xml castToToolButton "events_last"

       sidebarVBox        <- xmlGetWidget xml castToVBox "sidebar_vbox"
       sidebarHBox        <- xmlGetWidget xml castToHBox "sidebar_hbox"
       sidebarCombo       <- xmlGetWidget xml castToComboBox "sidebar_combobox"
       sidebarCloseButton <- xmlGetWidget xml castToButton "sidebar_close_button"

       return ViewerState { .. }
