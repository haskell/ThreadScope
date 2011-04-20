{-# LANGUAGE CPP #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

-- Imports for GTK
import Graphics.UI.Gtk
import System.Glib.GError (failOnGError)
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk.ModelView as New

-- Imports from Haskell library
import System.Environment
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Function
import Data.List
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Concurrent
import Control.Exception

import Paths_threadscope

-- Imports for ThreadScope
import State
import About
import FileDialog
import Options
import ReadEvents
import EventsWindow
import Timeline
import SaveAsPDF
import SaveAsPNG
import Sidebar
import Traces

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

       writeIORef filenameIORef (if filename == "" then
                                   Nothing
                                 else
                                   Just filename)
        
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
       saveAsPDFMenuItem `onActivateLeaf` saveAsPDF state 

       ------------------------------------------------------------------------
       -- Save as PNG functionality
       saveAsPNGMenuItem `onActivateLeaf` saveAsPNG state 

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
       -- Sidebar

       setupSideBar state

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

#ifndef mingw32_HOST_OS
       main <- myThreadId
       installHandler sigINT (Catch (postGUIAsync (throw UserInterrupt))) Nothing
#endif

       ------------------------------------------------------------------------
       -- Enter main event loop for GUI.
       mainGUI

-------------------------------------------------------------------------------

buildInitialState :: [Option] -> IO ViewerState
buildInitialState options = failOnGError $ do

       builder <- builderNew
       builderAddFromFile builder =<< getDataFileName "threadscope.ui"
       let getWidget cast name = builderGetObject builder cast name 

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

       mainWindow         <- getWidget castToWindow "main_window"
       statusBar          <- getWidget castToStatusbar "statusbar"
       hpaned             <- getWidget castToHPaned "hpaned"

       bwToggle           <- getWidget castToCheckMenuItem "black_and_white"
       sidebarToggle      <- getWidget castToCheckMenuItem "view_sidebar"
       openMenuItem       <- getWidget castToMenuItem "openMenuItem"
       saveAsPDFMenuItem  <- getWidget castToMenuItem "saveAsPDFMenuItem"
       saveAsPNGMenuItem  <- getWidget castToMenuItem "saveAsPNGMenuItem"
       reloadMenuItem     <- getWidget castToMenuItem "view_reload"
       quitMenuItem       <- getWidget castToMenuItem "quitMenuItem"
       aboutMenuItem      <- getWidget castToMenuItem "aboutMenuItem"

       timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
       timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
       timelineKeyDrawingArea   <- getWidget castToDrawingArea "timeline_key_drawingarea"
       timelineHScrollbar  <- getWidget castToHScrollbar "timeline_hscroll"
       timelineVScrollbar  <- getWidget castToVScrollbar "timeline_vscroll"
       timelineAdj         <- rangeGetAdjustment timelineHScrollbar
       timelineVAdj        <- rangeGetAdjustment timelineVScrollbar

       timelineTraces     <- newIORef []
       timelinePrevView   <- newIORef Nothing

       zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
       zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
       zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

       showLabelsToggle   <- getWidget castToToggleToolButton "cpus_showlabels"
       firstButton        <- getWidget castToToolButton "cpus_first"
       lastButton         <- getWidget castToToolButton "cpus_last"
       centreButton       <- getWidget castToToolButton "cpus_centre"

       eventsFontExtents  <- newIORef (error "eventsFontExtents")
       eventsCursorIORef  <- newIORef Nothing
       eventsVScrollbar   <- getWidget castToVScrollbar "eventsVScroll"
       eventsAdj          <- rangeGetAdjustment eventsVScrollbar
       eventsDrawingArea  <- getWidget castToDrawingArea "eventsDrawingArea"
       eventsTextEntry    <- getWidget castToEntry      "events_entry"
   --  eventsFindButton   <- getWidget castToToolButton "events_find"
       eventsFirstButton  <- getWidget castToToolButton "events_first"
       eventsHomeButton   <- getWidget castToToolButton "events_home"
       eventsLastButton   <- getWidget castToToolButton "events_last"

       sidebarBox         <- getWidget castToWidget   "sidebar"
       bookmarkVBox       <- getWidget castToVBox     "bookmarks_vbox"
       bookmarkTreeView   <- getWidget castToTreeView "bookmark_list"
       tracesTreeView     <- getWidget castToTreeView "traces_list"

       -- Bookmarks
       addBookmarkButton    <- getWidget castToToolButton "add_bookmark_button"
       deleteBookmarkButton <- getWidget castToToolButton "delete_bookmark" 
       gotoBookmarkButton   <- getWidget castToToolButton "goto_bookmark_button"

       bookmarkStore <- New.listStoreNew []
       New.treeViewSetModel bookmarkTreeView bookmarkStore  
       New.treeViewSetHeadersVisible bookmarkTreeView True
       bookmarkColumn <- New.treeViewColumnNew
       New.treeViewColumnSetTitle bookmarkColumn "Time"    
       cell <- New.cellRendererTextNew
       New.treeViewColumnPackStart bookmarkColumn cell True
       New.cellLayoutSetAttributes bookmarkColumn cell bookmarkStore  
          (\record -> [New.cellText := show record ++ " ns"])
       New.treeViewAppendColumn bookmarkTreeView bookmarkColumn

       -- Traces
       tracesStore <- treeStoreNew []
       treeViewSetModel tracesTreeView tracesStore

       return ViewerState { .. }
