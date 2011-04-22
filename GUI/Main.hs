{-# LANGUAGE CPP #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module GUI.Main (runGUI) where

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import System.Glib.GError (failOnGError)
import Graphics.UI.Gtk.ModelView as New

-- Imports from Haskell library
import Control.Monad
import Data.IORef
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Exception

import Paths_threadscope

-- Imports for ThreadScope
import GUI.State
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsWindow
import GUI.Timeline
import GUI.SaveAsPDF
import GUI.SaveAsPNG
import GUI.Sidebar
import qualified GUI.ConcurrencyControl as ConcurrencyControl

-------------------------------------------------------------------------------

runGUI :: FilePath -> String -> Bool -> IO ()
runGUI filename traceName debug = do
  Gtk.initGUI

  startup filename traceName debug

#ifndef mingw32_HOST_OS
  --TODO: this seems suspicious, it should not be necessary.
  -- If it is necessary perhaps mainQuit is better than thowing an exception.
  installHandler sigINT (Catch (postGUIAsync (throw UserInterrupt))) Nothing
#endif

  -- Enter Gtk+ main event loop.
  Gtk.mainGUI

-------------------------------------------------------------------------------

startup :: FilePath -> String -> Bool -> IO ()
startup filename traceName debug
  = failOnGError $ do

       builder <- builderNew
       builderAddFromFile builder =<< getDataFileName "threadscope.ui"
       let getWidget cast name = builderGetObject builder cast name

       filenameIORef <- newIORef Nothing

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       hecsIORef         <- newIORef Nothing
       scaleIORef        <- newIORef defaultScaleValue
       cursorIORef       <- newIORef 0

       mainWindow         <- getWidget castToWindow "main_window"
       statusBar          <- getWidget castToStatusbar "statusbar"

       bwToggle           <- getWidget castToCheckMenuItem "black_and_white"
       sidebarToggle      <- getWidget castToCheckMenuItem "view_sidebar"
       eventsToggle       <- getWidget castToCheckMenuItem "view_events"
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

       timelinePrevView   <- newIORef Nothing

       zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
       zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
       zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

       showLabelsToggle   <- getWidget castToToggleToolButton "cpus_showlabels"
       firstButton        <- getWidget castToToolButton "cpus_first"
       lastButton         <- getWidget castToToolButton "cpus_last"
       centreButton       <- getWidget castToToolButton "cpus_centre"

       --TODO: these two are currently unbound, but they should be!
   --  eventsTextEntry    <- getWidget castToEntry      "events_entry"
   --  eventsFindButton   <- getWidget castToToolButton "events_find"

       bookmarkTreeView   <- getWidget castToTreeView "bookmark_list"

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
       --FIXME: this should almost certainly be constructed elsewhere
       -- e.g. Traces or Sidebar
       tracesStore <- treeStoreNew []

       concCtl <- ConcurrencyControl.start

       let state = ViewerState { .. }

       ------------------------------------------------------------------------

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
         openFileDialog mainWindow $ \filename ->
           registerEventsFromFile filename state

       ------------------------------------------------------------------------
       -- zoom buttons

       --TODO: these should be passed a timeline, not global state.

       zoomInButton  `onToolButtonClicked` timelineZoomIn    state
       zoomOutButton `onToolButtonClicked` timelineZoomOut   state
       zoomFitButton `onToolButtonClicked` timelineZoomToFit state

       firstButton  `onToolButtonClicked` timelineScrollToBeginning state
       lastButton   `onToolButtonClicked` timelineScrollToEnd state
       centreButton `onToolButtonClicked` timelineCentreOnCursor state

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

       eventsWin <- eventsWindowNew debug builder hecsIORef cursorIORef

       on eventsToggle checkMenuItemToggled $ do
         showEvents <- checkMenuItemGetActive eventsToggle
         eventsWindowSetVisibility eventsWin showEvents

       onToolButtonClicked firstButton $
         eventsWindowJumpToBeginning eventsWin

       onToolButtonClicked lastButton $
         eventsWindowJumpToEnd eventsWin

       onToolButtonClicked centreButton $ do
         cursorpos <- eventsWindowGetCursorLine eventsWin
         eventsWindowJumpToPosition eventsWin cursorpos


       ------------------------------------------------------------------------
       -- Sidebar

       sidebar <- sidebarNew tracesStore builder SidebarActions {
           sidebarTraceToggled = timelineParamsChanged state
         }
       on sidebarToggle checkMenuItemToggled $
         sidebarSetVisibility sidebar =<< checkMenuItemGetActive sidebarToggle

       --TODO: probably should move the bodies of these handlers elsewhere

       -- Button for adding the cursor position to the boomark list
       onToolButtonClicked addBookmarkButton  $ do
         when debug $ putStrLn "Add bookmark\n"
         cursorPos <- readIORef cursorIORef
         New.listStoreAppend bookmarkStore cursorPos
         queueRedrawTimelines state

       -- Button for deleting a bookmark
       onToolButtonClicked deleteBookmarkButton  $ do
         when debug $ putStrLn "Delete bookmark\n"
         sel <- treeViewGetSelection bookmarkTreeView
         selection <- treeSelectionGetSelected sel
         case selection of
           Nothing -> return ()
           Just (TreeIter _ pos _ _) -> listStoreRemove bookmarkStore (fromIntegral pos)
         queueRedrawTimelines state

       -- Button for jumping to bookmark
       onToolButtonClicked gotoBookmarkButton $ do
         sel <- treeViewGetSelection bookmarkTreeView
         selection <- treeSelectionGetSelected sel
         case selection of
           Nothing -> return ()
           Just (TreeIter _ pos _ _) -> do
             l <- listStoreToList bookmarkStore
             when debug $ putStrLn ("gotoBookmark: " ++ show l++ " pos = " ++ show pos)
             setCursorToTime state (l!!(fromIntegral pos))
         queueRedrawTimelines state

       ------------------------------------------------------------------------
       -- Quit
       quitMenuItem `onActivateLeaf` mainQuit

       ------------------------------------------------------------------------
       -- About dialog
       aboutMenuItem `onActivateLeaf` aboutDialog mainWindow

       ------------------------------------------------------------------------
       -- Quit behaviour
       onDestroy mainWindow mainQuit

       ------------------------------------------------------------------------
       -- Show all windows
       widgetShowAll mainWindow
