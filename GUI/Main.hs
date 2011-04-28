{-# LANGUAGE CPP, DoRec #-}
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
import GUI.MainWindow
import GUI.State
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsWindow
import GUI.Timeline
import GUI.Timeline.Motion (scrollLeft, scrollRight)
import GUI.SaveAs
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

       --TODO: eliminate these remaining getWidget calls here.
       mainWindow         <- getWidget castToWindow "main_window"
       statusBar          <- getWidget castToStatusbar "statusbar"
       bwToggle           <- getWidget castToCheckMenuItem "black_and_white"
       timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
       timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
       timelineHScrollbar  <- getWidget castToHScrollbar "timeline_hscroll"
       timelineVScrollbar  <- getWidget castToVScrollbar "timeline_vscroll"
       timelineAdj         <- rangeGetAdjustment timelineHScrollbar
       timelineVAdj        <- rangeGetAdjustment timelineVScrollbar

       timelinePrevView   <- newIORef Nothing

       showLabelsToggle   <- getWidget castToToggleToolButton "cpus_showlabels"
       bookmarkTreeView   <- getWidget castToTreeView "bookmark_list"

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

       rec mainWin <- mainWindowNew builder MainWindowActions {
               mainWinOpen          = openFileDialog mainWindow $ \filename ->
                                        registerEventsFromFile filename state
                                                               timelineWin eventsWin,
               mainWinSavePDF       = saveAsPDF state,
               mainWinSavePNG       = saveAsPNG state,
               mainWinQuit          = mainQuit,
               mainWinViewSidebar   = sidebarSetVisibility sidebar,
               mainWinViewEvents    = eventsWindowSetVisibility eventsWin,
               mainWinViewBW        = \_ -> timelineParamsChanged state timelineWin,
               mainWinViewRefresh   = do
                 mb_filename <- readIORef filenameIORef
                 case mb_filename of
                   Nothing -> return ()
                   Just filename -> registerEventsFromFile filename state
                                                           timelineWin eventsWin,

               mainWinAbout         = aboutDialog mainWindow,

               -- Toolbar actions
               mainWinJumpStart     = do timelineScrollToBeginning state
                                         eventsWindowJumpToEnd eventsWin,
               mainWinJumpEnd       = do timelineScrollToEnd state
                                         eventsWindowJumpToEnd eventsWin,
               mainWinJumpCursor    = do timelineCentreOnCursor state
                                         cursorpos <- eventsWindowGetCursorLine eventsWin
                                         eventsWindowJumpToPosition eventsWin cursorpos,

               mainWinScrollLeft    = scrollLeft  state,
               mainWinScrollRight   = scrollRight state,
               mainWinJumpZoomIn    = timelineZoomIn    state,
               mainWinJumpZoomOut   = timelineZoomOut   state,
               mainWinJumpZoomFit   = timelineZoomToFit state,
               mainWinDisplayLabels = timelineParamsChanged state timelineWin,

               mainWinAddBookmark    = do
                 when debug $ putStrLn "Add bookmark\n"
                 cursorPos <- readIORef cursorIORef
                 New.listStoreAppend bookmarkStore cursorPos
                 queueRedrawTimelines state,

               mainWinRemoveBookmark = do
                 when debug $ putStrLn "Delete bookmark\n"
                 sel <- treeViewGetSelection bookmarkTreeView
                 selection <- treeSelectionGetSelected sel
                 case selection of
                   Nothing -> return ()
                   Just (TreeIter _ pos _ _) -> listStoreRemove bookmarkStore (fromIntegral pos)
                 queueRedrawTimelines state,

               mainWinGotoBookmark   = do
                 sel <- treeViewGetSelection bookmarkTreeView
                 selection <- treeSelectionGetSelected sel
                 case selection of
                   Nothing -> return ()
                   Just (TreeIter _ pos _ _) -> do
                     l <- listStoreToList bookmarkStore
                     when debug $ putStrLn ("gotoBookmark: " ++ show l++ " pos = " ++ show pos)
                     setCursorToTime state timelineWin (l!!(fromIntegral pos))
                 queueRedrawTimelines state
             }

           eventsWin <- eventsWindowNew debug builder hecsIORef cursorIORef

           timelineWin <- timelineWindowNew debug builder state scaleIORef cursorIORef

           sidebar <- sidebarNew tracesStore builder SidebarActions {
               sidebarTraceToggled = timelineParamsChanged state timelineWin
             }

       ------------------------------------------------------------------------

       writeIORef filenameIORef (if filename == "" then
                                   Nothing
                                 else
                                   Just filename)

       ------------------------------------------------------------------------
       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for
       -- the capabilities and event array.
       when (filename /= "") $ registerEventsFromFile filename state timelineWin eventsWin

       -- Likewise for test traces
       when (traceName /= "") $ registerEventsFromTrace traceName state timelineWin eventsWin
