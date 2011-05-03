{-# LANGUAGE CPP, DoRec #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module GUI.Main (runGUI) where

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import System.Glib.GError (failOnGError)

-- Imports from Haskell library
import Text.Printf
import Control.Monad
import Data.IORef
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Exception
import Control.Concurrent

import Paths_threadscope

-- Imports for ThreadScope
import GUI.MainWindow as MainWindow
import GUI.Types
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsWindow
import GUI.Timeline
import GUI.TraceView
import GUI.BookmarkView
import GUI.SaveAs
import qualified GUI.ConcurrencyControl as ConcurrencyControl
import qualified GUI.ProgressView as ProgressView

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
startup filename traceName _debug --Note: debug not currently used
  = failOnGError $ do

       builder <- builderNew
       builderAddFromFile builder =<< getDataFileName "threadscope.ui"

       filenameIORef <- newIORef Nothing

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       hecsIORef         <- newIORef Nothing
       cursorIORef       <- newIORef 0

       concCtl <- ConcurrencyControl.start

       rec mainWin <- mainWindowNew builder MainWindowActions {
               mainWinOpen          = openFileDialog mainWin $ \filename -> do
                                        loadEvents (registerEventsFromFile filename)
                                        writeIORef filenameIORef (Just filename),
               mainWinSavePDF       = do
                 --FIXME: sort out the view paramater state issue
                 viewParams <- timelineGetViewParameters timelineWin
                 let viewParams' = viewParams {
                                     detail     = 1,
                                     bwMode     = False,
                                     labelsMode = False
                                   }
                 mb_fn   <- readIORef filenameIORef
                 mb_hecs <- readIORef hecsIORef
                 case (mb_fn, mb_hecs) of
                   (Just fn, Just hecs) -> saveAsPDF fn hecs viewParams'
                   _                    -> return (),

               mainWinSavePNG       = do
                 --FIXME: eliminate the duplication between the two save formats
                 viewParams <- timelineGetViewParameters timelineWin
                 let viewParams' = viewParams {
                                     detail     = 1,
                                     bwMode     = False,
                                     labelsMode = False
                                   }
                 mb_fn   <- readIORef filenameIORef
                 mb_hecs <- readIORef hecsIORef
                 case (mb_fn, mb_hecs) of
                   (Just fn, Just hecs) -> saveAsPNG fn hecs viewParams'
                   _                    -> return (),

               mainWinQuit          = mainQuit,
               mainWinViewSidebar   = MainWindow.sidebarSetVisibility mainWin,
               mainWinViewEvents    = MainWindow.eventsSetVisibility mainWin,
               mainWinViewBW        = timelineSetBWMode timelineWin,
               mainWinViewRefresh   = do
                 mb_filename <- readIORef filenameIORef
                 case mb_filename of
                   Nothing -> return ()
                   Just filename -> loadEvents (registerEventsFromFile filename),

               mainWinAbout         = aboutDialog mainWin,

               -- Toolbar actions
               mainWinJumpStart     = do timelineScrollToBeginning timelineWin
                                         eventsWindowJumpToEnd eventsWin,
               mainWinJumpEnd       = do timelineScrollToEnd timelineWin
                                         eventsWindowJumpToEnd eventsWin,
               mainWinJumpCursor    = do timelineCentreOnCursor timelineWin
                                         --FIXME: sync the cursor of the timeline and events windows
                                         cursorpos <- eventsWindowGetCursorLine eventsWin
                                         eventsWindowJumpToPosition eventsWin cursorpos,

               mainWinScrollLeft    = timelineScrollLeft  timelineWin,
               mainWinScrollRight   = timelineScrollRight timelineWin,
               mainWinJumpZoomIn    = timelineZoomIn    timelineWin,
               mainWinJumpZoomOut   = timelineZoomOut   timelineWin,
               mainWinJumpZoomFit   = timelineZoomToFit timelineWin,
               mainWinDisplayLabels = timelineSetShowLabels timelineWin
             }

           eventsWin <- eventsWindowNew builder

           timelineWin <- timelineViewNew builder TimelineViewActions {
               timelineViewCursorChanged = \ts -> do
                 writeIORef cursorIORef ts
                 timelineSetCursor timelineWin ts
             }

           traceView <- traceViewNew builder TraceViewActions {
               traceViewTracesChanged = timelineWindowSetTraces timelineWin
             }

           bookmarkView <- bookmarkViewNew builder BookmarkViewActions {
               bookmarkViewAddBookmark = do
                 cursorPos <- readIORef cursorIORef
                 bookmarkViewAdd bookmarkView cursorPos
                 timelineWindowSetBookmarks timelineWin =<< bookmarkViewGet bookmarkView,

               bookmarkViewRemoveBookmark = \n -> do
                 bookmarkViewRemove bookmarkView n
                 timelineWindowSetBookmarks timelineWin =<< bookmarkViewGet bookmarkView,

               bookmarkViewGotoBookmark = \ts -> do
                 timelineSetCursor timelineWin ts
                 timelineCentreOnCursor timelineWin
                 --FIXME: set the cursor in the evnets window too
                 --eventsWindowJumpToTimestamp eventsWin ts
             }

           let loadEvents registerEvents = do
                 forkIO $ do
                   ConcurrencyControl.fullSpeed concCtl $
                     ProgressView.withProgress mainWin $ \progress -> do
                       (hecs, file, nevents, timespan) <- registerEvents progress
                       MainWindow.setFileLoaded mainWin (Just file)
                       MainWindow.setStatusMessage mainWin $
                         printf "%s (%d events, %.3fs)" file nevents timespan

                       eventsWindowSetEvents eventsWin (Just (hecEventArray hecs))
                       traceViewSetHECs traceView hecs
                       traces' <- traceViewGetTraces traceView
                       timelineWindowSetHECs timelineWin (Just hecs)
                       timelineWindowSetTraces timelineWin traces'

                       -- note, this hecsIORef is not shared with the TimelineWindow
                       writeIORef hecsIORef (Just hecs)

                       --FIXME: note, not all state is reset on loading a file:
                       -- events cursor pos, timeline cursor pos, current view pos

                   return ()
                 return ()

       ------------------------------------------------------------------------

       writeIORef filenameIORef (if filename == "" then
                                   Nothing
                                 else
                                   Just filename)

       ------------------------------------------------------------------------
       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for
       -- the capabilities and event array.
       when (filename /= "") $ loadEvents (registerEventsFromFile filename)

       -- Likewise for test traces
       when (traceName /= "") $ loadEvents (registerEventsFromTrace traceName)
