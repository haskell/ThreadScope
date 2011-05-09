{-# LANGUAGE CPP #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module GUI.Main (runGUI) where

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import System.Glib.GError (failOnGError)

-- Imports from Haskell library
import Text.Printf
import Control.Monad
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Data.Array

import Paths_threadscope

-- Imports for ThreadScope
import GUI.MainWindow as MainWindow
import GUI.Types
import Events.HECs hiding (Event)
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsView
import GUI.Timeline
import GUI.TraceView
import GUI.BookmarkView
import GUI.KeyView
import GUI.SaveAs
import qualified GUI.ConcurrencyControl as ConcurrencyControl
import qualified GUI.ProgressView as ProgressView

-------------------------------------------------------------------------------

data UIEnv = UIEnv {

       mainWin       :: MainWindow,
       eventsView    :: EventsView,
       timelineWin   :: TimelineView,
       traceView     :: TraceView,
       bookmarkView  :: BookmarkView,
       keyView       :: KeyView,

       eventQueue    :: Chan Event,
       concCtl       :: ConcurrencyControl.ConcurrencyControl
     }

data EventlogState
   = NoEventlogLoaded
   | EventlogLoaded {
       mfilename :: Maybe FilePath, --test traces have no filepath
       hecs      :: HECs,
       cursorTs  :: Timestamp,
       cursorPos :: Int
     }

data FileSaveFormat = FormatPDF | FormatPNG

postEvent :: Chan Event -> Event -> IO ()
postEvent = Chan.writeChan

getEvent ::  Chan Event -> IO Event
getEvent = Chan.readChan

data Event
   = EventOpenDialog
   | EventAboutDialog
   | EventQuit

   | EventFileLoad   FilePath
   | EventTestLoad   String
   | EventFileReload
   | EventFileSave   FileSaveFormat

-- | EventStateClear
   | EventSetState (Maybe FilePath) HECs

   | EventShowSidebar Bool
   | EventShowEvents  Bool

   | EventTimelineJumpStart
   | EventTimelineJumpEnd
   | EventTimelineJumpCursor
   | EventTimelineScrollLeft
   | EventTimelineScrollRight
   | EventTimelineZoomIn
   | EventTimelineZoomOut
   | EventTimelineZoomToFit
   | EventTimelineShowLabels Bool
   | EventTimelineShowBW     Bool

   | EventCursorChangedIndex     Int
   | EventCursorChangedTimestamp Timestamp

   | EventTracesChanged [Trace]

   | EventBookmarkAdd
   | EventBookmarRemove Int

constructUI :: IO UIEnv
constructUI = failOnGError $ do

  builder <- builderNew
  builderAddFromFile builder =<< getDataFileName "threadscope.ui"

  eventQueue <- Chan.newChan
  let post = postEvent eventQueue

  mainWin <- mainWindowNew builder MainWindowActions {
    mainWinOpen          = post EventOpenDialog,
    mainWinSavePDF       = post (EventFileSave FormatPDF),
    mainWinSavePNG       = post (EventFileSave FormatPNG),
    mainWinQuit          = post EventQuit,
    mainWinViewSidebar   = post . EventShowSidebar,
    mainWinViewEvents    = post . EventShowEvents,
    mainWinViewRefresh   = post EventFileReload,
    mainWinAbout         = post EventAboutDialog,
    mainWinJumpStart     = post EventTimelineJumpStart,
    mainWinJumpEnd       = post EventTimelineJumpEnd,
    mainWinJumpCursor    = post EventTimelineJumpCursor,
    mainWinScrollLeft    = post EventTimelineScrollLeft,
    mainWinScrollRight   = post EventTimelineScrollRight,
    mainWinJumpZoomIn    = post EventTimelineZoomIn,
    mainWinJumpZoomOut   = post EventTimelineZoomOut,
    mainWinJumpZoomFit   = post EventTimelineZoomToFit,
    mainWinDisplayLabels = post . EventTimelineShowLabels,
    mainWinViewBW        = post . EventTimelineShowBW
  }

  timelineWin <- timelineViewNew builder TimelineViewActions {
    timelineViewCursorChanged = post . EventCursorChangedTimestamp
  }

  eventsView <- eventsViewNew builder EventsViewActions {
    timelineViewCursorChanged = post . EventCursorChangedIndex
  }

  traceView <- traceViewNew builder TraceViewActions {
    traceViewTracesChanged = post . EventTracesChanged
  }

  bookmarkView <- bookmarkViewNew builder BookmarkViewActions {
    bookmarkViewAddBookmark    = post EventBookmarkAdd,
    bookmarkViewRemoveBookmark = post . EventBookmarRemove,
    bookmarkViewGotoBookmark   = \ts -> post (EventCursorChangedTimestamp ts)
                                     >> post EventTimelineJumpCursor
  }

  keyView <- keyViewNew builder

  concCtl <- ConcurrencyControl.start

  return UIEnv{..}

-------------------------------------------------------------------------------

data LoopDone = LoopDone

eventLoop :: UIEnv -> EventlogState -> IO ()
eventLoop uienv@UIEnv{..} eventlogState = do

    event <- getEvent eventQueue
    next  <- dispatch event eventlogState
    case next of
      Left  LoopDone       -> return ()
      Right eventlogState' -> eventLoop uienv eventlogState'

  where
    dispatch :: Event -> EventlogState -> IO (Either LoopDone EventlogState)

    dispatch EventQuit _ = return (Left LoopDone)

    dispatch EventOpenDialog _ = do
      openFileDialog mainWin $ \filename ->
        post (EventFileLoad filename)
      continue

    dispatch (EventFileLoad filename) _ = do
      forkIO $ loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

    dispatch (EventTestLoad testname) _ = do
      forkIO $ loadEvents Nothing (registerEventsFromTrace testname)
      --TODO: set state to be empty during loading
      continue

    dispatch EventFileReload EventlogLoaded{mfilename = Just filename} = do
      forkIO $ loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

--    dispatch EventClearState _

    dispatch (EventSetState mfilename hecs) _ =
      continueWith EventlogLoaded {
        mfilename = mfilename,
        hecs      = hecs,
        cursorTs  = 0,
        cursorPos = 0
      }

    dispatch (EventFileSave format)
             EventlogLoaded {hecs, mfilename = Just filename} = do
      viewParams <- timelineGetViewParameters timelineWin
      let viewParams' = viewParams {
                          detail     = 1,
                          bwMode     = False,
                          labelsMode = False
                        }
      case format of
        FormatPDF -> saveAsPDF filename hecs viewParams'
        FormatPNG -> saveAsPDF filename hecs viewParams'
      continue

    dispatch EventAboutDialog _ = do
      aboutDialog mainWin
      continue

    dispatch (EventShowSidebar visible) _ = do
      MainWindow.sidebarSetVisibility mainWin visible
      continue

    dispatch (EventShowEvents visible) _ = do
      MainWindow.eventsSetVisibility mainWin visible
      continue

    dispatch EventTimelineJumpStart _ = do
      timelineScrollToBeginning timelineWin
      eventsViewScrollToLine eventsView 0
      continue

    dispatch EventTimelineJumpEnd EventlogLoaded{hecs} = do
      timelineScrollToEnd timelineWin
      let (_,end) = bounds (hecEventArray hecs)
      eventsViewScrollToLine eventsView end
      continue

    dispatch EventTimelineJumpCursor EventlogLoaded{cursorPos} = do
      timelineCentreOnCursor timelineWin --TODO: pass cursorTs here
      eventsViewScrollToLine eventsView cursorPos
      continue

    dispatch EventTimelineScrollLeft  _ = do
      timelineScrollLeft  timelineWin
      continue

    dispatch EventTimelineScrollRight _ = do
      timelineScrollRight timelineWin
      continue
    dispatch EventTimelineZoomIn      _ = do
      timelineZoomIn    timelineWin
      continue
    dispatch EventTimelineZoomOut     _ = do
      timelineZoomOut   timelineWin
      continue
    dispatch EventTimelineZoomToFit   _ = do
      timelineZoomToFit timelineWin
      continue

    dispatch (EventTimelineShowLabels showLabels) _ = do
      timelineSetShowLabels timelineWin showLabels
      continue

    dispatch (EventTimelineShowBW showBW) _ = do
      timelineSetBWMode timelineWin showBW
      continue

    dispatch (EventCursorChangedIndex cursorPos') EventlogLoaded{hecs} = do
      let cursorTs' = eventIndexToTimestamp hecs cursorPos'
      timelineSetCursor   timelineWin cursorTs'
      eventsViewSetCursor eventsView  cursorPos'
      continueWith eventlogState {
        cursorTs  = cursorTs',
        cursorPos = cursorPos'
      }

    dispatch (EventCursorChangedTimestamp cursorTs') EventlogLoaded{hecs} = do
      let cursorPos' = timestampToEventIndex hecs cursorTs'
      timelineSetCursor   timelineWin cursorTs'
      eventsViewSetCursor eventsView  cursorPos'
      continueWith eventlogState {
        cursorTs  = cursorTs',
        cursorPos = cursorPos'
      }

    dispatch (EventTracesChanged traces) _ = do
      timelineWindowSetTraces timelineWin traces
      continue

    dispatch EventBookmarkAdd EventlogLoaded{cursorTs} = do
      bookmarkViewAdd bookmarkView cursorTs
      timelineWindowSetBookmarks timelineWin =<< bookmarkViewGet bookmarkView
      continue

    dispatch (EventBookmarRemove n) _ = do
      bookmarkViewRemove bookmarkView n
      timelineWindowSetBookmarks timelineWin =<< bookmarkViewGet bookmarkView
      continue

    loadEvents mfilename registerEvents = do
      ConcurrencyControl.fullSpeed concCtl $
        ProgressView.withProgress mainWin $ \progress -> do
          (hecs, name, nevents, timespan) <- registerEvents progress
          MainWindow.setFileLoaded mainWin (Just name)
          MainWindow.setStatusMessage mainWin $
            printf "%s (%d events, %.3fs)" name nevents timespan

          eventsViewSetEvents eventsView (Just (hecEventArray hecs))
          traceViewSetHECs traceView hecs
          traces' <- traceViewGetTraces traceView
          timelineWindowSetHECs timelineWin (Just hecs)
          timelineWindowSetTraces timelineWin traces'
          post (EventSetState mfilename hecs)
      return ()

    post = postEvent eventQueue
    continue = continueWith eventlogState
    continueWith = return . Right

-------------------------------------------------------------------------------

runGUI :: FilePath -> String -> Bool -> IO ()
runGUI filename traceName _debug = do
  Gtk.initGUI

  uiEnv <- constructUI

  let post = postEvent (eventQueue uiEnv)

  when (filename /= "") $
    post (EventFileLoad filename)

  -- Likewise for test traces
  when (traceName /= "") $
    post (EventTestLoad traceName)

  forkIO $ do
    eventLoop uiEnv NoEventlogLoaded
    Gtk.mainQuit

#ifndef mingw32_HOST_OS
  installHandler sigINT (Catch $ post EventQuit) Nothing
#endif

  -- Enter Gtk+ main event loop.
  Gtk.mainGUI
