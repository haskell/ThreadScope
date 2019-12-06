{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module GUI.Main (runGUI) where

-- Imports for GTK
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.GError (failOnGError)

-- Imports from Haskell library
import Text.Printf
#ifndef mingw32_HOST_OS
import System.Posix
#endif
import Control.Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Exception
import Data.Array
import Data.Maybe

-- Imports for ThreadScope
import qualified GUI.App as App
import qualified GUI.MainWindow as MainWindow
import GUI.Types
import Events.HECs hiding (Event)
import GUI.DataFiles (ui)
import GUI.Dialogs
import Events.ReadEvents
import GUI.EventsView
import GUI.SummaryView
import GUI.StartupInfoView
import GUI.Histogram
import GUI.Timeline
import GUI.TraceView
import GUI.BookmarkView
import GUI.KeyView
import GUI.SaveAs
import qualified GUI.ConcurrencyControl as ConcurrencyControl
import qualified GUI.ProgressView as ProgressView
import qualified GUI.GtkExtras as GtkExtras

-------------------------------------------------------------------------------

data UIEnv = UIEnv {

       mainWin       :: MainWindow.MainWindow,
       eventsView    :: EventsView,
       startupView   :: StartupInfoView,
       summaryView   :: SummaryView,
       histogramView :: HistogramView,
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
       selection :: TimeSelection,
       cursorPos :: Int
     }

postEvent :: Chan Event -> Event -> IO ()
postEvent = Chan.writeChan

getEvent ::  Chan Event -> IO Event
getEvent = Chan.readChan

data Event
   = EventOpenDialog
   | EventExportDialog
   | EventLaunchWebsite
   | EventLaunchTutorial
   | EventAboutDialog
   | EventQuit

   | EventFileLoad   FilePath
   | EventTestLoad   String
   | EventFileReload
   | EventFileExport FilePath FileExportFormat

-- | EventStateClear
   | EventSetState HECs (Maybe FilePath) String Int Double

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
   | EventTimelineLabelsMode Bool
   | EventTimelineShowBW     Bool

   | EventCursorChangedIndex     Int
   | EventCursorChangedSelection TimeSelection

   | EventTracesChanged [Trace]

   | EventBookmarkAdd
   | EventBookmarkRemove Int
   | EventBookmarkEdit   Int String

   | EventUserError String SomeException
                    -- can add more specific ones if necessary

constructUI :: IO UIEnv
constructUI = failOnGError $ do

  builder <- Gtk.builderNew
  Gtk.builderAddFromString builder $ui

  eventQueue <- Chan.newChan
  let post = postEvent eventQueue

  mainWin <- MainWindow.mainWindowNew builder MainWindow.MainWindowActions {
    mainWinOpen          = post EventOpenDialog,
    mainWinExport        = post EventExportDialog,
    mainWinQuit          = post EventQuit,
    mainWinViewSidebar   = post . EventShowSidebar,
    mainWinViewEvents    = post . EventShowEvents,
    mainWinViewReload    = post EventFileReload,
    mainWinWebsite       = post EventLaunchWebsite,
    mainWinTutorial      = post EventLaunchTutorial,
    mainWinAbout         = post EventAboutDialog,
    mainWinJumpStart     = post EventTimelineJumpStart,
    mainWinJumpEnd       = post EventTimelineJumpEnd,
    mainWinJumpCursor    = post EventTimelineJumpCursor,
    mainWinScrollLeft    = post EventTimelineScrollLeft,
    mainWinScrollRight   = post EventTimelineScrollRight,
    mainWinJumpZoomIn    = post EventTimelineZoomIn,
    mainWinJumpZoomOut   = post EventTimelineZoomOut,
    mainWinJumpZoomFit   = post EventTimelineZoomToFit,
    mainWinDisplayLabels = post . EventTimelineLabelsMode,
    mainWinViewBW        = post . EventTimelineShowBW
  }

  timelineWin <- timelineViewNew builder TimelineViewActions {
    timelineViewSelectionChanged = post . EventCursorChangedSelection
  }

  eventsView <- eventsViewNew builder EventsViewActions {
    eventsViewCursorChanged = post . EventCursorChangedIndex
  }

  startupView <- startupInfoViewNew builder
  summaryView <- summaryViewNew builder

  histogramView <- histogramViewNew builder

  traceView <- traceViewNew builder TraceViewActions {
    traceViewTracesChanged = post . EventTracesChanged
  }

  bookmarkView <- bookmarkViewNew builder BookmarkViewActions {
    bookmarkViewAddBookmark    = post EventBookmarkAdd,
    bookmarkViewRemoveBookmark = post . EventBookmarkRemove,
    bookmarkViewGotoBookmark   = \ts -> do
      post (EventCursorChangedSelection (PointSelection ts))
      post EventTimelineJumpCursor,
    bookmarkViewEditLabel      = \n v -> post (EventBookmarkEdit n v)
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
#if __GLASGOW_HASKELL__ <= 612
               -- workaround for a wierd exception handling bug in ghc-6.12
               `catch` \e -> throwIO (e :: SomeException)
#endif
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
      async "loading the eventlog" $
        loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

    dispatch (EventTestLoad testname) _ = do
      async "loading the test eventlog" $
        loadEvents Nothing (registerEventsFromTrace testname)
      --TODO: set state to be empty during loading
      continue

    dispatch EventFileReload EventlogLoaded{mfilename = Just filename} = do
      async "reloading the eventlog" $
        loadEvents (Just filename) (registerEventsFromFile filename)
      --TODO: set state to be empty during loading
      continue

    dispatch EventFileReload EventlogLoaded{mfilename = Nothing} =
      continue

--    dispatch EventClearState _

    dispatch (EventSetState hecs mfilename name nevents timespan) _ =

     -- We have to draw this ASAP, before the user manages to move
     -- the mouse away from the window, or the window is left
     -- in a partially drawn state.
     ConcurrencyControl.fullSpeed concCtl $ do

      MainWindow.setFileLoaded mainWin (Just name)
      MainWindow.setStatusMessage mainWin $
        printf "%s (%d events, %.3fs)" name nevents timespan

      let mevents = Just $ hecEventArray hecs
      eventsViewSetEvents eventsView mevents
      startupInfoViewSetEvents startupView mevents
      summaryViewSetEvents summaryView mevents
      histogramViewSetHECs histogramView (Just hecs)
      traceViewSetHECs traceView hecs
      traces' <- traceViewGetTraces traceView
      timelineWindowSetHECs timelineWin (Just hecs)
      timelineWindowSetTraces timelineWin traces'

      -- We set user 'traceMarker' events as initial bookmarks.
      let usrMarkers = extractUserMarkers hecs
      bookmarkViewClear bookmarkView
      sequence_ [ bookmarkViewAdd bookmarkView ts label
                | (ts, label) <- usrMarkers ]
      timelineWindowSetBookmarks timelineWin (map fst usrMarkers)

      if nevents == 0
        then continueWith NoEventlogLoaded
        else continueWith EventlogLoaded
          { mfilename = mfilename
          , hecs      = hecs
          , selection = PointSelection 0
          , cursorPos = 0
          }

    dispatch EventExportDialog
             EventlogLoaded {mfilename} = do
      exportFileDialog mainWin (fromMaybe "" mfilename) $ \filename' format ->
        post (EventFileExport filename' format)
      continue

    dispatch (EventFileExport filename format)
             EventlogLoaded {hecs} = do
      viewParams <- timelineGetViewParameters timelineWin
      let viewParams' = viewParams {
                          detail     = 1,
                          bwMode     = False,
                          labelsMode = False
                        }
      let yScaleArea = timelineGetYScaleArea timelineWin
      case format of
        FormatPDF ->
          saveAsPDF filename hecs viewParams' yScaleArea
        FormatPNG ->
          saveAsPNG filename hecs viewParams' yScaleArea
      continue

    dispatch EventLaunchWebsite _ = do
      GtkExtras.launchProgramForURI "http://www.haskell.org/haskellwiki/ThreadScope"
      continue

    dispatch EventLaunchTutorial _ = do
      GtkExtras.launchProgramForURI "http://www.haskell.org/haskellwiki/ThreadScope_Tour"
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
      timelineCentreOnCursor timelineWin --TODO: pass selection here
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

    dispatch (EventTimelineLabelsMode labelsMode) _ = do
      timelineSetLabelsMode timelineWin labelsMode
      continue

    dispatch (EventTimelineShowBW showBW) _ = do
      timelineSetBWMode timelineWin showBW
      continue

    dispatch (EventCursorChangedIndex cursorPos') EventlogLoaded{hecs} = do
      let cursorTs'  = eventIndexToTimestamp hecs cursorPos'
          selection' = PointSelection cursorTs'
      timelineSetSelection timelineWin selection'
      eventsViewSetCursor eventsView  cursorPos' Nothing
      continueWith eventlogState {
        selection = selection',
        cursorPos = cursorPos'
      }

    dispatch (EventCursorChangedSelection selection'@(PointSelection cursorTs'))
             EventlogLoaded{hecs} = do
      let cursorPos' = timestampToEventIndex hecs cursorTs'
      timelineSetSelection timelineWin selection'
      eventsViewSetCursor eventsView cursorPos' Nothing
      histogramViewSetInterval histogramView Nothing
      summaryViewSetInterval summaryView Nothing
      continueWith eventlogState {
        selection = selection',
        cursorPos = cursorPos'
      }

    dispatch (EventCursorChangedSelection selection'@(RangeSelection start end))
             EventlogLoaded{hecs} = do
      let cursorPos' = timestampToEventIndex hecs start
          mrange = Just (cursorPos', timestampToEventIndex hecs end)
      timelineSetSelection timelineWin selection'
      eventsViewSetCursor eventsView cursorPos' mrange
      histogramViewSetInterval histogramView (Just (start, end))
      summaryViewSetInterval summaryView (Just (start, end))
      continueWith eventlogState {
        selection = selection',
        cursorPos = cursorPos'
      }

    dispatch (EventTracesChanged traces) _ = do
      timelineWindowSetTraces timelineWin traces
      continue

    dispatch EventBookmarkAdd EventlogLoaded{selection} = do
      case selection of
        PointSelection a   -> bookmarkViewAdd bookmarkView a ""
        RangeSelection a b -> do bookmarkViewAdd bookmarkView a ""
                                 bookmarkViewAdd bookmarkView b ""
      --TODO: should have a way to add/set a single bookmark for the timeline
      -- rather than this hack where we ask the bookmark view for the whole lot.
      ts <- bookmarkViewGet bookmarkView
      timelineWindowSetBookmarks timelineWin (map fst ts)
      continue

    dispatch (EventBookmarkRemove n) _ = do
      bookmarkViewRemove bookmarkView n
      --TODO: should have a way to add/set a single bookmark for the timeline
      -- rather than this hack where we ask the bookmark view for the whole lot.
      ts <- bookmarkViewGet bookmarkView
      timelineWindowSetBookmarks timelineWin (map fst ts)
      continue

    dispatch (EventBookmarkEdit n v) _ = do
      bookmarkViewSetLabel bookmarkView n v
      continue

    dispatch (EventUserError doing exception) _ = do
      let headline    = "There was a problem " ++ doing ++ "."
          explanation = show exception
      errorMessageDialog mainWin headline explanation
      continue

    dispatch _ NoEventlogLoaded = continue

    loadEvents mfilename registerEvents = do
      ConcurrencyControl.fullSpeed concCtl $
        ProgressView.withProgress mainWin $ \progress -> do
          (hecs, name, nevents, timespan) <- registerEvents progress
          -- This is a desperate hack to avoid the "segfault on reload" bug
          -- http://trac.haskell.org/ThreadScope/ticket/1
          -- It should be enough to let other threads finish and so avoid
          -- re-entering gtk C code (see ticket for the dirty details).
          --
          -- Unfortunately it halts drawing of the loaded events if the user
          -- manages to move the mouse away from the window during the delay.
          --   threadDelay 100000 -- 1/10th of a second
          post (EventSetState hecs mfilename name nevents timespan)
      return ()

    async doing action =
      forkIO (action `catch` \e -> post (EventUserError doing e))

    post = postEvent eventQueue
    continue = continueWith eventlogState
    continueWith = return . Right

-------------------------------------------------------------------------------

runGUI :: Maybe (Either FilePath String) -> IO ()
runGUI initialTrace = do
  App.initApp

  uiEnv <- constructUI

  let post = postEvent (eventQueue uiEnv)

  case initialTrace of
   Nothing                -> return ()
   Just (Left  filename)  -> post (EventFileLoad filename)
   Just (Right traceName) -> post (EventTestLoad traceName)

  doneVar <- newEmptyMVar

  forkIO $ do
    res <- try $ eventLoop uiEnv NoEventlogLoaded
    Gtk.mainQuit
    putMVar doneVar (res :: Either SomeException ())

#ifndef mingw32_HOST_OS
  installHandler sigINT (Catch $ post EventQuit) Nothing
#endif

  -- Enter Gtk+ main event loop.
  Gtk.mainGUI

  -- Wait for child event loop to terminate
  -- This lets us wait for any exceptions.
  either throwIO return =<< takeMVar doneVar
