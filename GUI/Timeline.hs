module GUI.Timeline (
    TimelineWindow,
    timelineWindowNew,

    timelineSetBWMode,

    --TODO: this group needs reviewing
    renderTraces,
    timelineParamsChanged,
    defaultScaleValue,
    queueRedrawTimelines,
    setCursorToTime,

    timelineZoomIn,
    timelineZoomOut,
    timelineZoomToFit,
    timelineScrollToBeginning,
    timelineScrollToEnd,
    timelineCentreOnCursor,
 ) where

import GUI.Timeline.Types (TimelineWindow(..))
import GUI.Timeline.Motion
import GUI.Timeline.Render
import GUI.Timeline.Key

import GUI.Types
import GHC.RTS.Events

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Old hiding (eventModifier)
import Graphics.UI.Gtk.Gdk.EventM as New
import Graphics.Rendering.Cairo  as C

import Data.Maybe
import Data.IORef
import Control.Monad
import Text.Printf
-- import Debug.Trace

-----------------------------------------------------------------------------
-- The CPUs view

-- | Draw some parts of the timeline in black and white rather than colour.
--
timelineSetBWMode :: TimelineWindow -> Bool -> IO ()
timelineSetBWMode timelineWin bwmode = do
  writeIORef (bwmodeIORef timelineWin) bwmode
  widgetQueueDraw (timelineDrawingArea timelineWin)

-----------------------------------------------------------------------------

timelineWindowNew :: Bool -> Builder
                  -> ListStore Timestamp -> TreeStore (Trace, Bool) -> IORef (Maybe HECs) -> IORef Double -> IORef Timestamp --TODO: eliminate
                  -> IO TimelineWindow
timelineWindowNew debug builder bookmarkStore tracesStore hecsIORef scaleIORef cursorIORef = do

  let getWidget cast = builderGetObject builder cast
  timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
  timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
  timelineKeyDrawingArea   <- getWidget castToDrawingArea "timeline_key_drawingarea"
  timelineHScrollbar       <- getWidget castToHScrollbar "timeline_hscroll"
  timelineVScrollbar       <- getWidget castToVScrollbar "timeline_vscroll"
  timelineAdj              <- rangeGetAdjustment timelineHScrollbar
  timelineVAdj             <- rangeGetAdjustment timelineVScrollbar
  showLabelsToggle         <- getWidget castToToggleToolButton "cpus_showlabels"

  bwmodeIORef <- newIORef False
  timelinePrevView <- newIORef Nothing

  let timelineWin = TimelineWindow {..}

  ------------------------------------------------------------------------
  -- Key presses

  --TODO: should not have access to mainWindow here, should attach events in
  -- MainWindow and manage actions in main interaction module.
  mainWindow <- getWidget castToWindow "main_window"
  onKeyPress mainWindow $ \Key { Old.eventKeyName = key, eventKeyChar = mch } -> do
    -- when debug $ putStrLn ("key " ++ key)
    case key of
      "Escape" -> mainQuit >> return True
      "Right"  -> do scrollRight timelineWin; return True
      "Left"   -> do scrollLeft  timelineWin; return True
      _ -> if isJust mch then
             case fromJust mch of
               '+' -> do zoomIn  timelineWin; return True
               '-' -> do zoomOut timelineWin; return True
               _   -> return True
           else
             return True

  ------------------------------------------------------------------------
  -- Porgram the callback for the capability drawingArea
  timelineLabelDrawingArea `onExpose` \_ -> do
    updateLabelDrawingArea timelineWin
    return True

  ------------------------------------------------------------------------
  -- Set-up the key timelineDrawingArea.
  timelineKeyDrawingArea `onExpose` updateKeyDrawingArea timelineKeyDrawingArea

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  on timelineDrawingArea scrollEvent $ tryEvent $ do
    dir <- eventScrollDirection
    mods <- eventModifier
    liftIO $ do
    case (dir,mods) of
      (ScrollUp,   [Control]) -> zoomIn timelineWin
      (ScrollDown, [Control]) -> zoomOut timelineWin
      (ScrollUp,   [])        -> vscrollUp timelineWin
      (ScrollDown, [])        -> vscrollDown timelineWin
      _ -> return ()

  ------------------------------------------------------------------------
  -- Mouse button

  onButtonPress timelineDrawingArea $ \button -> do
     when debug $ putStrLn ("button pressed: " ++ show button)
     case button of
       Button{ Old.eventButton = LeftButton, Old.eventClick = SingleClick,
               -- eventModifier = [],  -- contains [Alt2] for me
               eventX = x } -> do
           setCursor debug timelineWin x
           return True
       _other -> do
           return False

  onValueChanged timelineAdj  $ queueRedrawTimelines timelineWin
  onValueChanged timelineVAdj $ queueRedrawTimelines timelineWin

  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- New.eventRegion
     liftIO $ do
       bwmode <- readIORef bwmodeIORef
       exposeTraceView timelineWin bwmode exposeRegion
     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureTimelineDrawingArea timelineWin
     return True

  return timelineWin

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

timelineParamsChanged :: TimelineWindow -> IO ()
timelineParamsChanged timelineWin = do
  queueRedrawTimelines timelineWin
  updateTimelineVScroll timelineWin

configureTimelineDrawingArea :: TimelineWindow -> IO ()
configureTimelineDrawingArea timelineWin = do
  updateTimelineVScroll timelineWin
  updateTimelineHPageSize timelineWin

updateTimelineVScroll :: TimelineWindow -> IO ()
updateTimelineVScroll timelineWin@TimelineWindow{..} = do
  h <- calculateTotalTimelineHeight timelineWin
  (_,winh) <- widgetGetSize timelineDrawingArea
  let winh' = fromIntegral winh; h' = fromIntegral h
  adjustmentSetLower    timelineVAdj 0
  adjustmentSetUpper    timelineVAdj h'

  val <- adjustmentGetValue timelineVAdj
  when (val > h') $ adjustmentSetValue timelineVAdj h'

  set timelineVAdj [
      adjustmentPageSize      := winh',
      adjustmentStepIncrement := winh' * 0.1,
      adjustmentPageIncrement := winh' * 0.9
    ]

-- when the drawing area is resized, we update the page size of the
-- adjustment.  Everything else stays the same: we don't scale or move
-- the view at all.
updateTimelineHPageSize :: TimelineWindow -> IO ()
updateTimelineHPageSize TimelineWindow{..} = do
  (winw,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef
  adjustmentSetPageSize timelineAdj (fromIntegral winw * scaleValue)

-------------------------------------------------------------------------------
-- Set the cursor to a new position

setCursor :: Bool -> TimelineWindow -> Double -> IO ()
setCursor debug timelineWin@TimelineWindow{..} x = do
  hadjValue <- adjustmentGetValue timelineAdj
  scaleValue <- readIORef scaleIORef
  let cursor = round (hadjValue + x * scaleValue)
  when debug $ printf "cursor set to: %d\n" cursor
  writeIORef cursorIORef cursor
  queueRedrawTimelines timelineWin

-------------------------------------------------------------------------------

setCursorToTime :: TimelineWindow -> Timestamp -> IO ()
setCursorToTime timelineWin@TimelineWindow{..} x
  = do writeIORef cursorIORef x
       pageSize <- adjustmentGetPageSize timelineAdj
       adjustmentSetValue timelineAdj ((fromIntegral x - pageSize/2) `max` 0)
       queueRedrawTimelines timelineWin

-------------------------------------------------------------------------------

timelineZoomIn :: TimelineWindow -> IO ()
timelineZoomIn timelineWin = zoomIn timelineWin

timelineZoomOut :: TimelineWindow -> IO ()
timelineZoomOut timelineWin = zoomOut timelineWin

timelineZoomToFit :: TimelineWindow -> IO ()
timelineZoomToFit timelineWin = zoomToFit timelineWin

timelineScrollToBeginning :: TimelineWindow -> IO ()
timelineScrollToBeginning timelineWin = scrollToBeginning timelineWin

timelineScrollToEnd :: TimelineWindow -> IO ()
timelineScrollToEnd timelineWin = scrollToEnd timelineWin

-- This one is especially evil since it relies on a shared cursor IORef
timelineCentreOnCursor :: TimelineWindow -> IO ()
timelineCentreOnCursor timelineWin = centreOnCursor timelineWin

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0
