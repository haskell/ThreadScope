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

import GUI.Timeline.Motion
import GUI.Timeline.Render
import GUI.Timeline.Key

import GUI.State (ViewerState)
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

data TimelineWindow = TimelineWindow {
       timelineDrawingArea      :: DrawingArea,
       timelineLabelDrawingArea :: DrawingArea,
       timelineKeyDrawingArea   :: DrawingArea,
       timelineAdj              :: Adjustment,
       timelineVAdj             :: Adjustment,

       bwmodeIORef :: IORef Bool,

       --TODO: eliminate, these are **shared** not private IORefs !!
       -- Should instead have methods for updating the display state
       -- and events for when the cursor is changed. Let the interaction
       -- module hold the state.
       scaleIORef        :: IORef Double, -- in ns/pixel
       cursorIORef       :: IORef Timestamp
     }

-----------------------------------------------------------------------------

-- | Draw some parts of the timeline in black and white rather than colour.
--
timelineSetBWMode :: TimelineWindow -> Bool -> IO ()
timelineSetBWMode timelineWin bwmode = do
  writeIORef (bwmodeIORef timelineWin) bwmode
  widgetQueueDraw (timelineDrawingArea timelineWin)

-----------------------------------------------------------------------------

timelineWindowNew :: Bool -> Builder
                  -> ViewerState -> IORef Double -> IORef Timestamp --TODO: eliminate
                  -> IO TimelineWindow
timelineWindowNew debug builder state scaleIORef cursorIORef = do

  let getWidget cast = builderGetObject builder cast
  timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
  timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
  timelineKeyDrawingArea   <- getWidget castToDrawingArea "timeline_key_drawingarea"
  timelineHScrollbar       <- getWidget castToHScrollbar "timeline_hscroll"
  timelineVScrollbar       <- getWidget castToVScrollbar "timeline_vscroll"
  timelineAdj              <- rangeGetAdjustment timelineHScrollbar
  timelineVAdj             <- rangeGetAdjustment timelineVScrollbar

  bwmodeIORef <- newIORef False

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
      "Right"  -> do scrollRight state; return True
      "Left"   -> do scrollLeft  state; return True
      _ -> if isJust mch then
             case fromJust mch of
               '+' -> do zoomIn  state; return True
               '-' -> do zoomOut state; return True
               _   -> return True
           else
             return True

  ------------------------------------------------------------------------
  -- Porgram the callback for the capability drawingArea
  timelineLabelDrawingArea `onExpose` \_ -> do
    updateLabelDrawingArea state
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
      (ScrollUp,   [Control]) -> zoomIn state
      (ScrollDown, [Control]) -> zoomOut state
      (ScrollUp,   [])        -> vscrollUp state
      (ScrollDown, [])        -> vscrollDown state
      _ -> return ()

  ------------------------------------------------------------------------
  -- Mouse button

  onButtonPress timelineDrawingArea $ \button -> do
     when debug $ putStrLn ("button pressed: " ++ show button)
     case button of
       Button{ Old.eventButton = LeftButton, Old.eventClick = SingleClick,
               -- eventModifier = [],  -- contains [Alt2] for me
               eventX = x } -> do
           setCursor state debug timelineWin x
           return True
       _other -> do
           return False

  onValueChanged timelineAdj  $ queueRedrawTimelines state
  onValueChanged timelineVAdj $ queueRedrawTimelines state

  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- New.eventRegion
     liftIO $ do
       bwmode <- readIORef bwmodeIORef
       exposeTraceView state bwmode exposeRegion
     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureTimelineDrawingArea state timelineWin
     return True

  return timelineWin

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

timelineParamsChanged :: ViewerState -> TimelineWindow -> IO ()
timelineParamsChanged state timelineWin = do
  queueRedrawTimelines state
  updateTimelineVScroll state timelineWin

configureTimelineDrawingArea :: ViewerState -> TimelineWindow -> IO ()
configureTimelineDrawingArea state timelineWin = do
  updateTimelineVScroll state timelineWin
  updateTimelineHPageSize timelineWin

updateTimelineVScroll :: ViewerState -> TimelineWindow -> IO ()
updateTimelineVScroll state TimelineWindow{..} = do
  h <- calculateTotalTimelineHeight state
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

setCursor :: ViewerState -> Bool -> TimelineWindow -> Double -> IO ()
setCursor state debug TimelineWindow{..} x = do
  hadjValue <- adjustmentGetValue timelineAdj
  scaleValue <- readIORef scaleIORef
  let cursor = round (hadjValue + x * scaleValue)
  when debug $ printf "cursor set to: %d\n" cursor
  writeIORef cursorIORef cursor
  queueRedrawTimelines state

-------------------------------------------------------------------------------

setCursorToTime :: ViewerState -> TimelineWindow -> Timestamp -> IO ()
setCursorToTime state TimelineWindow{..} x
  = do writeIORef cursorIORef x
       pageSize <- adjustmentGetPageSize timelineAdj
       adjustmentSetValue timelineAdj ((fromIntegral x - pageSize/2) `max` 0)
       queueRedrawTimelines state

-------------------------------------------------------------------------------

--TODO: change all these ViewerState to TimelineWindow

timelineZoomIn :: ViewerState -> IO ()
timelineZoomIn state = zoomIn state

timelineZoomOut :: ViewerState -> IO ()
timelineZoomOut state = zoomOut state

timelineZoomToFit :: ViewerState -> IO ()
timelineZoomToFit state = zoomToFit state

timelineScrollToBeginning :: ViewerState -> IO ()
timelineScrollToBeginning state = scrollToBeginning state

timelineScrollToEnd :: ViewerState -> IO ()
timelineScrollToEnd state = scrollToEnd state

-- This one is especially evil since it relies on a shared cursor IORef
timelineCentreOnCursor :: ViewerState -> IO ()
timelineCentreOnCursor state = centreOnCursor state

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0
