module GUI.Timeline (
    setupTimelineView,
    renderTraces,
    timelineParamsChanged,
    defaultScaleValue,
    queueRedrawTimelines,
    setCursorToTime
 ) where

import GUI.Timeline.Motion
import GUI.Timeline.Render
import GUI.Timeline.Key

import GUI.State
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

setupTimelineView :: ViewerState -> IO ()
setupTimelineView state@ViewerState{..} = do

  ------------------------------------------------------------------------
  -- Key presses
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
  timelineLabelDrawingArea `onExpose` updateLabelDrawingArea state

  ------------------------------------------------------------------------
  -- Set-up the key timelineDrawingArea.
  timelineKeyDrawingArea `onExpose` updateKeyDrawingArea timelineKeyDrawingArea

  ------------------------------------------------------------------------
  -- zoom buttons

  zoomInButton  `onToolButtonClicked` zoomIn    state
  zoomOutButton `onToolButtonClicked` zoomOut   state
  zoomFitButton `onToolButtonClicked` zoomToFit state

  firstButton  `onToolButtonClicked` scrollToBeginning state
  lastButton   `onToolButtonClicked` scrollToEnd state
  centreButton `onToolButtonClicked` centreOnCursor state

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
           setCursor state x
           return True
       _other -> do
           return False

  onValueChanged timelineAdj  $ queueRedrawTimelines state
  onValueChanged timelineVAdj $ queueRedrawTimelines state

  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- New.eventRegion
     liftIO $ exposeTraceView state exposeRegion
     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureTimelineDrawingArea state
     return True

  return ()

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

timelineParamsChanged :: ViewerState -> IO ()
timelineParamsChanged state = do
  queueRedrawTimelines state
  updateTimelineVScroll state

configureTimelineDrawingArea :: ViewerState -> IO ()
configureTimelineDrawingArea state = do
  updateTimelineVScroll state
  updateTimelineHPageSize state

updateTimelineVScroll :: ViewerState -> IO ()
updateTimelineVScroll state@ViewerState{..} = do
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
updateTimelineHPageSize :: ViewerState -> IO ()
updateTimelineHPageSize state@ViewerState{..} = do
  (winw,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef
  adjustmentSetPageSize timelineAdj (fromIntegral winw * scaleValue)

-------------------------------------------------------------------------------
-- Set the cursor to a new position

setCursor :: ViewerState -> Double -> IO ()
setCursor state@ViewerState{..} x = do
  hadjValue <- adjustmentGetValue timelineAdj
  scaleValue <- readIORef scaleIORef
  let cursor = round (hadjValue + x * scaleValue)
  when debug $ printf "cursor set to: %d\n" cursor
  writeIORef cursorIORef cursor
  queueRedrawTimelines state

-------------------------------------------------------------------------------

setCursorToTime :: ViewerState -> Timestamp -> IO ()
setCursorToTime state@ViewerState{..} x
  = do hadjValue <- adjustmentGetValue timelineAdj
       scaleValue <- readIORef scaleIORef
       -- let cursor = round (hadjValue + x * scaleValue)
       -- when debug $ printf "cursor set to: %d\n" cursor
       writeIORef cursorIORef x
       pageSize <- adjustmentGetPageSize timelineAdj
       adjustmentSetValue timelineAdj ((fromIntegral x - pageSize/2) `max` 0)
       queueRedrawTimelines state

-------------------------------------------------------------------------------


-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0
