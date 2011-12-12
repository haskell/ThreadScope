{-# LANGUAGE CPP #-}
module GUI.Timeline (
    TimelineView,
    timelineViewNew,
    TimelineViewActions(..),

    timelineSetBWMode,
    timelineSetLabelsMode,
    timelineGetViewParameters,
    timelineGetYScaleArea,
    timelineWindowSetHECs,
    timelineWindowSetTraces,
    timelineWindowSetBookmarks,
    timelineSetSelection,
    TimeSelection(..),

    timelineZoomIn,
    timelineZoomOut,
    timelineZoomToFit,
    timelineScrollLeft,
    timelineScrollRight,
    timelineScrollToBeginning,
    timelineScrollToEnd,
    timelineCentreOnCursor,
 ) where

import GUI.Types
import GUI.Timeline.Types

import GUI.Timeline.Motion
import GUI.Timeline.Render
import GUI.Timeline.Render.Constants

import Events.HECs

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef
import Control.Monad

-----------------------------------------------------------------------------
-- The CPUs view

data TimelineView = TimelineView {

       timelineState   :: TimelineState,

       hecsIORef       :: IORef (Maybe HECs),
       tracesIORef     :: IORef [Trace],
       bookmarkIORef   :: IORef [Timestamp],

       selectionRef    :: IORef TimeSelection,
       labelsModeIORef :: IORef Bool,
       bwmodeIORef     :: IORef Bool,

       cursorIBeam     :: Cursor,
       cursorMove      :: Cursor
     }

data TimelineViewActions = TimelineViewActions {
       timelineViewSelectionChanged :: TimeSelection -> IO ()
     }

-- | Draw some parts of the timeline in black and white rather than colour.
timelineSetBWMode :: TimelineView -> Bool -> IO ()
timelineSetBWMode timelineWin bwmode = do
  writeIORef (bwmodeIORef timelineWin) bwmode
  widgetQueueDraw (timelineDrawingArea (timelineState timelineWin))

timelineSetLabelsMode :: TimelineView -> Bool -> IO ()
timelineSetLabelsMode timelineWin labelsMode = do
  writeIORef (labelsModeIORef timelineWin) labelsMode
  widgetQueueDraw (timelineDrawingArea (timelineState timelineWin))

timelineGetViewParameters :: TimelineView -> IO ViewParameters
timelineGetViewParameters TimelineView{tracesIORef, bwmodeIORef, labelsModeIORef,
                                       timelineState=TimelineState{..}} = do

  (w, _) <- widgetGetSize timelineDrawingArea
  scaleValue  <- readIORef scaleIORef
  maxSpkValue <- readIORef maxSpkIORef

  -- snap the view to whole pixels, to avoid blurring
  hadj_value0 <- adjustmentGetValue timelineAdj
  let hadj_value = toWholePixels scaleValue hadj_value0

  traces <- readIORef tracesIORef
  bwmode <- readIORef bwmodeIORef
  labelsMode <- readIORef labelsModeIORef

  (_, xScaleAreaHeight) <- widgetGetSize timelineXScaleArea
  let histTotalHeight = stdHistogramHeight + histXScaleHeight
      timelineHeight =
        calculateTotalTimelineHeight labelsMode histTotalHeight traces

  return ViewParameters
           { width      = w
           , height     = timelineHeight
           , viewTraces = traces
           , hadjValue  = hadj_value
           , scaleValue = scaleValue
           , maxSpkValue = maxSpkValue
           , detail     = 3 --for now
           , bwMode     = bwmode
           , labelsMode = labelsMode
           , histogramHeight = stdHistogramHeight
           , minterval = Nothing
           , xScaleAreaHeight = xScaleAreaHeight
           }

timelineGetYScaleArea :: TimelineView -> DrawingArea
timelineGetYScaleArea timelineWin =
  timelineYScaleArea $ timelineState timelineWin

timelineWindowSetHECs :: TimelineView -> Maybe HECs -> IO ()
timelineWindowSetHECs timelineWin@TimelineView{..} mhecs = do
  writeIORef hecsIORef mhecs
  zoomToFit timelineState mhecs
  timelineParamsChanged timelineWin

timelineWindowSetTraces :: TimelineView -> [Trace] -> IO ()
timelineWindowSetTraces timelineWin@TimelineView{tracesIORef} traces = do
  writeIORef tracesIORef traces
  timelineParamsChanged timelineWin

timelineWindowSetBookmarks :: TimelineView -> [Timestamp] -> IO ()
timelineWindowSetBookmarks timelineWin@TimelineView{bookmarkIORef} bookmarks = do
  writeIORef bookmarkIORef bookmarks
  timelineParamsChanged timelineWin

-----------------------------------------------------------------------------

timelineViewNew :: Builder -> TimelineViewActions -> IO TimelineView
timelineViewNew builder actions@TimelineViewActions{..} = do

  let getWidget cast = builderGetObject builder cast
  timelineViewport    <- getWidget castToWidget "timeline_viewport"
  timelineDrawingArea <- getWidget castToDrawingArea "timeline_drawingarea"
  timelineYScaleArea  <- getWidget castToDrawingArea "timeline_yscale_area"
  timelineXScaleArea  <- getWidget castToDrawingArea "timeline_xscale_area"
  timelineHScrollbar  <- getWidget castToHScrollbar "timeline_hscroll"
  timelineVScrollbar  <- getWidget castToVScrollbar "timeline_vscroll"
  timelineAdj         <- rangeGetAdjustment timelineHScrollbar
  timelineVAdj        <- rangeGetAdjustment timelineVScrollbar

  -- HACK: layoutSetAttributes does not work for \mu, so let's work around
  fd <- fontDescriptionNew
  fontDescriptionSetSize fd 8
  fontDescriptionSetFamily fd "sans serif"
  widgetModifyFont timelineYScaleArea (Just fd)

  cursorIBeam <- cursorNew Xterm
  cursorMove  <- cursorNew Fleur

  hecsIORef   <- newIORef Nothing
  tracesIORef <- newIORef []
  bookmarkIORef <- newIORef []
  scaleIORef  <- newIORef 0
  maxSpkIORef <- newIORef 0
  selectionRef <- newIORef (PointSelection 0)
  bwmodeIORef <- newIORef False
  labelsModeIORef <- newIORef False
  timelinePrevView <- newIORef Nothing

  let timelineState = TimelineState{..}
      timelineWin   = TimelineView{..}

  ------------------------------------------------------------------------
  -- Redrawing labelDrawingArea
  timelineYScaleArea `onExpose` \_ -> do
    maybeEventArray <- readIORef hecsIORef

    -- Check to see if an event trace has been loaded
    case maybeEventArray of
      Nothing   -> return False
      Just hecs -> do
        traces <- readIORef tracesIORef
        labelsMode <- readIORef labelsModeIORef
        let maxP = maxSparkPool hecs
            maxH = fromIntegral (maxYHistogram hecs)
        updateYScaleArea timelineState maxP maxH Nothing labelsMode traces
        return True

  ------------------------------------------------------------------------
  -- Redrawing XScaleArea
  timelineXScaleArea `onExpose` \_ -> do
    maybeEventArray <- readIORef hecsIORef

    -- Check to see if an event trace has been loaded
    case maybeEventArray of
      Nothing   -> return False
      Just hecs -> do
        let lastTx = hecLastEventTime hecs
        updateXScaleArea timelineState lastTx
        return True

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  on timelineViewport scrollEvent $ tryEvent $ do
    dir <- eventScrollDirection
    mods <- eventModifier
    (x, _y) <- eventCoordinates
    x_ts    <- liftIO $ viewPointToTime timelineWin x
    liftIO $ case (dir,mods) of
      (ScrollUp,   [Control]) -> zoomIn  timelineState x_ts
      (ScrollDown, [Control]) -> zoomOut timelineState x_ts
      (ScrollUp,   [])        -> vscrollUp timelineState
      (ScrollDown, [])        -> vscrollDown timelineState
      _ -> return ()

  ------------------------------------------------------------------------
  -- Mouse button and selection

  widgetSetCursor timelineDrawingArea (Just cursorIBeam)

  mouseStateVar <- newIORef None

  let withMouseState action = liftIO $ do
      st  <- readIORef mouseStateVar
      st' <- action st
      writeIORef mouseStateVar st'

  on timelineDrawingArea buttonPressEvent $ do
    (x,_y) <- eventCoordinates
    button <- eventButton
    liftIO $ widgetGrabFocus timelineViewport
    withMouseState (\st -> mousePress timelineWin actions st button x)
    return False

  on timelineDrawingArea buttonReleaseEvent $ do
    (x,_y) <- eventCoordinates
    button <- eventButton
    withMouseState (\st -> mouseRelease timelineWin actions st button x)
    return False

  widgetAddEvents timelineDrawingArea [Button1MotionMask, Button2MotionMask]
  on timelineDrawingArea motionNotifyEvent $ do
    (x, _y) <- eventCoordinates
    withMouseState (\st -> mouseMove timelineWin st x)
    return False

  on timelineDrawingArea grabBrokenEvent $ do
    withMouseState (mouseMoveCancel timelineWin actions)
    return False

  -- Escape key to cancel selection or drag
  on timelineViewport keyPressEvent $ do
    let liftNoMouse a =
          let whenNoMouse None = a >> return None
              whenNoMouse st   = return st
          in withMouseState whenNoMouse >> return True
    keyName <- eventKeyName
    keyVal <- eventKeyVal
    case (keyName, keyToChar keyVal, keyVal) of
      ("Right", _, _)   -> liftNoMouse $ scrollRight timelineState
      ("Left",  _, _)   -> liftNoMouse $ scrollLeft  timelineState
      (_ , Just '+', _) -> liftNoMouse $ timelineZoomIn  timelineWin
      (_ , Just '-', _) -> liftNoMouse $ timelineZoomOut timelineWin
      (_, _, 0xff1b)    -> withMouseState (mouseMoveCancel timelineWin actions)
                           >> return True
      _                 -> return False

  ------------------------------------------------------------------------
  -- Scroll bars

  onValueChanged timelineAdj  $ queueRedrawTimelines timelineState
  onValueChanged timelineVAdj $ queueRedrawTimelines timelineState
  onAdjChanged   timelineAdj  $ queueRedrawTimelines timelineState
  onAdjChanged   timelineVAdj $ queueRedrawTimelines timelineState

  ------------------------------------------------------------------------
  -- Redrawing

  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- eventRegion
     liftIO $ do
       maybeEventArray <- readIORef hecsIORef

       -- Check to see if an event trace has been loaded
       case maybeEventArray of
         Nothing   -> return ()
         Just hecs -> do
           params <- timelineGetViewParameters timelineWin
           -- render either the whole height of the timeline, or the window, whichever
           -- is larger (this just ensure we fill the background if the timeline is
           -- smaller than the window).
           (_, h) <- widgetGetSize timelineDrawingArea
           let params' = params { height = max (height params) h }
           selection  <- readIORef selectionRef
           bookmarks <- readIORef bookmarkIORef

           renderView timelineState params' hecs selection bookmarks exposeRegion

     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureTimelineDrawingArea timelineWin
     return True

  return timelineWin

-------------------------------------------------------------------------------

viewPointToTime :: TimelineView -> Double -> IO Timestamp
viewPointToTime TimelineView{timelineState=TimelineState{..}} x = do
    hadjValue  <- adjustmentGetValue timelineAdj
    scaleValue <- readIORef scaleIORef
    let ts = round (max 0 (hadjValue + x * scaleValue))
    return $! ts

viewPointToTimeNoClamp :: TimelineView -> Double -> IO Double
viewPointToTimeNoClamp TimelineView{timelineState=TimelineState{..}} x = do
    hadjValue  <- adjustmentGetValue timelineAdj
    scaleValue <- readIORef scaleIORef
    let ts = hadjValue + x * scaleValue
    return $! ts

viewRangeToTimeRange :: TimelineView
                     -> (Double, Double) -> IO (Timestamp, Timestamp)
viewRangeToTimeRange view (x, x') = do
    let xMin = min x x'
        xMax = max x x'
    xv  <- viewPointToTime view xMin
    xv' <- viewPointToTime view xMax
    return (xv, xv')

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

queueRedrawTimelines :: TimelineState -> IO ()
queueRedrawTimelines TimelineState{..} = do
  widgetQueueDraw timelineDrawingArea
  widgetQueueDraw timelineYScaleArea
  widgetQueueDraw timelineXScaleArea

--FIXME: we are still unclear about which state changes involve which updates
timelineParamsChanged :: TimelineView -> IO ()
timelineParamsChanged timelineWin@TimelineView{timelineState} = do
  queueRedrawTimelines timelineState
  updateTimelineVScroll timelineWin

configureTimelineDrawingArea :: TimelineView -> IO ()
configureTimelineDrawingArea timelineWin@TimelineView{timelineState} = do
  updateTimelineVScroll timelineWin
  updateTimelineHPageSize timelineState

updateTimelineVScroll :: TimelineView -> IO ()
updateTimelineVScroll TimelineView{tracesIORef, labelsModeIORef, timelineState=TimelineState{..}} = do
  traces <- readIORef tracesIORef
  labelsMode <- readIORef labelsModeIORef
  let histTotalHeight = stdHistogramHeight + histXScaleHeight
      h = calculateTotalTimelineHeight labelsMode histTotalHeight traces
  (_,winh) <- widgetGetSize timelineDrawingArea
  let winh' = fromIntegral winh;
      h' = fromIntegral h
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
updateTimelineHPageSize :: TimelineState -> IO ()
updateTimelineHPageSize TimelineState{..} = do
  (winw,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef
  adjustmentSetPageSize timelineAdj (fromIntegral winw * scaleValue)

-------------------------------------------------------------------------------
-- Cursor / selection and mouse interaction

timelineSetSelection :: TimelineView -> TimeSelection -> IO ()
timelineSetSelection TimelineView{..} selection = do
  writeIORef selectionRef selection
  queueRedrawTimelines timelineState

-- little state machine
data MouseState = None
                | PressLeft  !Double   -- left mouse button is currently pressed
                                       -- but not over threshold for dragging
                | DragLeft   !Double   -- dragging with left mouse button
                | DragMiddle !Double !Double  -- dragging with middle mouse button

mousePress :: TimelineView -> TimelineViewActions
           -> MouseState -> MouseButton -> Double -> IO MouseState
mousePress view@TimelineView{..} TimelineViewActions{..} state button x =
  case (state, button) of
    (None, LeftButton)   -> do xv <- viewPointToTime view x
                               -- update the view without notifying the client
                               timelineSetSelection view (PointSelection xv)
                               return (PressLeft x)
    (None, MiddleButton) -> do widgetSetCursor timelineDrawingArea (Just cursorMove)
                               v <- adjustmentGetValue timelineAdj
                               return (DragMiddle x v)
    _                    -> return state
  where
    TimelineState{timelineAdj, timelineDrawingArea} = timelineState


mouseMove :: TimelineView -> MouseState
          -> Double -> IO MouseState
mouseMove view@TimelineView{..} state x =
  case state of
    None              -> return None
    PressLeft x0
      | dragThreshold -> mouseMove view (DragLeft x0) x
      | otherwise     -> return (PressLeft x0)
      where
        dragThreshold = abs (x - x0) > 5
    DragLeft  x0      -> do (xv, xv') <- viewRangeToTimeRange view (x0, x)
                            -- update the view without notifying the client
                            timelineSetSelection view (RangeSelection xv xv')
                            return (DragLeft x0)
    DragMiddle x0 v   -> do xv  <- viewPointToTimeNoClamp view x
                            xv' <- viewPointToTimeNoClamp view x0
                            scrollTo timelineState (v + (xv' - xv))
                            return (DragMiddle x0 v)


mouseMoveCancel :: TimelineView -> TimelineViewActions
                -> MouseState -> IO MouseState
mouseMoveCancel view@TimelineView{..} TimelineViewActions{..} state =
  case state of
    PressLeft x0   -> do xv <- viewPointToTime view x0
                         timelineViewSelectionChanged (PointSelection xv)
                         return None
    DragLeft  x0   -> do xv <- viewPointToTime view x0
                         timelineViewSelectionChanged (PointSelection xv)
                         return None
    DragMiddle _ _ -> do widgetSetCursor timelineDrawingArea (Just cursorIBeam)
                         return None
    None           -> return None
  where
    TimelineState{timelineDrawingArea} = timelineState

mouseRelease :: TimelineView -> TimelineViewActions
             -> MouseState -> MouseButton -> Double -> IO MouseState
mouseRelease view@TimelineView{..} TimelineViewActions{..} state button x =
  case (state, button) of
    (PressLeft x0,  LeftButton)  -> do xv <- viewPointToTime view x0
                                       timelineViewSelectionChanged (PointSelection xv)
                                       return None
    (DragLeft x0,   LeftButton)  -> do (xv, xv') <- viewRangeToTimeRange view (x0, x)
                                       timelineViewSelectionChanged (RangeSelection xv xv')
                                       return None
    (DragMiddle{}, MiddleButton) -> do widgetSetCursor timelineDrawingArea (Just cursorIBeam)
                                       return None
    _                            -> return state
  where
    TimelineState{timelineDrawingArea} = timelineState


widgetSetCursor :: WidgetClass widget => widget -> Maybe Cursor -> IO ()
widgetSetCursor widget cursor = do
#if MIN_VERSION_gtk(0,12,1)
    dw <- widgetGetDrawWindow widget
    drawWindowSetCursor dw cursor
#endif
    return ()

-------------------------------------------------------------------------------

timelineZoomIn :: TimelineView -> IO ()
timelineZoomIn TimelineView{..} = do
  selection <- readIORef selectionRef
  zoomIn timelineState (selectionPoint selection)

timelineZoomOut :: TimelineView -> IO ()
timelineZoomOut TimelineView{..} = do
  selection <- readIORef selectionRef
  zoomOut timelineState (selectionPoint selection)

timelineZoomToFit :: TimelineView -> IO ()
timelineZoomToFit TimelineView{..} = do
  mhecs <- readIORef hecsIORef
  zoomToFit timelineState mhecs

timelineScrollLeft :: TimelineView -> IO ()
timelineScrollLeft TimelineView{timelineState} = scrollLeft timelineState

timelineScrollRight :: TimelineView -> IO ()
timelineScrollRight TimelineView{timelineState} = scrollRight timelineState

timelineScrollToBeginning :: TimelineView -> IO ()
timelineScrollToBeginning TimelineView{timelineState} =
  scrollToBeginning timelineState

timelineScrollToEnd :: TimelineView -> IO ()
timelineScrollToEnd TimelineView{timelineState} =
  scrollToEnd timelineState

-- This one is especially evil since it relies on a shared cursor IORef
timelineCentreOnCursor :: TimelineView -> IO ()
timelineCentreOnCursor TimelineView{..} = do
  selection <- readIORef selectionRef
  centreOnCursor timelineState (selectionPoint selection)

selectionPoint :: TimeSelection -> Timestamp
selectionPoint (PointSelection x)    = x
selectionPoint (RangeSelection x x') = midpoint x x'
  where
    midpoint a b = a + (b - a) `div` 2
