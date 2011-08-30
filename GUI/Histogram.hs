module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    HistogramViewActions(..),

    histogramSetBWMode,
    histogramSetShowLabels,
    histogramGetViewParameters,
    histogramWindowSetHECs,
    histogramWindowSetTraces,
    histogramWindowSetBookmarks,
    histogramSetCursor,

    histogramZoomIn,
    histogramZoomOut,
    histogramZoomToFit,
    histogramScrollLeft,
    histogramScrollRight,
    histogramScrollToBeginning,
    histogramScrollToEnd,
    histogramCentreOnCursor,
 ) where

import GUI.Timeline.Types (TimelineState(..))
import GUI.Timeline.Motion
import GUI.Timeline.Render

import GUI.Types
import Events.HECs

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Old hiding (eventModifier)
import Graphics.UI.Gtk.Gdk.EventM as New
import Graphics.Rendering.Cairo as C

import Data.IORef
import Control.Monad

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Renderable as ChartR
import qualified Graphics.Rendering.Chart.Gtk as ChartG
import Data.Accessor

-- | This function redraws the currently visible part of the
--   main trace canvas plus related canvases.
--
renderViewHistogram ::
  TimelineState
           -> ViewParameters
           -> HECs -> Timestamp -> [Timestamp]
           -> Region -> IO ()
renderViewHistogram
  TimelineState{timelineDrawingArea, timelineVAdj, timelinePrevView}
           params hecs cursor_t bookmarks exposeRegion = do

  -- Get state information from user-interface components
  (dAreaWidth, _) <- widgetGetSize timelineDrawingArea
  vadj_value <- adjustmentGetValue timelineVAdj

  prev_view <- readIORef timelinePrevView

  rect <- regionGetClipbox exposeRegion

  let plot xs =
              let layout = Chart.layout1_plots ^= [ Left (Chart.plotBars bars) ]
                           $ Chart.defaultLayout1 :: Chart.Layout1 Double Double

                  bars = Chart.plot_bars_values ^= barvs
                         $ Chart.defaultPlotBars

                  barvs = [(intDoub t, [intDoub height]) | (t, height) <- xs]

                  intDoub :: Integral a => a -> Double
                  intDoub = fromIntegral
              in layout
  ChartG.updateCanvas (ChartR.toRenderable (plot (durHistogram hecs))) timelineDrawingArea >> return ()


-----------------------------------------------------------------------------
-- The histogram view

data HistogramView = HistogramView {

       histogramState   :: TimelineState,

       hecsIORef       :: IORef (Maybe HECs),
       tracesIORef     :: IORef [Trace],
       bookmarkIORef   :: IORef [Timestamp],

       cursorIORef     :: IORef Timestamp,
       showLabelsIORef :: IORef Bool,
       bwmodeIORef     :: IORef Bool
     }

data HistogramViewActions = HistogramViewActions {
       histogramViewCursorChanged :: Timestamp -> IO ()
     }

-- | Draw some parts of the histogram in black and white rather than colour.
--
histogramSetBWMode :: HistogramView -> Bool -> IO ()
histogramSetBWMode histogramWin bwmode = do
  writeIORef (bwmodeIORef histogramWin) bwmode
  widgetQueueDraw (timelineDrawingArea (histogramState histogramWin))

histogramSetShowLabels :: HistogramView -> Bool -> IO ()
histogramSetShowLabels histogramWin showLabels = do
  writeIORef (showLabelsIORef histogramWin) showLabels
  widgetQueueDraw (timelineDrawingArea (histogramState histogramWin))

histogramGetViewParameters :: HistogramView -> IO ViewParameters
histogramGetViewParameters HistogramView{tracesIORef, bwmodeIORef, showLabelsIORef, histogramState=TimelineState{..}} = do

  (dAreaWidth,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef

  -- snap the view to whole pixels, to avoid blurring
  hadj_value0 <- adjustmentGetValue timelineAdj
  let hadj_value = toWholePixels scaleValue hadj_value0

  traces <- readIORef tracesIORef
  bwmode <- readIORef bwmodeIORef
  showLabels <- readIORef showLabelsIORef

  let histogramHeight = calculateTotalTimelineHeight showLabels traces

  return ViewParameters {
           width      = dAreaWidth,
           height     = histogramHeight,
           viewTraces = traces,
           hadjValue  = hadj_value,
           scaleValue = scaleValue,
           detail     = 3, --for now
           bwMode     = bwmode,
           labelsMode = showLabels
         }


histogramWindowSetHECs :: HistogramView -> Maybe HECs -> IO ()
histogramWindowSetHECs histogramWin@HistogramView{..} mhecs = do
  writeIORef hecsIORef mhecs
  writeIORef (scaleIORef histogramState) defaultScaleValue
  -- FIXME: this defaultScaleValue = -1 stuff is a terrible hack
  zoomToFit histogramState mhecs
  histogramParamsChanged histogramWin

histogramWindowSetTraces :: HistogramView -> [Trace] -> IO ()
histogramWindowSetTraces histogramWin@HistogramView{tracesIORef} traces = do
  writeIORef tracesIORef traces
  histogramParamsChanged histogramWin

histogramWindowSetBookmarks :: HistogramView -> [Timestamp] -> IO ()
histogramWindowSetBookmarks histogramWin@HistogramView{bookmarkIORef} bookmarks = do
  writeIORef bookmarkIORef bookmarks
  histogramParamsChanged histogramWin

-----------------------------------------------------------------------------

histogramViewNew :: Builder -> HistogramViewActions -> IO HistogramView
histogramViewNew builder HistogramViewActions{..} = do

  let getWidget cast = builderGetObject builder cast
  timelineDrawingArea      <- getWidget castToDrawingArea "histogram_drawingarea"
  timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
  histogramHScrollbar       <- getWidget castToHScrollbar "timeline_hscroll"
  histogramVScrollbar       <- getWidget castToVScrollbar "timeline_vscroll"
  timelineAdj              <- rangeGetAdjustment histogramHScrollbar
  timelineVAdj             <- rangeGetAdjustment histogramVScrollbar

  hecsIORef   <- newIORef Nothing
  tracesIORef <- newIORef []
  bookmarkIORef <- newIORef []
  scaleIORef  <- newIORef defaultScaleValue
  cursorIORef <- newIORef 0
  bwmodeIORef <- newIORef False
  showLabelsIORef <- newIORef False
  timelinePrevView <- newIORef Nothing

  let histogramState = TimelineState{..}
      histogramWin   = HistogramView{..}

  ------------------------------------------------------------------------
  -- Porgram the callback for the capability labelDrawingArea
  timelineLabelDrawingArea `onExpose` \_ -> do
{-    traces <- readIORef tracesIORef
    showLabels <- readIORef showLabelsIORef
    updateLabelDrawingArea histogramState showLabels traces -}
    return True

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  on timelineDrawingArea scrollEvent $ tryEvent $ do
    dir <- eventScrollDirection
    mods <- eventModifier
    liftIO $ do
    cursor <- readIORef cursorIORef
    case (dir,mods) of
      (ScrollUp,   [Control]) -> zoomIn  histogramState cursor
      (ScrollDown, [Control]) -> zoomOut histogramState cursor
      (ScrollUp,   [])        -> vscrollUp histogramState
      (ScrollDown, [])        -> vscrollDown histogramState
      _ -> return ()

  ------------------------------------------------------------------------
  -- Mouse button

  onButtonPress timelineDrawingArea $ \button -> do
     case button of
       Button{ Old.eventButton = LeftButton, Old.eventClick = SingleClick,
               -- eventModifier = [],  -- contains [Alt2] for me
               eventX = x } -> do
           hadjValue <- adjustmentGetValue timelineAdj
           scaleValue <- readIORef scaleIORef
           let cursor = round (hadjValue + x * scaleValue)
           widgetGrabFocus timelineDrawingArea
           histogramViewCursorChanged cursor

           return True
       _other -> do
           return False

  onValueChanged timelineAdj  $ queueRedrawHistograms histogramState
  onValueChanged timelineVAdj $ queueRedrawHistograms histogramState
  onAdjChanged   timelineAdj  $ queueRedrawHistograms histogramState
  onAdjChanged   timelineVAdj $ queueRedrawHistograms histogramState

  -- Porgram the callback for the capability drawingArea
  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- New.eventRegion
     liftIO $ do
       maybeEventArray <- readIORef hecsIORef

       -- Check to see if an event trace has been loaded
       case maybeEventArray of
         Nothing   -> return ()
         Just hecs -> do
           params <- histogramGetViewParameters histogramWin
           -- render either the whole height of the histogram, or the window, whichever
           -- is larger (this just ensure we fill the background if the histogram is
           -- smaller than the window).
           (_,dAreaHeight) <- widgetGetSize timelineDrawingArea
           let params' = params { height = max (height params) dAreaHeight }
           cursor    <- readIORef cursorIORef
           bookmarks <- readIORef bookmarkIORef

           renderViewHistogram histogramState params' hecs cursor bookmarks exposeRegion

     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureHistogramDrawingArea histogramWin
     return True

  return histogramWin

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

queueRedrawHistograms :: TimelineState -> IO ()
queueRedrawHistograms TimelineState{..} = do
  widgetQueueDraw timelineDrawingArea
--  widgetQueueDraw timelineLabelDrawingArea

--FIXME: we are still unclear about which state changes involve which updates
histogramParamsChanged :: HistogramView -> IO ()
histogramParamsChanged histogramWin@HistogramView{histogramState} = do
  queueRedrawHistograms histogramState
--  updateHistogramVScroll histogramWin

configureHistogramDrawingArea :: HistogramView -> IO ()
configureHistogramDrawingArea histogramWin@HistogramView{histogramState} = do
  updateHistogramVScroll histogramWin
  updateHistogramHPageSize histogramState

updateHistogramVScroll :: HistogramView -> IO ()
updateHistogramVScroll HistogramView{tracesIORef, showLabelsIORef, histogramState=TimelineState{..}} = do
  traces <- readIORef tracesIORef
  showLabels <- readIORef showLabelsIORef
  let h = calculateTotalTimelineHeight showLabels traces
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
updateHistogramHPageSize :: TimelineState -> IO ()
updateHistogramHPageSize TimelineState{..} = do
  (winw,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef
  adjustmentSetPageSize timelineAdj (fromIntegral winw * scaleValue)

-------------------------------------------------------------------------------
-- Set the cursor to a new position

histogramSetCursor :: HistogramView -> Timestamp -> IO ()
histogramSetCursor HistogramView{..} ts = do
  writeIORef cursorIORef ts
  queueRedrawHistograms histogramState

-------------------------------------------------------------------------------

histogramZoomIn :: HistogramView -> IO ()
histogramZoomIn HistogramView{..} = do
  cursor <- readIORef cursorIORef
  zoomIn histogramState cursor

histogramZoomOut :: HistogramView -> IO ()
histogramZoomOut HistogramView{..} = do
  cursor <- readIORef cursorIORef
  zoomOut histogramState cursor

histogramZoomToFit :: HistogramView -> IO ()
histogramZoomToFit HistogramView{..} = do
  mhecs <- readIORef hecsIORef
  zoomToFit histogramState mhecs

histogramScrollLeft :: HistogramView -> IO ()
histogramScrollLeft HistogramView{histogramState} = scrollLeft histogramState

histogramScrollRight :: HistogramView -> IO ()
histogramScrollRight HistogramView{histogramState} = scrollRight histogramState

histogramScrollToBeginning :: HistogramView -> IO ()
histogramScrollToBeginning HistogramView{histogramState} =
  scrollToBeginning histogramState

histogramScrollToEnd :: HistogramView -> IO ()
histogramScrollToEnd HistogramView{histogramState} =
  scrollToEnd histogramState

-- This one is especially evil since it relies on a shared cursor IORef
histogramCentreOnCursor :: HistogramView -> IO ()
histogramCentreOnCursor HistogramView{..} = do
  cursor <- readIORef cursorIORef
  centreOnCursor histogramState cursor

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0
