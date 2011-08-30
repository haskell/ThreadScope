module GUI.Timeline (
    TimelineView,
    timelineViewNew,
    TimelineViewActions(..),

    timelineSetBWMode,
    timelineSetShowLabels,
    timelineGetViewParameters,
    timelineWindowSetHECs,
    timelineWindowSetTraces,
    timelineWindowSetBookmarks,
    timelineSetCursor,

    timelineZoomIn,
    timelineZoomOut,
    timelineZoomToFit,
    timelineScrollLeft,
    timelineScrollRight,
    timelineScrollToBeginning,
    timelineScrollToEnd,
    timelineCentreOnCursor,
 ) where

import GUI.Timeline.Types (TimelineState(..))
import GUI.Timeline.Motion
import GUI.Timeline.Render

import GUI.Types
import Events.HECs

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Old hiding (eventModifier)
import Graphics.UI.Gtk.Gdk.EventM as New
import Graphics.Rendering.Cairo  as C

import Data.IORef
import Control.Monad

-----------------------------------------------------------------------------
-- The CPUs view

data TimelineView = TimelineView {

       timelineState   :: TimelineState,

       hecsIORef       :: IORef (Maybe HECs),
       tracesIORef     :: IORef [Trace],
       bookmarkIORef   :: IORef [Timestamp],

       cursorIORef     :: IORef Timestamp,
       showLabelsIORef :: IORef Bool,
       bwmodeIORef     :: IORef Bool
     }

data TimelineViewActions = TimelineViewActions {
       timelineViewCursorChanged :: Timestamp -> IO ()
     }

-- | Draw some parts of the timeline in black and white rather than colour.
--
timelineSetBWMode :: TimelineView -> Bool -> IO ()
timelineSetBWMode timelineWin bwmode = do
  writeIORef (bwmodeIORef timelineWin) bwmode
  widgetQueueDraw (timelineDrawingArea (timelineState timelineWin))

timelineSetShowLabels :: TimelineView -> Bool -> IO ()
timelineSetShowLabels timelineWin showLabels = do
  writeIORef (showLabelsIORef timelineWin) showLabels
  widgetQueueDraw (timelineDrawingArea (timelineState timelineWin))

timelineGetViewParameters :: TimelineView -> IO ViewParameters
timelineGetViewParameters TimelineView{tracesIORef, bwmodeIORef, showLabelsIORef, timelineState=TimelineState{..}} = do

  (dAreaWidth,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef

  -- snap the view to whole pixels, to avoid blurring
  hadj_value0 <- adjustmentGetValue timelineAdj
  let hadj_value = toWholePixels scaleValue hadj_value0

  traces <- readIORef tracesIORef
  bwmode <- readIORef bwmodeIORef
  showLabels <- readIORef showLabelsIORef

  let timelineHeight = calculateTotalTimelineHeight showLabels traces

  return ViewParameters {
           width      = dAreaWidth,
           height     = timelineHeight,
           viewTraces = traces,
           hadjValue  = hadj_value,
           scaleValue = scaleValue,
           detail     = 3, --for now
           bwMode     = bwmode,
           labelsMode = showLabels
         }


timelineWindowSetHECs :: TimelineView -> Maybe HECs -> IO ()
timelineWindowSetHECs timelineWin@TimelineView{..} mhecs = do
  writeIORef hecsIORef mhecs
  writeIORef (scaleIORef timelineState) defaultScaleValue
  -- FIXME: this defaultScaleValue = -1 stuff is a terrible hack
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
timelineViewNew builder TimelineViewActions{..} = do

  let getWidget cast = builderGetObject builder cast
  timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
  timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
  timelineHScrollbar       <- getWidget castToHScrollbar "timeline_hscroll"
  timelineVScrollbar       <- getWidget castToVScrollbar "timeline_vscroll"
  timelineAdj              <- rangeGetAdjustment timelineHScrollbar
  timelineVAdj             <- rangeGetAdjustment timelineVScrollbar

  hecsIORef   <- newIORef Nothing
  tracesIORef <- newIORef []
  bookmarkIORef <- newIORef []
  scaleIORef  <- newIORef defaultScaleValue
  cursorIORef <- newIORef 0
  bwmodeIORef <- newIORef False
  showLabelsIORef <- newIORef False
  timelinePrevView <- newIORef Nothing

  let timelineState = TimelineState{..}
      timelineWin   = TimelineView{..}

  ------------------------------------------------------------------------
  -- Program the callback for the capability labelDrawingArea
  timelineLabelDrawingArea `onExpose` \_ -> do
    traces <- readIORef tracesIORef
    showLabels <- readIORef showLabelsIORef
    updateLabelDrawingArea timelineState showLabels traces
    return True

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  on timelineDrawingArea scrollEvent $ tryEvent $ do
    dir <- eventScrollDirection
    mods <- eventModifier
    liftIO $ do
    cursor <- readIORef cursorIORef
    case (dir,mods) of
      (ScrollUp,   [Control]) -> zoomIn  timelineState cursor
      (ScrollDown, [Control]) -> zoomOut timelineState cursor
      (ScrollUp,   [])        -> vscrollUp timelineState
      (ScrollDown, [])        -> vscrollDown timelineState
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
           timelineViewCursorChanged cursor

           return True
       _other -> do
           return False

  onValueChanged timelineAdj  $ queueRedrawTimelines timelineState
  onValueChanged timelineVAdj $ queueRedrawTimelines timelineState
  onAdjChanged   timelineAdj  $ queueRedrawTimelines timelineState
  onAdjChanged   timelineVAdj $ queueRedrawTimelines timelineState

  -- Program the callback for the capability drawingArea
  on timelineDrawingArea exposeEvent $ do
     exposeRegion <- New.eventRegion
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
           (_,dAreaHeight) <- widgetGetSize timelineDrawingArea
           let params' = params { height = max (height params) dAreaHeight }
           cursor    <- readIORef cursorIORef
           bookmarks <- readIORef bookmarkIORef

           renderView timelineState params' hecs cursor bookmarks exposeRegion

     return True

  on timelineDrawingArea configureEvent $ do
     liftIO $ configureTimelineDrawingArea timelineWin
     return True

  return timelineWin

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

queueRedrawTimelines :: TimelineState -> IO ()
queueRedrawTimelines TimelineState{..} = do
  widgetQueueDraw timelineDrawingArea
  widgetQueueDraw timelineLabelDrawingArea

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
updateTimelineVScroll TimelineView{tracesIORef, showLabelsIORef, timelineState=TimelineState{..}} = do
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
updateTimelineHPageSize :: TimelineState -> IO ()
updateTimelineHPageSize TimelineState{..} = do
  (winw,_) <- widgetGetSize timelineDrawingArea
  scaleValue <- readIORef scaleIORef
  adjustmentSetPageSize timelineAdj (fromIntegral winw * scaleValue)

-------------------------------------------------------------------------------
-- Set the cursor to a new position

timelineSetCursor :: TimelineView -> Timestamp -> IO ()
timelineSetCursor TimelineView{..} ts = do
  writeIORef cursorIORef ts
  queueRedrawTimelines timelineState

-------------------------------------------------------------------------------

timelineZoomIn :: TimelineView -> IO ()
timelineZoomIn TimelineView{..} = do
  cursor <- readIORef cursorIORef
  zoomIn timelineState cursor

timelineZoomOut :: TimelineView -> IO ()
timelineZoomOut TimelineView{..} = do
  cursor <- readIORef cursorIORef
  zoomOut timelineState cursor

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
  cursor <- readIORef cursorIORef
  centreOnCursor timelineState cursor

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0
