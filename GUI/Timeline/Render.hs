module GUI.Timeline.Render (
    renderView,
    renderTraces,
    updateXScaleArea,
    renderYScaleArea,
    updateYScaleArea,
    calculateTotalTimelineHeight,
    toWholePixels,
  ) where

import GUI.Timeline.Types
import GUI.Timeline.Render.Constants
import GUI.Timeline.Ticks
import GUI.Timeline.HEC
import GUI.Timeline.Sparks
import GUI.Timeline.Activity

import Events.HECs
import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.CairoDrawing

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef
import Control.Monad

-------------------------------------------------------------------------------

-- | This function redraws the currently visible part of the
--   main trace canvas plus related canvases.
--
renderView :: TimelineState
           -> ViewParameters
           -> HECs -> TimeSelection -> [Timestamp]
           -> Region -> IO ()
renderView TimelineState{timelineDrawingArea, timelineVAdj, timelinePrevView}
           params hecs selection bookmarks exposeRegion = do

  -- Get state information from user-interface components
  (w, _) <- widgetGetSize timelineDrawingArea
  vadj_value <- adjustmentGetValue timelineVAdj

  prev_view <- readIORef timelinePrevView

  rect <- regionGetClipbox exposeRegion

  win <- widgetGetDrawWindow timelineDrawingArea
  renderWithDrawable win $ do

  let renderToNewSurface = do
        new_surface <- withTargetSurface $ \surface ->
          liftIO $ createSimilarSurface surface ContentColor w (height params)
        renderWith new_surface $ do
          clearWhite
          renderTraces params hecs rect
        return new_surface

  surface <-
    case prev_view of
      Nothing -> renderToNewSurface

      Just (old_params, surface)
        | old_params == params
        -> return surface

        | width  old_params == width  params &&
          height old_params == height params
        -> do
             if old_params { hadjValue = hadjValue params } == params
                -- only the hadjValue changed
                && abs (hadjValue params - hadjValue old_params) <
                   fromIntegral (width params) * scaleValue params
                -- and the views overlap...
               then
                 scrollView surface old_params params hecs
               else do
                 renderWith surface $ do
                   clearWhite; renderTraces params hecs rect
                 return surface

        | otherwise
        -> do surfaceFinish surface
              renderToNewSurface

  liftIO $ writeIORef timelinePrevView (Just (params, surface))

  region exposeRegion
  clip
  setSourceSurface surface 0 (-vadj_value)
          -- ^^ this is where we adjust for the vertical scrollbar
  setOperator OperatorSource
  paint
  renderBookmarks bookmarks params
  drawSelection params selection

-------------------------------------------------------------------------------

-- Render the bookmarks
renderBookmarks :: [Timestamp] -> ViewParameters -> Render ()
renderBookmarks bookmarks vp@ViewParameters{height} = do
  setLineWidth 1
  setSourceRGBAhex bookmarkColour 1.0
  sequence_
    [ do moveTo x 0
         lineTo x (fromIntegral height)
         stroke
    | bookmark <- bookmarks
    , let x = timestampToView vp bookmark ]

-------------------------------------------------------------------------------

drawSelection :: ViewParameters -> TimeSelection -> Render ()
drawSelection vp@ViewParameters{height} (PointSelection x) = do
  setLineWidth 3
  setOperator OperatorOver
  setSourceRGBAhex blue 1.0
  moveTo xv 0
  lineTo xv (fromIntegral height)
  stroke
 where
  xv = timestampToView vp x

drawSelection vp@ViewParameters{height} (RangeSelection x x') = do
  setLineWidth 1.5
  setOperator OperatorOver

  setSourceRGBAhex blue 0.25
  rectangle xv 0 (xv' - xv) (fromIntegral height)
  fill

  setSourceRGBAhex blue 1.0
  moveTo xv 0
  lineTo xv (fromIntegral height)
  moveTo xv' 0
  lineTo xv' (fromIntegral height)
  stroke
 where
  xv  = timestampToView vp x
  xv' = timestampToView vp x'

-------------------------------------------------------------------------------

-- We currently have two different way of converting from logical units
-- (ie timestamps in micro-seconds) to device units (ie pixels):
--   * the first is to set the cairo context to the appropriate scale
--   * the second is to do the conversion ourself
--
-- While in principle the first is superior due to the simplicity: cairo
-- lets us use Double as the logical unit and scaling factor. In practice
-- however cairo does not support the full Double range because internally
-- it makes use of a 32bit fixed point float format. With very large scaling
-- factors we end up with artifacts like lines disappearing.
--
-- So sadly we will probably have to convert to using the second method.

-- | Use cairo to convert from logical units (timestamps) to device units
--
withViewScale :: ViewParameters -> Render () -> Render ()
withViewScale ViewParameters{scaleValue, hadjValue} inner = do
  save
  scale (1/scaleValue) 1.0
  translate (-hadjValue) 0
  inner
  restore

-- | Manually convert from logical units (timestamps) to device units.
--
timestampToView :: ViewParameters -> Timestamp -> Double
timestampToView ViewParameters{scaleValue, hadjValue} ts =
  (fromIntegral ts - hadjValue) / scaleValue

-------------------------------------------------------------------------------
-- This function draws the current view of all the HECs with Cairo.

renderTraces :: ViewParameters -> HECs -> Rectangle
             -> Render ()
renderTraces params@ViewParameters{..} hecs (Rectangle rx _ry rw _rh) = do
  let scale_rx    = fromIntegral rx * scaleValue
      scale_rw    = fromIntegral rw * scaleValue
      scale_width = fromIntegral width * scaleValue

      startPos :: Timestamp
      startPos = fromIntegral $ truncate (scale_rx + hadjValue)

      endPos :: Timestamp
      endPos = minimum [
                 ceiling (hadjValue + scale_width),
                 ceiling (hadjValue + scale_rx + scale_rw),
                 hecLastEventTime hecs
              ]

      -- For spark traces, round the start time down, and the end time up,
      -- to a slice boundary:
      start = (startPos `div` slice) * slice
      end = ((endPos + slice) `div` slice) * slice
      (slice, prof) = treesProfile scaleValue start end hecs

  withViewScale params $ do
    -- Render the vertical rulers across all the traces.
    renderVRulers scaleValue startPos endPos height XScaleTime

    -- This function helps to render a single HEC.
    -- Traces are rendered even if the y-region falls outside visible area.
    -- OTOH, trace rendering function tend to drawn only the visible
    -- x-region of the graph.
    let renderTrace trace y = do
          save
          translate 0 (fromIntegral y)
          case trace of
             TraceHEC c ->
               let (dtree, etree, _) = hecTrees hecs !! c
               in renderHEC params startPos endPos
                    (perfNames hecs) (dtree, etree)
             TraceInstantHEC c ->
               let (_, etree, _) = hecTrees hecs !! c
               in renderInstantHEC params startPos endPos
                    (perfNames hecs) etree
             TraceCreationHEC c ->
               renderSparkCreation params slice start end (prof !! c)
             TraceConversionHEC c ->
               renderSparkConversion params slice start end (prof !! c)
             TracePoolHEC c ->
               let maxP = maxSparkPool hecs
               in renderSparkPool params slice start end (prof !! c) maxP
             TraceHistogram ->
               renderSparkHistogram params hecs
             TraceGroup _ -> error "renderTrace"
             TraceActivity ->
               renderActivity params hecs startPos endPos
          restore
        histTotalHeight = histogramHeight + histXScaleHeight
    -- Now render all the HECs.
    zipWithM_ renderTrace viewTraces
      (traceYPositions labelsMode histTotalHeight viewTraces)

-------------------------------------------------------------------------------

-- parameters differ only in the hadjValue, we can scroll ...
scrollView :: Surface
           -> ViewParameters -> ViewParameters
           -> HECs
           -> Render Surface
scrollView surface old new hecs = do
--   scrolling on the same surface seems not to work, I get garbled results.
--   Not sure what the best way to do this is.
--   let new_surface = surface
  new_surface <- withTargetSurface $ \surface ->
                   liftIO $ createSimilarSurface surface ContentColor
                               (width new) (height new)

  renderWith new_surface $ do
    let scale    = scaleValue new
        old_hadj = hadjValue old
        new_hadj = hadjValue new
        w        = fromIntegral (width new)
        h        = fromIntegral (height new)
        off      = (old_hadj - new_hadj) / scale

--   liftIO $ printf "scrollView: old: %f, new %f, dist = %f (%f pixels)\n"
--              old_hadj new_hadj (old_hadj - new_hadj) off

    -- copy the content from the old surface to the new surface,
    -- shifted by the appropriate amount.
    setSourceSurface surface off 0
    if old_hadj > new_hadj
       then rectangle off 0 (w - off) h -- scroll right.
       else rectangle 0   0 (w + off) h -- scroll left.
    fill

    let rect | old_hadj > new_hadj
             = Rectangle 0 0 (ceiling off) (height new)
             | otherwise
             = Rectangle (truncate (w + off)) 0 (ceiling (-off)) (height new)

    case rect of
      Rectangle x y w h -> rectangle (fromIntegral x) (fromIntegral y)
                                     (fromIntegral w) (fromIntegral h)
    setSourceRGBA 0xffff 0xffff 0xffff 0xffff
    fill

    renderTraces new hecs rect

  surfaceFinish surface
  return new_surface

--------------------------------------------------------------------------------

-- | Update the X scale widget, based on the state of all timeline areas.
-- For simplicity, unlike for the traces, we redraw the whole area
-- and not only the newly exposed area. This is comparatively very cheap.
updateXScaleArea :: TimelineState -> Timestamp -> IO ()
updateXScaleArea TimelineState{..} lastTx = do
  win <- widgetGetDrawWindow timelineXScaleArea
  (width, _) <- widgetGetSize timelineDrawingArea
  (_, xScaleAreaHeight) <- widgetGetSize timelineXScaleArea
  scaleValue <- readIORef scaleIORef
  -- Snap the view to whole pixels, to avoid blurring.
  hadjValue0 <- adjustmentGetValue timelineAdj
  let hadjValue = toWholePixels scaleValue hadjValue0
      off y = y + xScaleAreaHeight - 17
  renderWithDrawable win $
    renderXScale scaleValue hadjValue lastTx width off XScaleTime
  return ()

--------------------------------------------------------------------------------

-- | Render the Y scale area (an axis, ticks and a label for each graph),
-- based on view parameters and hecs.
renderYScaleArea :: ViewParameters -> HECs -> DrawingArea -> Render ()
renderYScaleArea ViewParameters{maxSpkValue, labelsMode, viewTraces,
                                histogramHeight, minterval}
                 hecs yScaleArea = do
  let maxP = maxSparkPool hecs
      maxH = fromIntegral $ maxYHistogram hecs
  (xoffset, _) <- liftIO $ widgetGetSize yScaleArea
  drawYScaleArea
    maxSpkValue maxP maxH minterval (fromIntegral xoffset) 0
    labelsMode histogramHeight viewTraces yScaleArea

-- | Update the Y scale widget, based on the state of all timeline areas
-- and on traces (only for graph labels and relative positions).
updateYScaleArea :: TimelineState -> Double -> Double -> Maybe Interval
                 -> Bool -> [Trace] -> IO ()
updateYScaleArea TimelineState{..} maxSparkPool maxYHistogram minterval
                 labelsMode traces = do
  win <- widgetGetDrawWindow timelineYScaleArea
  maxSpkValue  <- readIORef maxSpkIORef
  vadj_value   <- adjustmentGetValue timelineVAdj
  (xoffset, _) <- widgetGetSize timelineYScaleArea
  renderWithDrawable win $
    drawYScaleArea maxSpkValue maxSparkPool maxYHistogram minterval
      (fromIntegral xoffset) vadj_value labelsMode stdHistogramHeight traces
      timelineYScaleArea

-- | Render the Y scale area, by rendering an axis, ticks and a label
-- for each graph-like trace in turn (and only labels for other traces).
drawYScaleArea :: Double -> Double -> Double -> Maybe Interval -> Double
               -> Double -> Bool -> Int -> [Trace] -> DrawingArea
               -> Render ()
drawYScaleArea maxSpkValue maxSparkPool maxYHistogram minterval xoffset
               vadj_value labelsMode histogramHeight traces yScaleArea = do
  let histTotalHeight = histogramHeight + histXScaleHeight
      ys = map (subtract (round vadj_value)) $
             traceYPositions labelsMode histTotalHeight traces
  pcontext <- liftIO $ widgetCreatePangoContext yScaleArea
  zipWithM_
     (drawSingleYScale
        maxSpkValue maxSparkPool maxYHistogram minterval xoffset
        histogramHeight pcontext)
     traces ys

-- | Render a single Y scale axis, set of ticks and label, or only a label,
-- if the trace is not a graph.
drawSingleYScale :: Double -> Double -> Double -> Maybe Interval -> Double -> Int
                 -> PangoContext -> Trace -> Int
                 -> Render ()
drawSingleYScale maxSpkValue maxSparkPool maxYHistogram minterval xoffset
                 histogramHeight pcontext trace y = do
  setSourceRGBAhex black 1
  move_to (ox, y + 8)
  layout <- liftIO $ layoutText pcontext (showTrace minterval trace)
  liftIO $ do
    layoutSetWidth layout (Just $ xoffset - 50)
    -- Note: the following does not always work, see the HACK in Timeline.hs
    layoutSetAttributes layout [AttrSize minBound maxBound 8,
                                AttrFamily minBound maxBound "sans serif"]
  showLayout layout
  case traceMaxSpark maxSpkValue maxSparkPool maxYHistogram trace of
    Just v  ->
      renderYScale
        (traceHeight histogramHeight trace) 1 v (xoffset - 13) (fromIntegral y)
    Nothing -> return ()  -- not a graph-like trace

--------------------------------------------------------------------------------

-- | Calculate Y positions of all traces.
traceYPositions :: Bool -> Int -> [Trace] -> [Int]
traceYPositions labelsMode histTotalHeight traces =
  scanl (\a b -> a + (height b) + extra + tracePad) firstTraceY traces
 where
  height b = traceHeight histTotalHeight b
  extra = if labelsMode then hecLabelExtra else 0

traceHeight :: Int -> Trace -> Int
traceHeight _ TraceHEC{}           = hecTraceHeight
traceHeight _ TraceInstantHEC{}    = hecInstantHeight
traceHeight _ TraceCreationHEC{}   = hecSparksHeight
traceHeight _ TraceConversionHEC{} = hecSparksHeight
traceHeight _ TracePoolHEC{}       = hecSparksHeight
traceHeight h TraceHistogram       = h
traceHeight _ TraceGroup{}         = error "traceHeight"
traceHeight _ TraceActivity        = activityGraphHeight

-- | Calculate the total Y span of all traces.
calculateTotalTimelineHeight :: Bool -> Int -> [Trace] -> Int
calculateTotalTimelineHeight labelsMode histTotalHeight traces =
 last (traceYPositions labelsMode histTotalHeight traces)

-- | Produce a descriptive label for a trace.
showTrace :: Maybe Interval -> Trace -> String
showTrace _ (TraceHEC n) =
  "HEC " ++ show n
showTrace _ (TraceInstantHEC n) =
  "HEC " ++ show n ++ "\nInstant"
showTrace _ (TraceCreationHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark creation rate (spark/ms)"
showTrace _ (TraceConversionHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark conversion rate (spark/ms)"
showTrace _ (TracePoolHEC n) =
  "\nHEC " ++ show n ++ "\n\nSpark pool size"
showTrace Nothing TraceHistogram =
  "Sum of spark times\n(" ++ mu ++ "s)"
showTrace Just{}  TraceHistogram =
  "Sum of selected spark times\n(" ++ mu ++ "s)"
showTrace _ TraceActivity =
  "Activity"
showTrace _ TraceGroup{} = error "Render.showTrace"

-- | Calcaulate the maximal Y value for a graph-like trace, or Nothing.
traceMaxSpark :: Double -> Double -> Double -> Trace -> Maybe Double
traceMaxSpark maxS _ _ TraceCreationHEC{}   = Just $ maxS * 1000
traceMaxSpark maxS _ _ TraceConversionHEC{} = Just $ maxS * 1000
traceMaxSpark _ maxP _ TracePoolHEC{}       = Just $ maxP
traceMaxSpark _ _ maxH TraceHistogram       = Just $ maxH
traceMaxSpark _ _ _ _ = Nothing

-- | Snap a value to a whole pixel, based on drawing scale.
toWholePixels :: Double -> Double -> Double
toWholePixels 0     _ = 0
toWholePixels scale x = fromIntegral (truncate (x / scale)) * scale
