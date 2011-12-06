module GUI.Timeline.Sparks (
    treesProfile,
    maxSparkRenderedValue,
    renderSparkCreation,
    renderSparkConversion,
    renderSparkPool,
    renderSparkHistogram,
  ) where

import GUI.Timeline.Render.Constants

import Events.HECs
import Events.SparkTree
import qualified Events.SparkStats as SparkStats

import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.Ticks

import Graphics.Rendering.Cairo

import Control.Monad

-- Rendering sparks. No approximation nor extrapolation is going on here.
-- The sample data, recalculated for a given slice size in sparkProfile,
-- before these functions are called, is straightforwardly rendered.

maxSparkRenderedValue :: Timestamp -> SparkStats.SparkStats -> Double
maxSparkRenderedValue duration c =
  max (SparkStats.rateDud c +
       SparkStats.rateCreated c +
       SparkStats.rateOverflowed c)
      (SparkStats.rateFizzled c +
       SparkStats.rateConverted c +
       SparkStats.rateGCd c)
  / fromIntegral duration

spark_detail :: Int
spark_detail = 4 -- in pixels

treesProfile :: Double -> Timestamp -> Timestamp -> HECs
             -> (Timestamp, [[SparkStats.SparkStats]])
treesProfile scale start end hecs =
  let slice = ceiling (fromIntegral spark_detail * scale)
      pr trees = let (_, _, stree) = trees
                 in sparkProfile slice start end stree
  in (slice, map pr (hecTrees hecs))


renderSparkCreation :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                    -> [SparkStats.SparkStats]
                    -> Render ()
renderSparkCreation params !slice !start !end prof = do
  let f1 c =        SparkStats.rateCreated c
      f2 c = f1 c + SparkStats.rateDud c
      f3 c = f2 c + SparkStats.rateOverflowed c
  renderSpark params slice start end prof
    f1 createdConvertedColour f2 fizzledDudsColour f3 overflowedColour

renderSparkConversion :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                      -> [SparkStats.SparkStats]
                      -> Render ()
renderSparkConversion params !slice !start !end prof = do
  let f1 c =        SparkStats.rateConverted c
      f2 c = f1 c + SparkStats.rateFizzled c
      f3 c = f2 c + SparkStats.rateGCd c
  renderSpark params slice start end prof
    f1 createdConvertedColour f2 fizzledDudsColour f3 gcColour

renderSparkPool :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                -> [SparkStats.SparkStats]
                -> Double -> Render ()
renderSparkPool ViewParameters{..} !slice !start !end prof !maxSparkPool = do
  let f1 c = SparkStats.minPool c
      f2 c = SparkStats.meanPool c
      f3 c = SparkStats.maxPool c
  addSparks outerPercentilesColour maxSparkPool f1 f2 start slice prof
  addSparks outerPercentilesColour maxSparkPool f2 f3 start slice prof
  outlineSparks maxSparkPool f2 start slice prof
  outlineSparks maxSparkPool (const 0) start slice prof
  renderHRulers hecSparksHeight start end

renderSpark :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
            -> [SparkStats.SparkStats]
            -> (SparkStats.SparkStats -> Double) -> Color
            -> (SparkStats.SparkStats -> Double) -> Color
            -> (SparkStats.SparkStats -> Double) -> Color
            -> Render ()
renderSpark ViewParameters{..} slice start end prof f1 c1 f2 c2 f3 c3 = do
  -- maxSpkValue is maximal spark transition rate, so
  -- maxSliceSpark is maximal number of sparks per slice for current data.
  let maxSliceSpark = maxSpkValue * fromIntegral slice
  outlineSparks maxSliceSpark f3 start slice prof
  addSparks c1 maxSliceSpark (const 0) f1 start slice prof
  addSparks c2 maxSliceSpark f1 f2 start slice prof
  addSparks c3 maxSliceSpark f2 f3 start slice prof
  renderHRulers hecSparksHeight start end

off :: Double -> (SparkStats.SparkStats -> Double)
    -> SparkStats.SparkStats
    -> Double
off maxSliceSpark f t =
  let clipped = min 1 (f t / maxSliceSpark)
  in fromIntegral hecSparksHeight * (1 - clipped)

outlineSparks :: Double
              -> (SparkStats.SparkStats -> Double)
              -> Timestamp -> Timestamp
              -> [SparkStats.SparkStats]
              -> Render ()
outlineSparks maxSliceSpark f start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t = zip points (map (off maxSliceSpark f) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t)
      mapM_ (uncurry lineTo) t
      setSourceRGBAhex black 1.0
      setLineWidth 1
      stroke

addSparks :: Color
          -> Double
          -> (SparkStats.SparkStats -> Double)
          -> (SparkStats.SparkStats -> Double)
          -> Timestamp -> Timestamp
          -> [SparkStats.SparkStats]
          -> Render ()
addSparks colour maxSliceSpark f0 f1 start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      -- liftIO $ printf "ts: %s\n" (show (map f1 (ts)))
      -- liftIO $ printf "off: %s\n"
      --   (show (map (off maxSliceSpark f1) (ts) :: [Double]))
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t0 = zip points (map (off maxSliceSpark f0) ts)
          t1 = zip points (map (off maxSliceSpark f1) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t1)
      mapM_ (uncurry lineTo) t1
      mapM_ (uncurry lineTo) (reverse t0)
      setSourceRGBAhex colour 1.0
      fill

-- | Render the spark duration histogram together with it's X scale and
-- horizontal and vertical rulers.
renderSparkHistogram :: ViewParameters -> HECs -> Render ()
renderSparkHistogram ViewParameters{..} hecs =
  let intDoub :: Integral a => a -> Double
      intDoub = fromIntegral
      inR :: Timestamp -> Bool
      inR = case minterval of
              Nothing -> const True
              Just (from, to) -> \ t -> t >= from && t <= to
      -- TODO: if xs is sorted, we can slightly optimize the filtering
      inRange :: [(Timestamp, Int, Timestamp)] -> [(Int, (Timestamp, Int))]
      inRange xs = [(logdur, (dur, 1))
                   | (start, logdur, dur) <- xs, inR start]
      xs = durHistogram hecs
      bars :: [(Double, Double, Int)]
      bars = [(intDoub t, intDoub height, count)
              | (t, (height, count)) <- histogramCounts $ inRange xs]
      -- TODO: data processing up to this point could be done only at interval
      -- changes (keeping @bars@ in ViewParameters and in probably also in IOref.
      -- The rest has to be recomputed at each redraw, because resizing
      -- the window modifies the way the graph is drawn.
      -- TODO: at least pull the above out into a separate function.

      -- Define general parameters for visualization.
      width' = width - 5  -- add a little margin on the right
      (w, h) = (intDoub width', intDoub histogramHeight)
      (minX, maxX, maxY) = (intDoub (minXHistogram hecs),
                            intDoub (maxXHistogram hecs),
                            intDoub (maxYHistogram hecs))
      nBars = max 5 (maxX - minX + 1)
      segmentWidth = w / nBars
      -- Define parameters for drawing the bars.
      gapWidth = 10
      barWidth = segmentWidth - gapWidth
      sX x = gapWidth / 2 + (x - minX) * segmentWidth
      sY y = y * h / (max 2 maxY)
      plotRect (x, y, count) = do
        -- Draw a single bar.
        setSourceRGBAhex blue 1.0
        rectangle (sX x) (sY maxY) barWidth (sY (-y))
        fillPreserve
        setSourceRGBA 0 0 0 0.7
        setLineWidth 1
        stroke
        -- Print the number of sparks in the bar.
        selectFontFace "sans serif" FontSlantNormal FontWeightNormal
        setFontSize 10
        let above = sY (-y) > -20
        if above
          then setSourceRGBAhex black 1.0
          else setSourceRGBAhex white 1.0
        moveTo (sX x + 3) (sY (maxY - y) + if above then -3 else 13)
        showText (show count)
      drawHist = forM_ bars plotRect
      -- Define parameters for X scale.
      off y = 16 - y
      xScaleMode = XScaleLog minX segmentWidth
      drawXScale = renderXScale 1 0 maxBound width' off xScaleMode
      -- Define parameters for vertical rulers.
      nB = round nBars
      mult | nB <= 7 = 1
           | nB `mod` 5 == 0 = 5
           | nB `mod` 4 == 0 = 4
           | nB `mod` 3 == 0 = 3
           | nB `mod` 2 == 0 = nB `div` 2
           | otherwise = nB
      drawVRulers = renderVRulers 1 0 (fromIntegral width') histogramHeight
                      (XScaleLog undefined (segmentWidth * fromIntegral mult))
      -- Define the horizontal rulers call.
      drawHRulers = renderHRulers histogramHeight 0 (fromIntegral width')
  in do
    -- Start the drawing by wiping out timeline vertical rules
    -- (for PNG/PDF that require clear, transparent background)
    save
    translate hadjValue 0
    scale scaleValue 1
    rectangle 0 (fromIntegral $ - tracePad) (fromIntegral width)
      (fromIntegral $ histogramHeight + histXScaleHeight + 2 * tracePad)
    setSourceRGBAhex white 1
    op <- getOperator
    setOperator OperatorAtop  -- TODO: fixme: it paints white vertical rulers
    fill
    setOperator op
    -- Draw the bars.
    drawHist
    -- Draw the rulers on top of the bars (they are partially transparent).
    drawVRulers
    drawHRulers
    -- Move to the bottom and draw the X scale. The Y scale is drawn
    -- independetly in another drawing area.
    translate 0 (fromIntegral histogramHeight)
    drawXScale
    restore
