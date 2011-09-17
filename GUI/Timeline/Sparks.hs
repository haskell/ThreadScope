module GUI.Timeline.Sparks (
    treesProfile,
    maxSparkRenderedValue,
    renderSparkCreation,
    renderSparkConversion,
    renderSparkPool,
    addScale,
  ) where

import GUI.Timeline.Render.Constants

import Events.HECs
import Events.SparkTree
import qualified Events.SparkStats as SparkStats

import GUI.Types
import GUI.ViewerColours
import GUI.Timeline.Ticks (dashedLine1, drawVTicks)

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

import Control.Monad

-- import Text.Printf

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
  let slice = round (fromIntegral spark_detail * scale)
      pr trees = let (_, _, stree) = trees
                 in sparkProfile slice start end stree
  in (slice, map pr (hecTrees hecs))


renderSparkCreation :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                       -> [SparkStats.SparkStats]
                       -> Render ()
renderSparkCreation params !slice !start !end prof = do
  let f1 c =        SparkStats.rateDud c
      f2 c = f1 c + SparkStats.rateCreated c
      f3 c = f2 c + SparkStats.rateOverflowed c
  renderSpark params slice start end prof
    f1 fizzledDudsColour f2 createdConvertedColour f3 overflowedColour

renderSparkConversion :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                         -> [SparkStats.SparkStats]
                         -> Render ()
renderSparkConversion params !slice !start !end prof = do
  let f1 c =        SparkStats.rateFizzled c
      f2 c = f1 c + SparkStats.rateGCd c
      f3 c = f2 c + SparkStats.rateConverted c
  renderSpark params slice start end prof
    f1 fizzledDudsColour f2 gcColour f3 createdConvertedColour

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
  addRulers hecSparksHeight start end

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
  addRulers hecSparksHeight start end

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
      save
      identityMatrix
      setLineWidth 1
      stroke
      restore

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


-- TODO: redesign and refactor the scales code after some feedback and testing.

-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick).
-- The timestamp values are in nanoseconds (1e-9), i.e.,
-- a timestamp value of 1000000000 represents 1s.
-- The x-position on the drawing canvas is in milliseconds (ms) (1e-3).
-- scaleValue is used to divide a timestamp value to yield a pixel value.
addScale :: Int -> Double -> Double -> Double -> Double -> Render ()
addScale hecSparksHeight scaleValue maxSpark xoffset yoffset = do
  let -- This is slightly off (by 1% at most), but often avoids decimal dot:
      maxS = if maxSpark < 100
             then maxSpark  -- too small, would be visible on screen
             else fromIntegral (2 * (ceiling maxSpark ` div` 2))
      incr = hecSparksHeight `div` 10

  newPath
  moveTo xoffset yoffset
  lineTo xoffset (yoffset + fromIntegral hecSparksHeight)
  setSourceRGBAhex blue 1.0
  save
  identityMatrix
  setLineWidth 1
  stroke
  restore

  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex blue 1.0
  save
  scale scaleValue 1.0
  setLineWidth 0.5
  let yoff = truncate yoffset
      xoff = truncate xoffset
  drawVTicks maxS 0 incr xoff yoff
  restore

addRulers :: Int -> Timestamp -> Timestamp -> Render ()
addRulers hecSparksHeight start end = do
  let dstart = fromIntegral start
      dend = fromIntegral end
      incr = hecSparksHeight `div` 10

  -- dashed lines across the graphs
  setSourceRGBAhex black 0.3
  save
  forM_ [0, 5] $ \h -> do
    let y = fromIntegral $ h * incr
    moveTo dstart y
    lineTo dend y
    dashedLine1
  restore
