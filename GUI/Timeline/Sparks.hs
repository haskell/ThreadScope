module GUI.Timeline.Sparks (
    renderSparkCreation,
    renderSparkConversion,
    renderSparkPool,
  ) where

import GUI.Timeline.Render.Constants

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

renderSparkCreation :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                       -> [SparkStats.SparkStats]
                       -> Double -> Render ()
renderSparkCreation ViewParameters{..} !slice !start !end prof !maxSparkValue = do
  let f1 c =        SparkStats.rateDud c
      f2 c = f1 c + SparkStats.rateCreated c
      f3 c = f2 c + SparkStats.rateOverflowed c
  renderSpark hecSparksHeight scaleValue slice start end prof
    f1 fizzledDudsColour f2 createdConvertedColour f3 overflowedColour
    maxSparkValue

renderSparkConversion :: ViewParameters -> Timestamp -> Timestamp -> Timestamp
                         -> [SparkStats.SparkStats]
                         -> Double -> Render ()
renderSparkConversion ViewParameters{..} !slice !start !end prof !maxSparkValue = do
  let f1 c =        SparkStats.rateFizzled c
      f2 c = f1 c + SparkStats.rateGCd c
      f3 c = f2 c + SparkStats.rateConverted c
  renderSpark hecSparksHeight scaleValue slice start end prof
    f1 fizzledDudsColour f2 gcColour f3 createdConvertedColour
    maxSparkValue

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
  addScale hecSparksHeight scaleValue maxSparkPool start end

renderSpark :: Int -> Double -> Timestamp -> Timestamp -> Timestamp
               -> [SparkStats.SparkStats]
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> Double -> Render ()
renderSpark hecSparksHeight scaleValue slice start end prof
            f1 c1 f2 c2 f3 c3 maxSparkValue = do
  let -- Maximum number of sparks per slice for current data.
      maxSliceSpark = fromIntegral slice * maxSparkValue
      -- Maximum spark transition rate in spark/ms.
      maxSlice = maxSparkValue * 1000000
  outlineSparks maxSliceSpark f3 start slice prof
  addSparks c1 maxSliceSpark (const 0) f1 start slice prof
  addSparks c2 maxSliceSpark f1 f2 start slice prof
  addSparks c3 maxSliceSpark f2 f3 start slice prof
  addScale hecSparksHeight scaleValue maxSlice start end

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
addScale :: Int -> Double -> Double -> Timestamp -> Timestamp -> Render ()
addScale hecSparksHeight scaleValue maxSpark start end = do
  let dstart = fromIntegral start
      dend = fromIntegral end
      dheight = fromIntegral hecSparksHeight
      -- TODO: this is slightly incorrect, but probably at most 1 pixel off
      maxS = if maxSpark < 100
             then maxSpark  -- too small, accuracy would suffer
             else fromIntegral (2 * (ceiling maxSpark ` div` 2))
      -- TODO: divide maxSpark instead, for nicer round numbers display
      incr = hecSparksHeight `div` 10
      majorTick = 10 * incr

  -- dashed lines across the graphs
  setSourceRGBAhex black 0.3
  save
  forM_ [0 .. 1] $ \h -> do
    let y = fromIntegral (floor (fromIntegral h * fromIntegral majorTick / 2)) - 0.5
    moveTo dstart y
    lineTo dend y
    dashedLine1
  restore

  -- draw scales only if the drawn area includes the very start
  -- TODO: this draws the scale too often, because the drawn area begins
  -- to the left of the 0 mark; will be fixed when scales are moved outside
  -- the scrollable area.
  when (start == 0) $ do

    newPath
    moveTo dstart 0
    lineTo dstart dheight
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
    drawVTicks maxS start scaleValue 0 incr majorTick hecSparksHeight
    restore
