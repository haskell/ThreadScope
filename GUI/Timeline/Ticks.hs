module GUI.Timeline.Ticks (
    renderVRulers,
    renderXScale,
    renderHRulers,
    renderYScale,
    mu,
    deZero,
    dashedLine1,
  ) where

import GUI.Timeline.Render.Constants
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo

-- Imports for GHC Events
import GHC.RTS.Events hiding (Event)

import Control.Monad
import Text.Printf

-- import Debug.Trace

-- Minor, semi-major and major ticks are drawn and the absolute periods of
-- the ticks is determined by the zoom level.
-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick).

-- The timestamp values are in nanos-seconds (1e-9) i.e.
-- a timestamp value of 1000000000 represents 1s.
-- The position on the drawing canvas is in milliseconds (ms) (1e-3).

-- scaleValue is used to divide a timestamp value to yield a pixel value.

-- NOTE: the code below will crash if the timestampFor100Pixels is 0.
-- The zoom factor should be controlled to ensure that this never happens.

renderVRulers :: Timestamp -> Timestamp -> Double -> Int -> Render()
renderVRulers startPos endPos scaleValue height = do
  setSourceRGBAhex blue 0.2
  let timestampFor100Pixels = truncate (100 * scaleValue)  -- ns time for 100 ps
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
      tickWidthInPixels :: Int
      tickWidthInPixels =
        truncate ((fromIntegral snappedTickDuration) / scaleValue)
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  setLineWidth scaleValue
  drawVRulers tickWidthInPixels height scaleValue firstTick
    snappedTickDuration endPos


drawVRulers :: Int -> Int -> Double -> Timestamp -> Timestamp
               -> Timestamp -> Render ()
drawVRulers tickWidthInPixels height scaleValue pos incr endPos =
  if pos <= endPos then do
    when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      draw_line (x1, 0) (x1, height)
      drawVRulers
        tickWidthInPixels height scaleValue (pos + incr) incr endPos
  else
    return ()
  where
    midTick = 5 * incr
    atMidTick = pos `mod`midTick == 0
    majorTick = 10 * incr
    atMajorTick = pos `mod` majorTick == 0
    -- We cheat at pos 0, to avoid half covering the tick by the grey label area.
    lineWidth = scaleValue
    x1 = if pos == 0 then ceiling (lineWidth / 2) else pos


-- TODO: refactor common parts with renderVRulers
renderXScale :: Timestamp -> Timestamp -> Double -> Int -> Render()
renderXScale startPos endPos scaleValue yoffset = do
  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex blue 1.0
  setLineWidth 1.0
  -- trace (printf "startPos: %d, endPos: %d" startPos endPos) $ do
  draw_line (startPos, yoffset - 16) (endPos, yoffset - 16)
  let timestampFor100Pixels = truncate (100 * scaleValue)  -- ns time for 100 ps
      snappedTickDuration :: Timestamp
      snappedTickDuration =
        10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
      tickWidthInPixels :: Int
      tickWidthInPixels =
        truncate ((fromIntegral snappedTickDuration) / scaleValue)
      firstTick :: Timestamp
      firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
  -- liftIO $
  --   do putStrLn ("timestampFor100Pixels = " ++ show timestampFor100Pixels)
  --     putStrLn ("tickWidthInPixels     = " ++ show tickWidthInPixels)
  --     putStrLn ("snappedTickDuration   = " ++ show snappedTickDuration)
  setLineWidth scaleValue
  drawXTicks
    tickWidthInPixels scaleValue firstTick snappedTickDuration endPos yoffset


drawXTicks :: Int -> Double -> Timestamp -> Timestamp -> Timestamp -> Int
              -> Render ()
drawXTicks tickWidthInPixels scaleValue pos incr endPos yoffset =
  if pos <= endPos then do
    draw_line (x1, yoffset - 16) (x1, yoffset - 16 + tickLength)
    when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
      move_to (pos - truncate (scaleValue * 4.0), yoffset - 26)
      m <- getMatrix
      identityMatrix
      tExtent <- textExtents tickTimeText
      (fourPixels, _) <- deviceToUserDistance 4 0
      when (isWideEnough tExtent fourPixels || atMajorTick) $
        showText tickTimeText
      setMatrix m
    drawXTicks tickWidthInPixels scaleValue (pos + incr) incr endPos yoffset
  else
    return ()
  where
    tickTimeText = showMultiTime pos
    width = if atMidTick then 5 * tickWidthInPixels
            else tickWidthInPixels
    isWideEnough tExtent fourPixels =
      textExtentsWidth tExtent + fourPixels < fromIntegral width
    midTick = 5 * incr
    atMidTick = pos `mod`midTick == 0
    majorTick = 10 * incr
    atMajorTick = pos `mod` majorTick == 0
    -- We cheat at pos 0, to avoid half covering the tick by the grey label area.
    lineWidth = scaleValue
    x1 = if pos == 0 then ceiling (lineWidth / 2) else pos
    tickLength | atMajorTick = 16
               | atMidTick = 12
               | otherwise = 8


-- This display the nano-second time unit with an appropriate suffix
-- depending on the actual time value.
-- For times < 1e-6 the time is shown in micro-seconds.
-- For times >= 1e-6 and < 0.1 seconds the time is shown in ms
-- For times >= 0.5 seconds the time is shown in seconds
showMultiTime :: Timestamp -> String
showMultiTime pos =
  if pos == 0 then "0s"
  else if pos < 1000000 then -- Show time as micro-seconds for times < 1e-6
         reformatMS  (posf / 1000) ++ (mu ++ "s")  -- microsecond (1e-6s).
       else if pos < 100000000 then -- Show miliseonds for time < 0.1s
              reformatMS (posf / 1000000) ++ "ms" -- miliseconds 1e-3
            else -- Show time in seconds
              reformatMS (posf / 1000000000) ++ "s"
  where
    posf :: Double
    posf = fromIntegral pos
    reformatMS :: Num a => a -> String
    reformatMS pos = deZero (show pos)

-------------------------------------------------------------------------------

-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick).
-- The timestamp values are in nanoseconds (1e-9), i.e.,
-- a timestamp value of 1000000000 represents 1s.
-- The x-position on the drawing canvas is in milliseconds (ms) (1e-3).
-- scaleValue is used to divide a timestamp value to yield a pixel value.
renderHRulers :: Int -> Timestamp -> Timestamp -> Render ()
renderHRulers hecSparksHeight start end = do
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


renderYScale :: Int -> Double -> Double -> Double -> Double -> Render ()
renderYScale hecSparksHeight scaleValue maxSpark xoffset yoffset = do
  let -- This is slightly off (by 1% at most), but often avoids decimal dot:
      maxS = if maxSpark < 100
             then maxSpark  -- too small, would be visible on screen
             else fromIntegral (2 * (ceiling maxSpark ` div` 2))
      incr = hecSparksHeight `div` 10

  newPath
  moveTo xoffset yoffset
  lineTo xoffset (yoffset + fromIntegral hecSparksHeight)
  setSourceRGBAhex blue 1.0
  stroke

  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex blue 1.0
  save
  scale scaleValue 1.0
  setLineWidth 0.5
  let yoff = truncate yoffset
      xoff = truncate xoffset
  drawYTicks maxS 0 incr xoff yoff
  restore


drawYTicks :: Double -> Int -> Int -> Int -> Int -> Render ()
drawYTicks maxS pos incr xoffset yoffset =
  if pos <= majorTick then do
    draw_line (xoffset             , yoffset + majorTick - pos)
              (xoffset + tickLength, yoffset + majorTick - pos)
    when (atMajorTick || atMidTick) $ do
      tExtent <- textExtents tickText
      (fewPixels, _) <- deviceToUserDistance 8 0
      move_to (xoffset - ceiling (textExtentsWidth tExtent + fewPixels),
               yoffset + majorTick - pos + ceiling (fewPixels / 2))
      when (atMidTick || atMajorTick) $
        showText tickText
    drawYTicks maxS (pos + incr) incr xoffset yoffset
  else
    return ()
  where
    tickText = reformatV (maxS * fromIntegral pos / fromIntegral majorTick)
    midTick = 5 * incr
    atMidTick = pos `mod`midTick == 0
    majorTick = 10 * incr
    atMajorTick = pos `mod` majorTick == 0
    tickLength | atMajorTick = 13
               | atMidTick   = 10
               | otherwise   = 6
    reformatV :: Double -> String
    reformatV v = deZero (printf "%.2f" v)

-------------------------------------------------------------------------------

mu :: String
#if MIN_VERSION_cairo(0,12,0) && !MIN_VERSION_cairo(0,12,1)
-- this version of cairo doesn't handle Unicode properly.
-- Thus, we do the encoding by hand:
mu = "\194\181"
#else
-- Haskell cairo bindings 0.12.1 have proper Unicode support
mu = "\x00b5"
#endif


deZero :: String -> String
deZero s
  | '.' `elem` s =
    reverse . dropWhile (=='.') . dropWhile (=='0') . reverse $ s
  | otherwise = s


dashedLine1 :: Render ()
dashedLine1 = do
  save
  identityMatrix
  let dash = fromIntegral ox
  setDash [dash, dash] 0.0
  setLineWidth 1
  stroke
  restore
