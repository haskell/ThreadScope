module GUI.Timeline.Ticks (
    renderHTicks,
    drawVTicks,
    mu,
    deZero,
    dashedLine1
  ) where

import GUI.Timeline.Render.Constants
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo

-- Imports for GHC Events
import GHC.RTS.Events hiding (Event)

import Control.Monad
import Text.Printf

--import Debug.Trace

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------

renderHTicks :: Timestamp -> Timestamp -> Double -> Int -> Render()
renderHTicks startPos endPos scaleValue height
  = do
    selectFontFace "sans serif" FontSlantNormal FontWeightNormal
    setFontSize 12
    setSourceRGBAhex blue 1.0
    setLineWidth 1.0
    -- trace (printf "startPos: %d, endPos: %d" startPos endPos) $ do
    draw_line (startPos, oy) (endPos, oy)
    let
        timestampFor100Pixels = truncate (100 * scaleValue) -- ns time for 100 pixels
        snappedTickDuration :: Timestamp
        snappedTickDuration = 10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
        tickWidthInPixels :: Int
        tickWidthInPixels = truncate ((fromIntegral snappedTickDuration) / scaleValue)
        firstTick :: Timestamp
        firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)
    -- liftIO $
    --   do putStrLn ("timestampFor100Pixels = " ++ show timestampFor100Pixels)
    --     putStrLn ("tickWidthInPixels     = " ++ show tickWidthInPixels)
    --     putStrLn ("snappedTickDuration   = " ++ show snappedTickDuration)
    drawHTicks tickWidthInPixels height scaleValue firstTick
              snappedTickDuration  (10*snappedTickDuration) endPos


drawHTicks :: Int -> Int -> Double -> Timestamp -> Timestamp ->
             Timestamp -> Timestamp -> Render ()
drawHTicks tickWidthInPixels height scaleValue pos incr majorTick endPos
  = if pos <= endPos then
      do setLineWidth scaleValue
         draw_line (x0, y0) (x1, y1)
         when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
               move_to (pos - truncate (scaleValue * 4.0), oy - 10)
               m <- getMatrix
               identityMatrix
               tExtent <- textExtents tickTimeText
               (fourPixels, _) <- deviceToUserDistance 4 0
               when (textExtentsWidth tExtent + fourPixels < fromIntegral tickWidthInPixels || atMidTick || atMajorTick) $
                 showText tickTimeText
               setMatrix m
               setSourceRGBAhex blue 0.2
               draw_line (x1, y1) (x1, height)
               setSourceRGBAhex blue 1.0

         drawHTicks tickWidthInPixels height scaleValue (pos+incr) incr majorTick endPos
    else
      return ()
    where
    tickTimeText = showMultiTime pos
    atMidTick = pos `mod` (majorTick `div` 2) == 0
    atMajorTick = pos `mod` majorTick == 0
    (x0, y0, x1, y1) = if pos `mod` majorTick == 0 then
                         (pos, oy, pos, oy+16)
                       else
                         if pos `mod` (majorTick `div` 2) == 0 then
                           (pos, oy, pos, oy+12)
                         else
                           (pos, oy, pos, oy+8)

-------------------------------------------------------------------------------
-- This display the nano-second time unit with an appropriate suffix
-- depending on the actual time value.
-- For times < 1e-6 the time is shown in micro-seconds.
-- For times >= 1e-6 and < 0.1 seconds the time is shown in ms
-- For times >= 0.5 seconds the time is shown in seconds

showMultiTime :: Timestamp -> String
showMultiTime pos
  = if pos == 0 then
      "0s"
    else
      if pos < 1000000 then -- Show time as micro-seconds for times < 1e-6
        reformatMS  (posf / 1000) ++ (mu ++ "s")  -- microsecond (1e-6s).
    else
      if pos < 100000000 then -- Show miliseonds for time < 0.1s
        reformatMS (posf / 1000000) ++ "ms" -- miliseconds 1e-3
      else -- Show time in seconds
        reformatMS (posf / 1000000000) ++ "s"
    where
    posf :: Double
    posf = fromIntegral pos
    reformatMS :: Num a => a -> String
    reformatMS pos = deZero (show pos)

-------------------------------------------------------------------------------

-- TODO: make it more robust when parameters change, e.g., if incr is too small
drawVTicks :: Double -> Timestamp -> Double -> Int -> Int -> Int -> Int -> Render ()
drawVTicks maxS offset scaleValue pos incr majorTick endPos
  = if pos <= endPos then do
      draw_line (x0, hecSparksHeight - y0) (x1, hecSparksHeight - y1)
      when (pos > 0
            && (atMajorTick || atMidTick || tickWidthInPixels > 30)) $ do
            move_to (offset + 15,
                     fromIntegral hecSparksHeight - pos + 4)
            m <- getMatrix
            identityMatrix
            tExtent <- textExtents tickText
            (fourPixels, _) <- deviceToUserDistance 4 0
            when (textExtentsWidth tExtent + fourPixels < fromIntegral tickWidthInPixels || atMidTick || atMajorTick) $
              showText tickText
            setMatrix m
      drawVTicks maxS offset scaleValue (pos+incr) incr majorTick endPos
    else
      return ()
    where
    tickWidthInPixels :: Int
    tickWidthInPixels = truncate ((fromIntegral incr) / scaleValue)
    tickText = reformatMS (maxS * fromIntegral pos
                             / fromIntegral hecSparksHeight)
    atMidTick = pos `mod` (majorTick `div` 2) == 0
    atMajorTick = pos `mod` majorTick == 0
    (x0, y0, x1, y1) = if atMajorTick then
                         (offset, pos, offset+13, pos)
                       else
                         if atMidTick then
                           (offset, pos, offset+10, pos)
                         else
                           (offset, pos, offset+6, pos)
    reformatMS :: Double -> String
    reformatMS pos = deZero (printf "%.2f" pos)

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

-------------------------------------------------------------------------------

deZero :: String -> String
deZero s
  | '.' `elem` s =
    reverse . dropWhile (=='.') . dropWhile (=='0') . reverse $ s
  | otherwise = s

-------------------------------------------------------------------------------

dashedLine1 :: Render ()
dashedLine1 = do
  save
  identityMatrix
  setDash [10,10] 0.0
  setLineWidth 1
  stroke
  restore
