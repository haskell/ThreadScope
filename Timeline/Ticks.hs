module Timeline.Ticks (
    renderTicks
  ) where

import Timeline.Render.Constants
import CairoDrawing
import ViewerColours

import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Control.Monad

--import Debug.Trace
--import Text.Printf 

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

renderTicks :: Timestamp -> Timestamp -> Double -> Int -> Render()
renderTicks startPos endPos scaleValue height
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
    drawTicks tickWidthInPixels height scaleValue firstTick 
              snappedTickDuration  (10*snappedTickDuration) endPos
  

drawTicks :: Int -> Int -> Double -> Timestamp -> Timestamp -> 
             Timestamp -> Timestamp -> Render ()
drawTicks tickWidthInPixels height scaleValue pos incr majorTick endPos
  = if pos <= endPos then
      do setLineWidth scaleValue
         draw_line (x0, y0) (x1, y1)
         when (atMajorTick || atMidTick || tickWidthInPixels > 30) $ do
               move_to (pos - truncate (scaleValue * 4.0), oy - 10)
               m <- getMatrix
               identityMatrix
               tExtent <- textExtents tickTimeText
               (fourPixels, _) <- deviceToUserDistance 4 0
               when (textExtentsWidth tExtent + fourPixels < fromIntegral tickWidthInPixels || atMidTick || atMajorTick) $ do
                 textPath tickTimeText
                 C.fill
               setMatrix m
               setSourceRGBAhex blue 0.2
               draw_line (x1, y1) (x1, height)
               setSourceRGBAhex blue 1.0
         
         drawTicks tickWidthInPixels height scaleValue (pos+incr) incr majorTick endPos
    else
      return ()
    where
    tickTimeText = showTickTime pos
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

showTickTime :: Timestamp -> String
showTickTime pos
  = if pos == 0 then
      "0s"
    else
      if pos < 1000000 then -- Show time as micro-seconds for times < 1e-6
        reformatMS  (posf / 1000) ++ ('\x00b5':"s")  -- microsecond (1e-6s).
    else
      if pos < 100000000 then -- Show miliseonds for time < 0.1s
        reformatMS (posf / 1000000) ++ "ms" -- miliseconds 1e-3
      else -- Show time in seconds
        reformatMS (posf / 1000000000) ++ "s" 
    where
    posf :: Double
    posf = fromIntegral pos

-------------------------------------------------------------------------------

reformatMS :: Num a => a -> String
reformatMS pos
  = deZero (show pos)

-------------------------------------------------------------------------------

deZero :: String -> String
deZero str
  = if length str >= 3 && take 2 revstr == "0." then
      reverse (drop 2 revstr)
    else
      str
    where
    revstr = reverse str

-------------------------------------------------------------------------------

