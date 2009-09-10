-------------------------------------------------------------------------------
--- $Id: Ticks.hs#2 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Ticks.hs $
-------------------------------------------------------------------------------

module Ticks
where
import Graphics.UI.Gtk
import EventlogViewerCommon
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C
import Control.Monad

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Text.Printf 

import CairoDrawing
import EventlogViewerCommon
import ViewerColours

-------------------------------------------------------------------------------
-- Minor, semi-major and major ticks are drawn and the absolute periods of 
-- the ticks is determined by the zoom level.
-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick). 

-- The timestamp values are in nanos-seconds (1e-9) i.e. 
-- a timestamp value of 1000000000 represents 1s.
-- The position on the drawing canvas is in milliseconds (ms) (1e-3).

-- scaleValue is used to multiply a timestamp value to yield a pixel value.

-------------------------------------------------------------------------------

drawTicks :: Int -> Double -> Timestamp -> Timestamp -> 
             Timestamp -> Timestamp -> Render ()
drawTicks height scaleValue pos incr majorTick endPos
  = if pos <= endPos then
      do draw_line (x0, y0) (x1,y1)
         stroke
         when (pos `mod` majorTick == 0 || pos `mod` (majorTick `div` 2) == 0) $ do
               move_to (ox+scaleIntegerBy pos scaleValue - 4, oy-10)
               textPath (showTickTime pos)
               C.fill
               setSourceRGBAhex blue 0.2
               draw_line (x1, y1) (x1, height)
               setSourceRGBAhex blue 1.0
         
         drawTicks height scaleValue (pos+incr) incr majorTick endPos
    else
      return ()
    where
    (x0, y0, x1, y1) = if pos `mod` majorTick == 0 then
                         (ox+ oxs, oy, ox+oxs, oy+16)
                       else 
                         if pos `mod` (majorTick `div` 2) == 0 then
                           (ox+oxs, oy, ox+oxs, oy+12)
                         else
                           (ox+oxs, oy, ox+oxs, oy+8)
    oxs = scaleIntegerBy pos scaleValue

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
        reformatMS  (posf / 1000) ++ "us"  -- microsecond (1e-6s).
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

