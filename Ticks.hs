-------------------------------------------------------------------------------
--- $Id: Ticks.hs#6 2009/03/10 13:55:46 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/Ticks.hs $
-------------------------------------------------------------------------------

module Ticks
where
import Graphics.UI.Gtk
import EventlogViewerCommon
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C
import Control.Monad

import Text.Printf 

import CairoDrawing
import EventlogViewerCommon
import ViewerColours

-------------------------------------------------------------------------------

drawTicks :: Int -> Double -> Integer -> Integer -> Integer -> Integer ->
             Render ()
drawTicks height scaleValue pos incr majorTick endPos
  = if pos <= endPos then
      do draw_line (x0, y0) (x1,y1)
         stroke
         when (pos `mod` majorTick == 0) $ do
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

showTickTime :: Integer -> String
showTickTime pos
  = if pos < 1000 then
      show pos ++ " us"
    else
      if pos < 1000000 then
        deZero (printf "%f" (posf / 1000)) ++ " ms"
      else
        show (posf / 1000000) ++ " s"
    where
    posf :: Double
    posf = fromIntegral pos

-------------------------------------------------------------------------------

showTickTime2 :: Double -> Integer -> String
showTickTime2 scale pos
  = if scale <= 3.125e-3 then
      printf "%f" (posf / 1000000) ++ " s"
    else
      if scale <= 0.25 then
        deZero (printf "%f" (posf / 1000)) ++ " ms"
      else
        show pos ++ " us"
    where
    posf :: Double
    posf = fromIntegral pos

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

stripTrailingZeros :: String -> String
stripTrailingZeros s
  = if head zerosStripped == '.' then
      reverse (tail zerosStripped)
    else
      reverse zerosStripped
    where
    zerosStripped = (dropWhile ((==)'0') . reverse) s

-------------------------------------------------------------------------------
