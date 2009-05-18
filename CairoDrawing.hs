-------------------------------------------------------------------------------
--- $Id: CairoDrawing.hs#2 2009/03/25 16:27:24 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/CairoDrawing.hs $
-------------------------------------------------------------------------------

module CairoDrawing
where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C
import Control.Monad

-------------------------------------------------------------------------------

draw_line (x0, y0) (x1, y1)
  = do move_to (x0,y0)
       lineTo (fromIntegral x1) (fromIntegral y1)
       stroke

move_to (x, y)
  = moveTo (fromIntegral x) (fromIntegral y)

rel_line_to (x, y)
  = relLineTo (fromIntegral x) (fromIntegral y)

draw_rectangle x0 y0 w h
  = do rectangle (fromIntegral x0) (fromIntegral y0) (fromIntegral w) (fromIntegral h)
       C.fill
       -- setLineWidth 1
       --setSourceRGBA 0 0 0 0.7
       --stroke

draw_rectangle_opt opt x0 y0 w h
  = do rectangle (fromIntegral x0) (fromIntegral y0) wr1 (fromIntegral h)
       C.fill
       when opt $ do
         setLineWidth 1
         setSourceRGBA 0 0 0 0.7
         rectangle (fromIntegral x0) (fromIntegral y0) (fromIntegral w) (fromIntegral h)
         stroke
   where
   wr = fromIntegral w
   wr1 = 1.0 `max` wr 

-------------------------------------------------------------------------------

draw_rectangle_outline x0 y0 w h
  = do setLineWidth 2
       rectangle (fromIntegral x0) (fromIntegral y0) (fromIntegral w) (fromIntegral h)
       stroke

-------------------------------------------------------------------------------
