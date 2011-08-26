-------------------------------------------------------------------------------
--- $Id: CairoDrawing.hs#3 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/CairoDrawing.hs $
-------------------------------------------------------------------------------

module GUI.Timeline.CairoDrawing
where

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C
import Control.Monad

-------------------------------------------------------------------------------

{-# INLINE draw_line #-}
draw_line :: (Integral a, Integral b, Integral c, Integral d) =>
             (a, b) -> (c, d) -> Render ()
draw_line (x0, y0) (x1, y1)
  = do move_to (x0, y0)
       lineTo (fromIntegral x1) (fromIntegral y1)
       stroke

{-# INLINE move_to #-}
move_to :: (Integral a, Integral b) => (a, b) -> Render ()
move_to (x, y)
  = moveTo (fromIntegral x) (fromIntegral y)

{-# INLINE rel_line_to #-}
rel_line_to :: (Integral a, Integral b) => (a, b) -> Render ()
rel_line_to (x, y)
  = relLineTo (fromIntegral x) (fromIntegral y)

-------------------------------------------------------------------------------

{-# INLINE draw_rectangle #-}
draw_rectangle :: (Integral x, Integral y, Integral w, Integral h)
               => x -> y -> w -> h
               -> Render ()
draw_rectangle x y w h = do
  rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  C.fill

-------------------------------------------------------------------------------

{-# INLINE draw_outlined_rectangle #-}
draw_outlined_rectangle :: (Integral x, Integral y, Integral w, Integral h)
                        => x -> y -> w -> h
                        -> Render ()
draw_outlined_rectangle x y w h = do
  rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  fillPreserve
  setLineWidth 1
  setSourceRGBA 0 0 0 0.7
  stroke

-------------------------------------------------------------------------------

{-# INLINE draw_rectangle_opt #-}
draw_rectangle_opt :: (Integral x, Integral y, Integral w, Integral h)
                   => Bool -> x -> y -> w -> h
                   -> Render ()
draw_rectangle_opt opt x y w h
  = draw_rectangle_opt' opt (fromIntegral x) (fromIntegral y)
                            (fromIntegral w) (fromIntegral h)

draw_rectangle_opt' :: Bool -> Double -> Double -> Double -> Double
                    -> Render ()
draw_rectangle_opt' opt x y w h
  = do rectangle x y (1.0 `max` w) h
       C.fill
       when opt $ do
         setLineWidth 1
         setSourceRGBA 0 0 0 0.7
         rectangle x y w h
         stroke

-------------------------------------------------------------------------------

{-# INLINE draw_rectangle_outline #-}
draw_rectangle_outline :: (Integral x, Integral y, Integral w, Integral h)
                       => x -> y -> w -> h
                       -> Render ()
draw_rectangle_outline x y w h = do
  setLineWidth 2
  rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
  stroke

-------------------------------------------------------------------------------

clearWhite :: Render ()
clearWhite = do
  save
  setOperator OperatorSource
  setSourceRGBA 0xffff 0xffff 0xffff 0xffff
  paint
  restore
