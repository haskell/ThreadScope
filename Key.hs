-------------------------------------------------------------------------------
--- $Id: Key.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Key.hs $
-------------------------------------------------------------------------------




module Key
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

import CairoDrawing
import EventlogViewerCommon
import ViewerColours

-------------------------------------------------------------------------------

updateKeyCanvas :: DrawingArea -> Event -> IO Bool
updateKeyCanvas canvas _
  = do win <- widgetGetDrawWindow canvas
       renderWithDrawable win drawKey
       return True

-------------------------------------------------------------------------------

drawKey :: Render ()
drawKey
  = do selectFontFace "times" FontSlantNormal FontWeightNormal
       setFontSize 12
       setSourceRGBAhex runningColour 1.0
       rectangle 10 0 50 (fromIntegral (barHeight `div` 2))
       C.fill
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 15 22
       textPath "running"
       C.fill

       setSourceRGBAhex orange 1.0
       rectangle 70 0 50 (fromIntegral (barHeight `div` 2))
       C.fill
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 80 22
       textPath "GC"
       C.fill

       setSourceRGBAhex lightBlue 1.0
       setLineWidth 2.0
       moveTo 130 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 135 15
       textPath "create thread"
       C.fill

       setSourceRGBAhex magenta 1.0
       moveTo 210 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 215 15
       textPath "run spark"
       C.fill

       setSourceRGBAhex darkGreen 1.0
       moveTo 270 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 275 15
       textPath "thread runnable"
       C.fill

       setSourceRGBAhex cyan 1.0
       moveTo 360 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 365 15
       textPath "seq GC req"
       C.fill

       setSourceRGBAhex darkBlue 1.0
       moveTo 430 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 435 15
       textPath "par GC req"
       C.fill

       setSourceRGBAhex darkRed 1.0
       moveTo 500 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 505 15
       textPath "migrate thread"
       C.fill

       setSourceRGBAhex purple 1.0
       moveTo 580 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 585 15
       textPath "thread wakeup"
       C.fill

       setSourceRGBAhex shutdownColour 1.0
       rectangle 670 0 (fromIntegral barHeight) (fromIntegral barHeight)
       C.fill
       moveTo 695 15
       setSourceRGBA 0.0 0.0 0.0 1.0
       textPath "shutdown"
       C.fill

-------------------------------------------------------------------------------

