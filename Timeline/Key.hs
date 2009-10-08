module Timeline.Key ( updateKeyDrawingArea )  where

import Timeline.Render.Constants

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

import ViewerColours

-------------------------------------------------------------------------------

updateKeyDrawingArea :: DrawingArea -> Event -> IO Bool
updateKeyDrawingArea canvas _
  = do win <- widgetGetDrawWindow canvas
       renderWithDrawable win drawKey
       return True

-------------------------------------------------------------------------------

drawKey :: Render ()
drawKey
  = do selectFontFace "sans serif" FontSlantNormal FontWeightNormal
       setFontSize 12
       setSourceRGBAhex runningColour 1.0
       rectangle 10 0 50 (fromIntegral (hecBarHeight `div` 2))
       C.fill
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 15 22
       textPath "running"
       C.fill

       setSourceRGBAhex orange 1.0
       rectangle 70 0 50 (fromIntegral (hecBarHeight `div` 2))
       C.fill
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 80 22
       textPath "GC"
       C.fill

       setSourceRGBAhex lightBlue 1.0
       setLineWidth 3.0
       moveTo 130 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 135 15
       textPath "create thread"
       C.fill

       setSourceRGBAhex magenta 1.0
       moveTo 220 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 225 15
       textPath "run spark"
       C.fill

       setSourceRGBAhex darkGreen 1.0
       moveTo 290 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 295 15
       textPath "thread runnable"
       C.fill

       setSourceRGBAhex cyan 1.0
       moveTo 390 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 395 15
       textPath "seq GC req"
       C.fill

       setSourceRGBAhex darkBlue 1.0
       moveTo 470 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 475 15
       textPath "par GC req"
       C.fill

       setSourceRGBAhex darkRed 1.0
       moveTo 550 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 555 15
       textPath "migrate thread"
       C.fill

       setSourceRGBAhex purple 1.0
       moveTo 650 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo 655 15
       textPath "thread wakeup"
       C.fill

       setSourceRGBAhex shutdownColour 1.0
       moveTo 750 0
       relLineTo 0 25
       C.stroke
       moveTo 755 15
       setSourceRGBA 0.0 0.0 0.0 1.0
       textPath "shutdown"
       C.fill

-------------------------------------------------------------------------------

