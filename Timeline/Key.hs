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
       renderWithDrawable win addKeyElements
       return True

-------------------------------------------------------------------------------

data KeyStyle = Box | Vertical

-------------------------------------------------------------------------------

addKeyElements :: Render ()
addKeyElements 
  = do selectFontFace "sans serif" FontSlantNormal FontWeightNormal
       setFontSize 12
       addKeyElements' 10 [(Box, "running", runningColour),
                           (Box, "GC", gcColour),
                           (Vertical, "create thread", createThreadColour),
                           (Vertical, "run spark", runSparkColour),
                           (Vertical, "thread runnable", threadRunnableColour),
                           (Vertical, "seq GC req", seqGCReqColour),
                           (Vertical, "par GC req", parGCReqColour),
                           (Vertical, "migrate thread", migrateThreadColour),
                           (Vertical, "thread wakeup", threadWakeupColour),
                           (Vertical, "shutdown", shutdownColour)]

-------------------------------------------------------------------------------

addKeyElements' :: Double -> [(KeyStyle, String, Color)] -> Render ()
addKeyElements' position [] = return ()
addKeyElements' position ((Box, keyText, keyColour):rest)
  = do setSourceRGBAhex keyColour 1.0
       rectangle position 0 50 (fromIntegral (hecBarHeight `div` 2))
       C.fill
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo (position+5) 22
       textPath keyText
       C.fill  
       tExtent <- textExtents keyText
       let textW = textExtentsWidth tExtent + 10
       addKeyElements' (position + (60 `max` textW)) rest
addKeyElements' position ((Vertical, keyText, keyColour):rest)
  = do setSourceRGBAhex keyColour 1.0
       setLineWidth 3.0
       moveTo position 0
       relLineTo 0 25
       C.stroke
       setSourceRGBA 0.0 0.0 0.0 1.0
       moveTo (position+5) 15
       textPath keyText
       C.fill
       tExtent <- textExtents keyText
       addKeyElements' (position + 20 + textExtentsWidth tExtent)
                       rest

-------------------------------------------------------------------------------

