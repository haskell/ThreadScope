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

keyBarHeight :: Int
keyBarHeight = barHeight `div` 2

drawKey :: Render ()
drawKey
  = do selectFontFace "times" FontSlantNormal FontWeightNormal
       setFontSize 12

       -- Thread events column.
       setSourceRGBAhex green 1.0
       draw_rectangle 10 0 50 keyBarHeight
       setSourceRGBAhex black 1.0
       moveTo 15 22
       textPath "running"
       C.fill

       setSourceRGBAhex lightBlue 1.0
       draw_line (10, 30) (10, 50)
       setSourceRGBAhex black 1.0
       moveTo 15 42
       textPath "create thread"
       C.fill

       setSourceRGBAhex darkGreen 1.0
       draw_line (10, 60) (10, 80)
       setSourceRGBAhex black 1.0
       moveTo 15 72
       textPath "thread runnable"
       C.fill

       setSourceRGBAhex darkRed 1.0
       draw_line (10, 90) (10, 110)
       setSourceRGBAhex black 1.0
       moveTo 15 102
       textPath "migrate thread"
       C.fill

       setSourceRGBAhex purple 1.0
       draw_line (10, 120) (10, 140) 
       setSourceRGBAhex black 1.0
       moveTo 15 132
       textPath "thread wakeup"
       C.fill

       -- Spark events column.
       setSourceRGBAhex yellow 1.0
       draw_line (200, 0) (200, 20)
       setSourceRGBAhex black 1.0
       moveTo 205 12
       textPath "create spark"
       C.fill

       setSourceRGBAhex magenta 1.0
       draw_line (200, 30) (200, 50)
       setSourceRGBAhex black 1.0
       moveTo 205 42
       textPath "run spark"
       C.fill

       -- GC events column.
       setSourceRGBAhex orange 1.0
       draw_rectangle 400 0 50 keyBarHeight
       setSourceRGBAhex black 1.0
       moveTo 405 22
       textPath "GC"
       C.fill

       setSourceRGBAhex cyan 1.0
       draw_line (400, 30) (400, 50)
       setSourceRGBAhex black 1.0
       moveTo 405 42
       textPath "seq GC req"
       C.fill

       setSourceRGBAhex darkBlue 1.0
       draw_line (400, 60) (400, 80)
       setSourceRGBAhex black 1.0
       moveTo 405 72
       textPath "par GC req"
       C.fill
       setSourceRGBAhex cyan 1.0

       -- Shutdown events column.
       setSourceRGBA (102/256) 0.0 (105/256) 1.0
       draw_rectangle 600 0 50 keyBarHeight
       setSourceRGBAhex black 1.0
       moveTo 605 22
       textPath "shutdown"
       C.fill

