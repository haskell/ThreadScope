module CapabilityLabels where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

-- Imports from Haskell library
import Control.Monad
import Data.IORef
import Data.Maybe

-- Imports for ThreadScope
import State
import EventlogViewerCommon
import ViewerColours

-------------------------------------------------------------------------------

updateCapabilityDrawingArea :: ViewerState -> Event -> IO Bool
updateCapabilityDrawingArea ViewerState{..} (Expose { eventArea=rect }) 
   = do maybeCapabilities <- readIORef capabilitiesIORef
        when (maybeCapabilities /= Nothing)
          (do let Just capabilities = maybeCapabilities
              win <- widgetGetDrawWindow capDrawingArea
              gc <- gcNew win
              mapM_ (labelCapability capDrawingArea gc) capabilities
          )
        return True
updateCapabilityDrawingArea _ _ = error "updateCapabilityDrawingArea"

-------------------------------------------------------------------------------

labelCapability :: DrawingArea -> GC -> Int -> IO ()
labelCapability canvas gc n
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` ("HEC " ++ show n)
       drawLayoutWithColors win gc 10 (oycap+6+gapcap*n) txt (Just black) Nothing

-------------------------------------------------------------------------------
