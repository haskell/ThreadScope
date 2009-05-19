module CapabilityLabels
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

-- Imports from Haskell library
import Control.Monad
import Data.IORef
import Data.Maybe

-- Imports for ThreadScope
import EventlogViewerCommon
import ViewerColours

-------------------------------------------------------------------------------

updateCapabilityCanvas :: DrawingArea -> IORef (Maybe [Int]) -> Event ->
                          IO Bool
updateCapabilityCanvas canvas capabilitiesIORef (Expose { eventArea=rect }) 
   = do maybeCapabilities <- readIORef capabilitiesIORef
        when (maybeCapabilities /= Nothing)
          (do let Just capabilities = maybeCapabilities
              win <- widgetGetDrawWindow canvas 
              gc <- gcNew win
              mapM_ (labelCapability canvas gc) capabilities
          )
        return True

-------------------------------------------------------------------------------

labelCapability :: DrawingArea -> GC -> Int -> IO ()
labelCapability canvas gc n
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` ("HEC " ++ show n)
       drawLayoutWithColors win gc 10 (oycap+6+gapcap*n) txt (Just black) Nothing

-------------------------------------------------------------------------------
