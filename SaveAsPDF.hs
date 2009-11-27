module SaveAsPDF
where

-- Imports from Haskell library
import Control.Monad
import Data.IORef

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for ThreadScope
import EventsWindow
import State
import Timeline
import Traces

-------------------------------------------------------------------------------

saveAsPDF :: ViewerState -> IO ()
saveAsPDF state@ViewerState{..} 
  = liftIO $ do
    value <- adjustmentGetValue eventsAdj
    mb_hecs <- readIORef hecsIORef
    Just fn <- readIORef filenameIORef
    case mb_hecs of
      Nothing   -> return ()
      Just hecs -> do
        let arr = hecEventArray hecs
        win <- widgetGetDrawWindow eventsDrawingArea
        (w,h) <- widgetGetSize eventsDrawingArea
    
        cursorpos <- getCursorLine state
        let r = drawEvents value arr w h cursorpos >> showPage
        withPDFSurface (fn++".pdf") (fromIntegral w) (fromIntegral h) (flip renderWith $ r)
        return ()
    
-------------------------------------------------------------------------------
