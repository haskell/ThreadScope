module GUI.SaveAsPDF
where

-- Imports from Haskell library
import Data.IORef

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-- Imports for ThreadScope
import GUI.EventsWindow
import GUI.Timeline.Render
import GUI.State
import GUI.Traces

-------------------------------------------------------------------------------

saveAsPDF :: ViewerState -> IO ()
saveAsPDF state@ViewerState{..}
  = liftIO $ do
    scaleValue <- readIORef scaleIORef
    hadj_value0 <- adjustmentGetValue timelineAdj
    let hadj_value = toWholePixels scaleValue hadj_value0
    mb_hecs <- readIORef hecsIORef
    Just fn <- readIORef filenameIORef
    case mb_hecs of
      Nothing   -> return ()
      Just hecs -> do
        (w, h) <- widgetGetSize timelineDrawingArea
        traces    <- getViewTraces state
        cursorpos <- getCursorLine state
        let params = ViewParameters w h traces hadj_value scaleValue 1 False
                                    False
        let r = renderTraces state params traces hecs (Rectangle 0 0 w h)
        withPDFSurface (fn++".pdf") (fromIntegral w) (fromIntegral h) (flip renderWith $ r)
        return ()

-------------------------------------------------------------------------------
