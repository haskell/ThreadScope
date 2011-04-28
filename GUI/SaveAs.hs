module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render
import GUI.State
import GUI.Traces

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-- Imports from Haskell library
import Data.IORef
import System.FilePath

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> ViewerState -> IO ()
saveAsPDF fn state@ViewerState{..} = do
    scaleValue <- readIORef scaleIORef
    hadj_value0 <- adjustmentGetValue timelineAdj
    let hadj_value = toWholePixels scaleValue hadj_value0
    mb_hecs <- readIORef hecsIORef
    case mb_hecs of
      Nothing   -> return ()
      Just hecs -> do
        (w, h) <- widgetGetSize timelineDrawingArea
        traces    <- getViewTraces state
        let params = ViewParameters w h traces hadj_value scaleValue 1 False
                                    False
        let r = renderTraces state params traces hecs (Rectangle 0 0 w h)
        withPDFSurface (fn <.> "pdf") (fromIntegral w) (fromIntegral h) (flip renderWith $ r)
        return ()

-------------------------------------------------------------------------------

saveAsPNG :: FilePath -> ViewerState -> IO ()
saveAsPNG fn state@ViewerState{..} = do
    scaleValue <- readIORef scaleIORef
    hadj_value0 <- adjustmentGetValue timelineAdj
    let hadj_value = toWholePixels scaleValue hadj_value0
    mb_hecs <- readIORef hecsIORef
    case mb_hecs of
      Nothing   -> return ()
      Just hecs -> do
        (w, h) <- widgetGetSize timelineDrawingArea
        traces    <- getViewTraces state
        let params = ViewParameters w h traces hadj_value scaleValue 1 False
                                    False
        let r = renderTraces state params traces hecs (Rectangle 0 0 w h)
        withImageSurface FormatARGB32 (fromIntegral w) (fromIntegral h)
         $ \ surface ->
              do renderWith surface r
                 surfaceWriteToPNG surface (fn <.> "png")
        return ()

-------------------------------------------------------------------------------
