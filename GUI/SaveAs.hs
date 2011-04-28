module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces)
import GUI.State (ViewerState, HECs, ViewParameters(..))

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-- Imports from Haskell library
import System.FilePath

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewerState -> ViewParameters -> IO ()
saveAsPDF fn hecs state viewParams =

    withPDFSurface (fn <.> "pdf") w' h' $ \surface ->
      renderWith surface $
        renderTraces state viewParams traces hecs (Rectangle 0 0 w h)

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h
    traces = viewTraces viewParams

-------------------------------------------------------------------------------

saveAsPNG :: FilePath -> HECs -> ViewerState -> ViewParameters -> IO ()
saveAsPNG fn hecs state viewParams =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $
        renderTraces state viewParams traces hecs (Rectangle 0 0 w h)
      surfaceWriteToPNG surface (fn <.> "png")

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h
    traces = viewTraces viewParams

-------------------------------------------------------------------------------
