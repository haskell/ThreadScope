module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-- Imports from Haskell library
import System.FilePath

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPDF fn hecs viewParams =

    withPDFSurface (fn <.> "pdf") w' h' $ \surface ->
      renderWith surface $
        renderTraces viewParams hecs (Rectangle 0 0 w h)

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h

-------------------------------------------------------------------------------

saveAsPNG :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPNG fn hecs viewParams =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $
        renderTraces viewParams hecs (Rectangle 0 0 w h)
      surfaceWriteToPNG surface (fn <.> "png")

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h

-------------------------------------------------------------------------------
