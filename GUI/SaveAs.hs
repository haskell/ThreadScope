module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPDF file hecs viewParams =

    withPDFSurface file w' h' $ \surface ->
      renderWith surface $
        -- TODO: renderYLabelsAndAxis vadj_value showLabels traces
        -- move_to (100, 0)
        renderTraces viewParams hecs (Rectangle 0 0 w h)

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h

-------------------------------------------------------------------------------

saveAsPNG :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPNG file hecs viewParams =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $
        -- TODO
        renderTraces viewParams hecs (Rectangle 0 0 w h)
      surfaceWriteToPNG surface file

  where
    w = width  viewParams; w' = fromIntegral w
    h = height viewParams; h' = fromIntegral h

-------------------------------------------------------------------------------
