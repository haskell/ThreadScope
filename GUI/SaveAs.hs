module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces, renderLabelArea)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPDF file hecs viewParams =

    withPDFSurface file w' h' $ \surface ->
      renderWith surface $ do
        renderLabelArea viewParams hecs
        translate (fromIntegral label_area_width) 0
        renderTraces viewParams hecs (Rectangle 0 0 w h)
  where
    w = width  viewParams; w' = fromIntegral $ label_area_width + w
    h = height viewParams; h' = fromIntegral $ label_area_width + h
    -- TODO: take from the .ui file, from timeline_labels_drawingarea:
    label_area_width = 110

-------------------------------------------------------------------------------

saveAsPNG :: FilePath -> HECs -> ViewParameters -> IO ()
saveAsPNG file hecs viewParams =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $ do
        renderLabelArea viewParams hecs
        translate (fromIntegral label_area_width) 0
        renderTraces viewParams hecs (Rectangle 0 0 w h)
      surfaceWriteToPNG surface file
  where
    w = width  viewParams; w' = fromIntegral $ label_area_width + w
    h = height viewParams; h' = fromIntegral $ label_area_width + h
    -- TODO: take from the .ui file, from timeline_labels_drawingarea:
    label_area_width = 110

-------------------------------------------------------------------------------
