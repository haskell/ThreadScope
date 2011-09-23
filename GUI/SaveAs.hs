module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces, renderLabelArea, renderXScaleArea)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewParameters -> Double -> Double -> IO ()
saveAsPDF file hecs viewParams label_area_width xscale_area_height =

    withPDFSurface file w' h' $ \surface ->
      renderWith surface $ do
        translate label_area_width xscale_area_height
        renderTraces viewParams hecs (Rectangle 0 0 w h)
        translate 0 (-xscale_area_height)
        renderXScaleArea viewParams hecs (ceiling xscale_area_height)
        -- Functions renderTraces and renderXScaleArea draw to the left of 0
        -- which is not seen in the normal mode, but would be seen in export.
        -- Workaround:
        translate (-label_area_width) 0
        rectangle 0 0 label_area_width h'
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
        -- End of workaround.
        translate 0 xscale_area_height
        renderLabelArea viewParams hecs label_area_width
  where
    w = width  viewParams; w' = fromIntegral $ ceiling label_area_width + w
    h = height viewParams; h' = fromIntegral $ ceiling xscale_area_height + h

-------------------------------------------------------------------------------

-- TODO: factor out the common parts
saveAsPNG :: FilePath -> HECs -> ViewParameters -> Double -> Double -> IO ()
saveAsPNG file hecs viewParams label_area_width xscale_area_height =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $ do
        translate label_area_width xscale_area_height
        renderTraces viewParams hecs (Rectangle 0 0 w h)
        translate 0 (-xscale_area_height)
        renderXScaleArea viewParams hecs (ceiling xscale_area_height)
        -- Functions renderTraces and renderXScaleArea draw to the left of 0
        -- which is not seen in the normal mode, but would be seen in export.
        -- Workaround:
        translate (-label_area_width) 0
        rectangle 0 0 label_area_width (fromIntegral h')
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
        -- End of workaround.
        translate 0 xscale_area_height
        renderLabelArea viewParams hecs label_area_width
      surfaceWriteToPNG surface file
  where
    w = width  viewParams; w' = fromIntegral $ ceiling label_area_width + w
    h = height viewParams; h' = fromIntegral $ ceiling xscale_area_height + h

-------------------------------------------------------------------------------
