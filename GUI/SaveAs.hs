module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces, renderLabelArea)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import GUI.ViewerColours

-------------------------------------------------------------------------------

saveAsPDF :: FilePath -> HECs -> ViewParameters -> Double -> IO ()
saveAsPDF file hecs viewParams label_area_width =

    withPDFSurface file w' h' $ \surface ->
      renderWith surface $ do
        -- TODO: minimal version should be
        -- renderLabelArea viewParams hecs
        -- translate label_area_width 0
        -- renderTraces viewParams hecs (Rectangle 0 0 w h)
        -- TODO: but renderTraces draws to the left of 0 (not seen on screen)
        -- Workaround plus extra features:
        translate label_area_width 0
        rectangle 0 0 (fromIntegral w) (fromIntegral h)
        setSourceRGBAhex white (1 - transparency)
        fill
        renderTraces viewParams hecs (Rectangle 0 0 w h)
        translate (-label_area_width) 0
        rectangle 0 0 label_area_width (fromIntegral h)
        setSourceRGBAhex gtkBorderGrey (1 - transparency)
        -- The operation is the workaround:
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
        renderLabelArea viewParams hecs label_area_width
  where
    w = width  viewParams; w' = fromIntegral $ ceiling label_area_width + w
    h = height viewParams; h' = fromIntegral $ ceiling label_area_width + h
    -- TODO: make this an option, so that the user can have WYSIWYG
    transparency = 1

-------------------------------------------------------------------------------

-- TODO: the same issues as above
saveAsPNG :: FilePath -> HECs -> ViewParameters -> Double -> IO ()
saveAsPNG file hecs viewParams label_area_width =

    withImageSurface FormatARGB32 w' h' $ \surface -> do
      renderWith surface $ do
        translate label_area_width 0
        rectangle 0 0 (fromIntegral w) (fromIntegral h)
        setSourceRGBAhex white (1 - transparency)
        fill
        renderTraces viewParams hecs (Rectangle 0 0 w h)
        translate (-label_area_width) 0
        rectangle 0 0 label_area_width (fromIntegral h)
        setSourceRGBAhex gtkBorderGrey (1 - transparency)
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
        renderLabelArea viewParams hecs label_area_width
      surfaceWriteToPNG surface file
  where
    w = width  viewParams; w' = fromIntegral $ ceiling label_area_width + w
    h = height viewParams; h' = fromIntegral $ ceiling label_area_width + h
    transparency = 1

-------------------------------------------------------------------------------
