module GUI.SaveAs (saveAsPDF, saveAsPNG) where

-- Imports for ThreadScope
import GUI.Timeline.Render (renderTraces, renderYScaleArea)
import GUI.Timeline.Ticks (renderXScaleArea)
import GUI.Types
import Events.HECs

-- Imports for GTK
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

saveAs :: HECs -> ViewParameters -> Double -> Double -> (Int, Int, Render ())
saveAs hecs params yScaleAreaWidth xScaleAreaHeight =
  let w = width params
      h = height params
      w' = ceiling yScaleAreaWidth + w
      h' = ceiling xScaleAreaHeight + h
      drawTraces = renderTraces params hecs (Rectangle 0 0 w h)
      drawXScale = renderXScaleArea params hecs (ceiling xScaleAreaHeight)
      drawYScale = renderYScaleArea params hecs yScaleAreaWidth
      -- Functions renderTraces and renderXScaleArea draw to the left of 0
      -- which is not seen in the normal mode, but would be seen in export,
      -- so it has to be cleared before renderYScaleArea is written on top:
      clearLeftArea = do
        rectangle 0 0 yScaleAreaWidth (fromIntegral h')
        op <- getOperator
        setOperator OperatorClear
        fill
        setOperator op
      drawAll = do
        translate yScaleAreaWidth xScaleAreaHeight
        drawTraces
        translate 0 (-xScaleAreaHeight)
        drawXScale
        translate (-yScaleAreaWidth) 0
        clearLeftArea
        translate 0 xScaleAreaHeight
        drawYScale
  in (w', h', drawAll)

saveAsPDF :: FilePath -> HECs -> ViewParameters -> Double -> Double -> IO ()
saveAsPDF filename hecs params yScaleAreaWidth xScaleAreaHeight =
  let (w', h', drawAll) = saveAs hecs params yScaleAreaWidth xScaleAreaHeight
  in withPDFSurface filename (fromIntegral w') (fromIntegral h') $ \surface ->
       renderWith surface drawAll

saveAsPNG :: FilePath -> HECs -> ViewParameters -> Double -> Double -> IO ()
saveAsPNG filename hecs params yScaleAreaWidth xScaleAreaHeight =
  let (w', h', drawAll) = saveAs hecs params yScaleAreaWidth xScaleAreaHeight
  in withImageSurface FormatARGB32 w' h' $ \surface -> do
       renderWith surface drawAll
       surfaceWriteToPNG surface filename
