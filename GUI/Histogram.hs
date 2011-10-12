module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    histogramViewSetHECs,
    histogramViewSetInterval,
 ) where

import Events.HECs
import GUI.Timeline.Render (renderYScaleArea, renderXScaleArea)
import GUI.Timeline.Sparks
import GUI.Types

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C

import Data.IORef

data HistogramView =
  HistogramView
  { hecsIORef :: IORef (Maybe HECs)
  , intervalIORef :: IORef (Maybe Interval)
  , histogramDrawingArea :: DrawingArea
  }

histogramViewSetHECs :: HistogramView -> Maybe HECs -> IO ()
histogramViewSetHECs HistogramView{..} mhecs = do
  writeIORef hecsIORef mhecs
  widgetQueueDraw histogramDrawingArea

histogramViewSetInterval :: HistogramView -> Maybe Interval -> IO ()
histogramViewSetInterval HistogramView{..} minterval = do
  writeIORef intervalIORef minterval
  widgetQueueDraw histogramDrawingArea

histogramViewNew :: Builder -> IO HistogramView
histogramViewNew builder = do
  let getWidget cast = builderGetObject builder cast
  histogramDrawingArea <- getWidget castToDrawingArea "histogram_drawingarea"
  timelineYScaleArea  <- getWidget castToDrawingArea "timeline_yscale_area"
  timelineXScaleArea  <- getWidget castToDrawingArea "timeline_xscale_area"
  (w, _) <- widgetGetSize timelineYScaleArea
  (_, h) <- widgetGetSize timelineXScaleArea
  let yScaleAreaWidth  = fromIntegral w
      xScaleAreaHeight = fromIntegral h
      paramsHack width = ViewParameters  -- TODO: a hack
        { width = ceiling width
        , height = undefined
        , viewTraces = [TraceHistogram]
        , hadjValue = 0
        , scaleValue = 1
        , maxSpkValue = undefined
        , detail = undefined
        , bwMode = undefined
        , labelsMode = False
        }
      renderHist hecs minterval size = do
        let params = paramsHack (fst size)
            drawHist = renderSparkHistogram hecs minterval size
            drawXScale = renderXScaleArea params hecs (ceiling xScaleAreaHeight)
            drawYScale = renderYScaleArea params hecs yScaleAreaWidth
        C.translate yScaleAreaWidth 0
        b <- drawHist
        if not b then return False else do
        C.translate 0 (snd size)
        drawXScale
        C.translate (-yScaleAreaWidth) (-(snd size))
        drawYScale
        return True

  hecsIORef <- newIORef Nothing
  intervalIORef <- newIORef Nothing

  -- Program the callback for the capability drawingArea
  on histogramDrawingArea exposeEvent $
     C.liftIO $ do
       (width, height) <- widgetGetSize histogramDrawingArea
       let size = (fromIntegral width  - yScaleAreaWidth,
                   fromIntegral height - xScaleAreaHeight)
       maybeEventArray <- readIORef hecsIORef
       minterval <- readIORef intervalIORef
       -- Check if an event trace has been loaded.
       case maybeEventArray of
         Nothing   -> return True
         Just hecs -> do
           win <- widgetGetDrawWindow histogramDrawingArea
           renderWithDrawable win (renderHist hecs minterval size)

  return HistogramView{..}
