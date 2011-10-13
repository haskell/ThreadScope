module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    histogramViewSetHECs,
    histogramViewSetInterval,
 ) where

import Events.HECs
import GUI.Timeline.Render (renderTraces, drawYScaleArea)
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
  histogramYScaleArea <- getWidget castToDrawingArea "timeline_yscale_area2"
  timelineXScaleArea <- getWidget castToDrawingArea "timeline_xscale_area"
  (_, h) <- widgetGetSize timelineXScaleArea
  let xScaleAreaHeight = fromIntegral h
      traces = [TraceHistogram]
      paramsHack size = ViewParameters  -- TODO: create here properly and pass around
        { width = ceiling $ fst size
        , height = ceiling $ snd size
        , viewTraces = traces
        , hadjValue = 0
        , scaleValue = 1
        , maxSpkValue = undefined
        , detail = undefined
        , bwMode = undefined
        , labelsMode = False
        }
      renderHist hecs minterval size = do
        let params = paramsHack size
            rect = Rectangle 0 0 (ceiling $ fst size) (ceiling $ snd size)
        renderTraces params hecs rect  --renderSparkHistogram hecs minterval size xScaleAreaHeight

  hecsIORef <- newIORef Nothing
  intervalIORef <- newIORef Nothing

  -- Program the callback for the capability drawingArea
  on histogramDrawingArea exposeEvent $
     C.liftIO $ do
       (width, height) <- widgetGetSize histogramDrawingArea
       let size = (fromIntegral width, fromIntegral height)
       maybeEventArray <- readIORef hecsIORef
       minterval <- readIORef intervalIORef
       -- Check if an event trace has been loaded.
       case maybeEventArray of
         Nothing -> return True
         Just hecs
           | null (durHistogram hecs) -> return True
           | otherwise -> do
               win <- widgetGetDrawWindow histogramDrawingArea
               renderWithDrawable win (renderHist hecs minterval size)
               return True

  -- Redrawing labelDrawingArea
  histogramYScaleArea `onExpose` \_ -> do
    maybeEventArray <- readIORef hecsIORef
    case maybeEventArray of
      Nothing -> return False
      Just hecs
        | null (durHistogram hecs) -> return True
        | otherwise -> do
            let maxP = maxSparkPool hecs
                maxH = fromIntegral (maxYHistogram hecs) / 1000
                maxS = 0  -- TODO
                labelsMode = False
            win <- widgetGetDrawWindow histogramYScaleArea
            vadj_value <- return 0  -- TODO
            (xoffset, _) <- widgetGetSize histogramYScaleArea
            renderWithDrawable win $
              drawYScaleArea maxS maxP maxH (fromIntegral xoffset)
                vadj_value labelsMode traces
            return True

  return HistogramView{..}
