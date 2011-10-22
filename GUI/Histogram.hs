module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    histogramViewSetHECs,
    histogramViewSetInterval,
 ) where

import Events.HECs
import GUI.Timeline.Render (renderTraces, renderYScaleArea)
import GUI.Timeline.Render.Constants
import GUI.Types

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C

import Data.IORef

data HistogramView =
  HistogramView
  { hecsIORef :: IORef (Maybe HECs)
  , mintervalIORef :: IORef (Maybe Interval)
  , histogramDrawingArea :: DrawingArea
  }

histogramViewSetHECs :: HistogramView -> Maybe HECs -> IO ()
histogramViewSetHECs HistogramView{..} mhecs = do
  writeIORef hecsIORef mhecs
  widgetQueueDraw histogramDrawingArea

histogramViewSetInterval :: HistogramView -> Maybe Interval -> IO ()
histogramViewSetInterval HistogramView{..} minterval = do
  writeIORef mintervalIORef minterval
  widgetQueueDraw histogramDrawingArea

histogramViewNew :: Builder -> IO HistogramView
histogramViewNew builder = do
  let getWidget cast = builderGetObject builder cast
  histogramDrawingArea <- getWidget castToDrawingArea "histogram_drawingarea"
  histogramYScaleArea <- getWidget castToDrawingArea "timeline_yscale_area2"
  timelineXScaleArea <- getWidget castToDrawingArea "timeline_xscale_area"
  (_, xh) <- widgetGetSize timelineXScaleArea
  let xScaleAreaHeight = fromIntegral xh
      traces = [TraceHistogram]
      paramsHist (w, h) minterval = ViewParameters
        { width = w
        , height = h
        , viewTraces = traces
        , hadjValue = 0
        , scaleValue = 1
        , maxSpkValue = undefined
        , detail = undefined
        , bwMode = undefined
        , labelsMode = False
        , histogramHeight = h - xScaleAreaHeight
        , minterval = minterval
        , xScaleAreaHeight = xScaleAreaHeight
        }

  hecsIORef <- newIORef Nothing
  mintervalIORef <- newIORef Nothing

  -- Program the callback for the capability drawingArea
  on histogramDrawingArea exposeEvent $
     C.liftIO $ do
       maybeEventArray <- readIORef hecsIORef
       case maybeEventArray of
         Nothing -> return False
         Just hecs
           | null (durHistogram hecs) -> return False
           | otherwise -> do
               win <- widgetGetDrawWindow histogramDrawingArea
               minterval <- readIORef mintervalIORef
               (w, windowHeight) <- widgetGetSize histogramDrawingArea
               let size = (w, windowHeight - firstTraceY)
                   params = paramsHist size minterval
                   rect = Rectangle 0 0 w (snd size)
               renderWithDrawable win $
                 renderTraces params hecs rect
               return True

  -- Redrawing labelDrawingArea
  histogramYScaleArea `onExpose` \_ -> do
    maybeEventArray <- readIORef hecsIORef
    case maybeEventArray of
      Nothing -> return False
      Just hecs
        | null (durHistogram hecs) -> return False
        | otherwise -> do
            win <- widgetGetDrawWindow histogramYScaleArea
            minterval <- readIORef mintervalIORef
            (xoffset, windowHeight) <- widgetGetSize histogramYScaleArea
            let size = (undefined, windowHeight - firstTraceY)
                params = paramsHist size minterval
            renderWithDrawable win $
              -- TODO: looks bad when h is not a multiple of 10
              renderYScaleArea params hecs (fromIntegral xoffset)
            return True

  return HistogramView{..}
