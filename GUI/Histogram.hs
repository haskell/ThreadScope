{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import qualified GUI.GtkExtras as GtkExt

import Data.IORef
import Control.Monad.Trans

data HistogramView =
  HistogramView
  { hecsIORef            :: IORef (Maybe HECs)
  , mintervalIORef       :: IORef (Maybe Interval)
  , histogramDrawingArea :: DrawingArea
  , histogramYScaleArea  :: DrawingArea
  }

histogramViewSetHECs :: HistogramView -> Maybe HECs -> IO ()
histogramViewSetHECs HistogramView{..} mhecs = do
  writeIORef hecsIORef mhecs
  writeIORef mintervalIORef Nothing  -- the old interval may make no sense
  widgetQueueDraw histogramDrawingArea
  widgetQueueDraw histogramYScaleArea

histogramViewSetInterval :: HistogramView -> Maybe Interval -> IO ()
histogramViewSetInterval HistogramView{..} minterval = do
  writeIORef mintervalIORef minterval
  widgetQueueDraw histogramDrawingArea
  widgetQueueDraw histogramYScaleArea

histogramViewNew :: Builder -> IO HistogramView
histogramViewNew builder = do
  let getWidget cast = builderGetObject builder cast
  histogramDrawingArea <- getWidget castToDrawingArea "histogram_drawingarea"
  histogramYScaleArea <- getWidget castToDrawingArea "timeline_yscale_area2"
  timelineXScaleArea <- getWidget castToDrawingArea "timeline_xscale_area"

  -- HACK: layoutSetAttributes does not work for \mu, so let's work around
  fd <- fontDescriptionNew
  fontDescriptionSetSize fd 8
  fontDescriptionSetFamily fd "sans serif"
  widgetModifyFont histogramYScaleArea (Just fd)

  Rectangle _ _ _ xh <- widgetGetAllocation timelineXScaleArea
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
        , histogramHeight = h - histXScaleHeight
        , minterval = minterval
        , xScaleAreaHeight = xScaleAreaHeight
        }

  hecsIORef <- newIORef Nothing
  mintervalIORef <- newIORef Nothing

  pangoCtx <- widgetGetPangoContext histogramDrawingArea
  style    <- get histogramDrawingArea widgetStyle
  layout   <- layoutEmpty pangoCtx
  (_ :: String) <- layoutSetMarkup layout $
    "No detailed spark events in this eventlog.\n"
    ++ "Re-run with <tt>+RTS -lf</tt> to generate them."

  -- Program the callback for the capability drawingArea
  on histogramDrawingArea draw $
     C.liftIO $ do
       maybeEventArray <- readIORef hecsIORef
       -- TODO: get rid of Just
       Just win <- widgetGetWindow histogramDrawingArea
       Rectangle _ _ w windowHeight <- widgetGetAllocation histogramDrawingArea
       case maybeEventArray of
         Nothing -> return ()
         Just hecs
           | null (durHistogram hecs) -> do
               renderWithDrawWindow win $ do
                 C.moveTo 4 20
                 showLayout layout
               return ()
           | otherwise -> do
               minterval <- readIORef mintervalIORef
               if windowHeight < 80
                 then return ()
                 else do
                   let size = (w, windowHeight - firstTraceY)
                       params = paramsHist size minterval
                       rect = Rectangle 0 0 w (snd size)
                   renderWithDrawWindow win $
                     renderTraces params hecs rect
                   return ()

  -- Redrawing histogramYScaleArea
  histogramYScaleArea `on` draw $ liftIO $ do
    maybeEventArray <- readIORef hecsIORef
    case maybeEventArray of
      Nothing -> return ()
      Just hecs
        | null (durHistogram hecs) -> return ()
        | otherwise -> do
            -- TODO: get rid of Just
            Just win <- widgetGetWindow histogramYScaleArea
            minterval <- readIORef mintervalIORef
            Rectangle _ _ _ windowHeight <- widgetGetAllocation histogramYScaleArea
            if windowHeight < 80
              then return ()
              else do
                let size = (undefined, windowHeight - firstTraceY)
                    params = paramsHist size minterval
                renderWithDrawWindow win $
                  renderYScaleArea params hecs histogramYScaleArea
                return ()

  return HistogramView{..}
