{-# LANGUAGE CPP #-}
module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    histogramViewSetHECs,
    histogramViewSetInterval,
 ) where

import Events.HECs
import GUI.Timeline.Ticks (mu, deZero)
import GUI.Timeline.Render (renderYScaleArea, renderXScaleArea)
import GUI.Types

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Renderable as ChartR
import qualified Graphics.Rendering.Chart.Plot.Hidden as ChartH

import Data.Accessor
import Data.IORef
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.Foldable as Foldable

import Text.Printf

data HistogramView =
  HistogramView
  { hecsIORef :: IORef (Maybe HECs)
  , intervalIORef :: IORef (Maybe Interval)
  , histogramDrawingArea :: DrawingArea
  }

type Interval = (Timestamp, Timestamp)

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
      paramsHack size = ViewParameters  -- TODO: a hack
        { width = ceiling (fst size)
        , height = undefined
        , viewTraces = [SparkPoolHEC 99]  -- TODO
        , hadjValue = 0
        , scaleValue = 1
        , maxSpkValue = snd size
        , detail = undefined
        , bwMode = undefined
        , labelsMode = False
        }
      renderHist hecs minterval size = do
        let params = paramsHack size
            drawHist = renderViewHistogram hecs minterval size
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

renderViewHistogram :: HECs -> Maybe Interval -> (Double, Double)
                       -> C.Render Bool
renderViewHistogram hecs minterval size =
  let intDoub :: Integral a => a -> Double
      intDoub = fromIntegral
      histo :: [(Int, Timestamp)] -> [(Int, Timestamp)]
      histo durs = IM.toList $ fromListWith' (+) durs
      inR :: Timestamp -> Bool
      inR = case minterval of
              Nothing -> const True
              Just (from, to) -> \ t -> t >= from && t <= to
      -- TODO: if xs is sorted, we can slightly optimize the filtering
      inRange :: [(Timestamp, Int, Timestamp)] -> [(Int, Timestamp)]
      inRange xs = [(logdur, dur)
                   | (start, logdur, dur) <- xs, inR start]
      -- TODO: factor out to module with helper stuff (mu, deZero, this)
      fromListWith' :: (a -> a -> a) -> [(IM.Key, a)] -> IM.IntMap a
      fromListWith' f xs =
        L.foldl' ins IM.empty xs
          where
            ins t (k,x) = IM.insertWith' f k x t
      plot xs =
        let layout = Chart.layout1_plots ^= [Left plot]
                   $ Chart.layout1_left_axis ^= yaxis
                   $ Chart.layout1_bottom_axis ^= xaxis
                   $ Chart.defaultLayout1 :: Chart.Layout1 Double Double
            yaxis  = Chart.laxis_title ^= ""
                   $ Chart.laxis_override ^= Chart.axis_labels ^: map override0
                   $ Chart.defaultLayoutAxis
            xaxis  = Chart.laxis_title ^= ""
                   $ Chart.laxis_override ^= Chart.axis_labels ^: map override0
                   $ Chart.defaultLayoutAxis
            ytitle = "Total duration (" ++ mu ++ "s)"
            xtitle = "Individual spark duration (" ++ mu ++ "s)"
            override0 d = [ (x, "") | (x, _) <- d]
            overrideX d = [ (x, deZero (printf "%.4f" (10 ** (x / 5) / 1000)))
                          | (x, _) <- d]  -- TODO: round it up before **
            plot = Chart.joinPlot plotBars plotHidden
            plotHidden =  -- to fix the x an y scales
              Chart.toPlot $ ChartH.PlotHidden
                [intDoub (minXHistogram hecs), intDoub (maxXHistogram hecs)]
                [0, intDoub (maxYHistogram hecs) / 1000]
            plotBars = Chart.plotBars bars
            bars = Chart.plot_bars_values ^= barvs $ Chart.defaultPlotBars
            barvs = [(intDoub t, [intDoub height / 1000])
                    | (t, height) <- histo $ inRange xs]
        in layout
      xs = durHistogram hecs
      renderable :: Chart.Renderable ()
      renderable = ChartR.toRenderable (plot xs)
-- TODO: translate 0 xScaleAreaHeight
  if null xs
    then return False  -- TODO: perhaps display "No data" in the tab?
    else ChartG.updateCanvas renderable historamDrawingArea
