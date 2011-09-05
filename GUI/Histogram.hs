module GUI.Histogram (
    HistogramView,
    histogramViewNew,
    histogramViewSetHECs
 ) where

import Events.HECs
import GUI.Timeline.Ticks (mu)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo as C

import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Renderable as ChartR
import qualified Graphics.Rendering.Chart.Gtk as ChartG

import Data.Accessor
import Data.IORef
import qualified Data.IntMap as IM

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

-- TODO: will it be needed?
histogramViewSetInterval :: HistogramView -> Maybe Interval -> IO ()
histogramViewSetInterval HistogramView{..} minterval = do
  writeIORef intervalIORef minterval
  widgetQueueDraw histogramDrawingArea

histogramViewNew :: Builder -> IO HistogramView
histogramViewNew builder = do
  let getWidget cast = builderGetObject builder cast
  histogramDrawingArea <- getWidget castToDrawingArea "histogram_drawingarea"

  hecsIORef <- newIORef Nothing
  intervalIORef <- newIORef Nothing
  let histogramView = HistogramView{..}

  -- Program the callback for the capability drawingArea
  on histogramDrawingArea exposeEvent $ do
     liftIO $ do
       maybeEventArray <- readIORef hecsIORef
       minterval <- readIORef intervalIORef
       -- Check if an event trace has been loaded.
       case maybeEventArray of
         Nothing   -> return True
         Just hecs ->
           renderViewHistogram histogramDrawingArea hecs minterval

  return histogramView

renderViewHistogram :: DrawingArea -> HECs -> Maybe Interval
                       -> IO Bool
renderViewHistogram historamDrawingArea hecs minterval = do
  let intDoub :: Integral a => a -> Double
      intDoub = fromIntegral
      histo :: [(Int, Timestamp)] -> [(Int, Timestamp)]
      histo durs = IM.toList $ IM.fromListWith (+) durs
      inR :: Timestamp -> Bool
      inR = case minterval of
              Nothing -> const True
              Just (from, to) -> \ t -> t >= from && t <= to
      -- TODO: if xs is sorted, we can slightly optimize the filtering
      inRange :: [(Timestamp, Int, Timestamp)] -> [(Int, Timestamp)]
      inRange xs = [(logdur, dur)
                   | (start, logdur, dur) <- xs, inR start, logdur > 0]

      plot xs =
        let layout = Chart.layout1_plots ^= [ Left (Chart.plotBars bars) ]
                   $ Chart.layout1_left_axis ^= yaxis
                   $ Chart.layout1_bottom_axis ^= xaxis
                   $ Chart.defaultLayout1 :: Chart.Layout1 Double Double
            yaxis  = Chart.laxis_title ^= ytitle
                   $ Chart.defaultLayoutAxis
            xaxis  = Chart.laxis_title ^= xtitle
                   $ Chart.laxis_override ^= Chart.axis_labels ^: map override
                   $ Chart.defaultLayoutAxis
            ytitle = "Total duration (" ++ mu ++ "s)"
            xtitle = "Individual spark duration (" ++ mu ++ "s)"
            override d = [(x, show (10 ** (x / 5) / 1000)) | (x, _) <- d]
            bars = Chart.plot_bars_values ^= barvs
                   $ Chart.defaultPlotBars
            barvs = [(intDoub t, [intDoub height / 1000])
                    | (t, height) <- hs]
            hs = histo (inRange xs)
        in layout
      renderable = ChartR.toRenderable (plot (durHistogram hecs))
  ChartG.updateCanvas renderable historamDrawingArea
