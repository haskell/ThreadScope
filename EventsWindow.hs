module EventsWindow (
    setupEventsWindow,
    updateEventsWindow,
    eventsWindowResize
  ) where

import ReadEvents (ViewerState(..))

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo 
import Graphics.UI.Gtk.Glade

import GHC.RTS.Events as GHC

import Control.Monad.Reader
import Data.Array
import Data.IORef
import Text.Printf

setupEventsWindow :: Bool -> ViewerState -> GladeXML -> IO ()
setupEventsWindow debug state xml = do
  drawingArea <- xmlGetWidget xml castToDrawingArea
		      "eventsDrawingArea"

  -- make the background white
  widgetModifyBg drawingArea StateNormal (Color 0xffff 0xffff 0xffff)

  events_sb <- xmlGetWidget xml castToVScrollbar
		      "eventsVScroll"

  adj <- rangeGetAdjustment events_sb
  adjustmentSetLower adj 0
  adjustmentSetStepIncrement adj 4

  widgetSetCanFocus drawingArea True

  on drawingArea configureEvent $ 
     eventsWindowResize state adj drawingArea

  on drawingArea exposeEvent $
     updateEventsWindow state adj drawingArea

  on drawingArea buttonPressEvent $ do
      button <- eventButton
      liftIO $ do
        when debug $ putStrLn ("button " ++ show button)
        widgetGrabFocus drawingArea
        return True

  on drawingArea focusInEvent $ liftIO $ do
     f <- get drawingArea widgetHasFocus
     when debug $ putStrLn ("focus in: " ++ show f)
--     set drawingArea [widgetHasFocus := True]
     return False

  on drawingArea focusOutEvent $ liftIO $ do
     f <- get drawingArea widgetHasFocus
     when debug $ putStrLn ("focus out: " ++ show f)
--     set drawingArea [widgetHasFocus := False]
     return False

  on drawingArea keyPressEvent $ do
      key <- eventKeyName
      when debug $ liftIO $ putStrLn ("key " ++ key)
      return True

  on drawingArea scrollEvent $ do
      dir <- eventScrollDirection
      liftIO $ do
        val  <- adjustmentGetValue adj
        step <- adjustmentGetStepIncrement adj
        case dir of
           ScrollUp   -> adjustmentSetValue adj (val - step)
           ScrollDown -> adjustmentSetValue adj (val + step)
           _          -> return ()
        return True

  onValueChanged adj $
     widgetQueueDraw drawingArea

  return ()


eventsWindowResize :: ViewerState -> Adjustment -> DrawingArea
		   -> EventM EConfigure Bool
eventsWindowResize state adj drawingArea = liftIO $ do
  (_,h) <- widgetGetSize drawingArea
  win <- widgetGetDrawWindow drawingArea
  exts <- renderWithDrawable win $ eventsFont
  let page = fromIntegral (truncate (fromIntegral h / fontExtentsHeight exts))
  arr <- readIORef (eventArrayIORef state)
  let (_, n_events) = bounds arr
  adjustmentSetPageIncrement adj page
  adjustmentSetPageSize adj page
  adjustmentSetUpper adj (fromIntegral n_events)
  -- printf "eventsWindowResize: %f" page
  return True

updateEventsWindow :: ViewerState -> Adjustment -> DrawingArea
		   -> EventM EExpose Bool
updateEventsWindow state adj drawingArea = liftIO $ do
  value <- adjustmentGetValue adj
  arr <- readIORef (eventArrayIORef state)
  win <- widgetGetDrawWindow drawingArea
  (w,h) <- widgetGetSize drawingArea
  renderWithDrawable win (drawEvents value arr w h)
  return True

eventsFont :: Render FontExtents
eventsFont = do
  selectFontFace "Monospace" FontSlantNormal FontWeightNormal
  setFontSize 12
  fontExtents

drawEvents :: Double -> Array Int GHC.CapEvent -> Int -> Int -> Render ()
drawEvents value arr _width height = do
  let val = truncate value :: Int
  exts <- eventsFont
  let h = fontExtentsHeight exts
      (_, upper) = bounds arr
      lines = ceiling (fromIntegral height / h)
      end = min upper (val + lines)

      draw y ev = do moveTo 0 y; showText (ppEvent ev)

  zipWithM_ draw [ h, h*2 .. ] [ arr ! n | n <- [ val .. end ] ]

ppEvent :: CapEvent -> String
ppEvent (CapEvent cap (GHC.Event ref time spec)) =
  printf "%9d: " time ++
  (case cap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent ->
      printf "unknown event; %d" ref

    Message msg -> msg

    _other ->
      case spec of
        Startup n_caps ->
          printf "startup: %d capabilities" n_caps
        EventBlock end_time cap _block_events ->
          printf "event block: cap %d, end time: %d\n" cap end_time
        CreateThread thread -> 
          printf "creating thread %d" thread
        RunThread thread -> 
          printf "running thread %d" thread
        StopThread thread status -> 
          printf "stopping thread %d (%s)" thread (showThreadStopStatus status)
        ThreadRunnable thread -> 
          printf "thread %d is runnable" thread
        MigrateThread thread newCap  -> 
          printf "migrating thread %d to cap %d" thread newCap
        RunSpark thread -> 
          printf "running a local spark (thread %d)" thread
        StealSpark thread victimCap -> 
          printf "thread %d stealing a spark from cap %d" thread victimCap 
        CreateSparkThread sparkThread -> 
          printf "creating spark thread %d" sparkThread
        Shutdown -> 
          printf "shutting down"
        WakeupThread thread otherCap -> 
          printf "waking up thread %d on cap %d" thread otherCap
        RequestSeqGC -> 
          printf "requesting sequential GC"
        RequestParGC -> 
          printf "requesting parallel GC"
        StartGC -> 
          printf "starting GC"
        EndGC -> 
          printf "finished GC"
	_ -> 
          printf "unknown event type"

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
showThreadStopStatus _              = "unknown thread status"
