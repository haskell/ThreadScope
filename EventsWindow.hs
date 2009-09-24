{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
module EventsWindow (
    setupEventsWindow,
    updateEventsWindow,
    eventsWindowResize
  ) where

import State
import ViewerColours

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo 

import GHC.RTS.Events as GHC

import Control.Monad.Reader
import Data.Array
import Data.IORef
import Text.Printf

setupEventsWindow :: ViewerState -> IO ()
setupEventsWindow state@ViewerState{..} = do

  -- make the background white
  widgetModifyBg eventsDrawingArea StateNormal (Color 0xffff 0xffff 0xffff)

  adj <- rangeGetAdjustment eventsVScrollbar
  adjustmentSetLower adj 0
  adjustmentSetStepIncrement adj 4

  widgetSetCanFocus eventsDrawingArea True

  on eventsDrawingArea configureEvent $ 
     eventsWindowResize state adj

  on eventsDrawingArea exposeEvent $
     updateEventsWindow state adj

  on eventsDrawingArea buttonPressEvent $ do
      button <- eventButton
      liftIO $ do
        when debug $ putStrLn ("button " ++ show button)
        widgetGrabFocus eventsDrawingArea
        return True

  on eventsDrawingArea focusInEvent $ liftIO $ do
     f <- get eventsDrawingArea widgetHasFocus
     when debug $ putStrLn ("focus in: " ++ show f)
--     set eventsDrawingArea [widgetHasFocus := True]
     return False

  on eventsDrawingArea focusOutEvent $ liftIO $ do
     f <- get eventsDrawingArea widgetHasFocus
     when debug $ putStrLn ("focus out: " ++ show f)
--     set eventsDrawingArea [widgetHasFocus := False]
     return False

  on eventsDrawingArea keyPressEvent $ do
      key <- eventKeyName
      when debug $ liftIO $ putStrLn ("key " ++ key)
      return True

  on eventsDrawingArea scrollEvent $ do
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
     widgetQueueDraw eventsDrawingArea

  return ()


eventsWindowResize :: ViewerState -> Adjustment
		   -> EventM EConfigure Bool
eventsWindowResize state@ViewerState{..} adj = liftIO $ do
  (_,h) <- widgetGetSize eventsDrawingArea
  win <- widgetGetDrawWindow eventsDrawingArea
  exts <- renderWithDrawable win $ eventsFont
  let page = fromIntegral (truncate (fromIntegral h / fontExtentsHeight exts))
  arr <- readIORef eventArrayIORef
  let (_, n_events) = bounds arr
  adjustmentSetPageIncrement adj page
  adjustmentSetPageSize adj page
  adjustmentSetUpper adj (fromIntegral n_events)
  -- printf "eventsWindowResize: %f" page
  return True

updateEventsWindow :: ViewerState -> Adjustment
		   -> EventM EExpose Bool
updateEventsWindow state@ViewerState{..} adj  = liftIO $ do
  value <- adjustmentGetValue adj
  arr <- readIORef eventArrayIORef
  win <- widgetGetDrawWindow eventsDrawingArea
  (w,h) <- widgetGetSize eventsDrawingArea
  cursor <- readIORef cursorIORef
  let cursorpos = locateCursor arr cursor
  when debug $ printf "cursorpos: %d\n" cursorpos
  renderWithDrawable win $ do
    drawEvents value arr w h cursorpos
  return True

locateCursor :: Array Int GHC.CapEvent -> Timestamp -> Int
locateCursor arr cursor = search l r
  where
  (l,r) = bounds arr

  search l r
    | r - l <= 1 = l
    | cursor < tmid = search l mid
    | otherwise     = search mid r
    where
    mid  = l + (r - l) `quot` 2
    tmid = time (ce_event (arr!mid))

eventsFont :: Render FontExtents
eventsFont = do
  selectFontFace "Monospace" FontSlantNormal FontWeightNormal
  setFontSize 12
  fontExtents

drawEvents :: Double -> Array Int GHC.CapEvent -> Int -> Int -> Int -> Render ()
drawEvents value arr width height cursor = do
  let val = truncate value :: Int
  exts <- eventsFont
  let h = fontExtentsHeight exts
      (_, upper) = bounds arr
      lines = ceiling (fromIntegral height / h)
      end = min upper (val + lines)

      draw y ev = do moveTo 0 y; showText (ppEvent ev)

  zipWithM_ draw [ h, h*2 .. ] [ arr ! n | n <- [ val .. end ] ]

  when (val <= cursor && cursor <= end) $ do
    setLineWidth 3
    setOperator OperatorOver
    setSourceRGBAhex blue 1.0
    let cursory = fromIntegral (cursor - val) * h
    moveTo 0                    cursory
    lineTo (fromIntegral width) cursory
    stroke


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
