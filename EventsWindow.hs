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
import Graphics.UI.Gtk.ModelView as New

import Control.Monad.Reader
import Data.Array
import Data.IORef
import Text.Printf

-------------------------------------------------------------------------------

setupEventsWindow :: ViewerState -> IO ()
setupEventsWindow state@ViewerState{..} = do

  -- make the background white
  widgetModifyBg eventsDrawingArea StateNormal (Color 0xffff 0xffff 0xffff)

  adj <- rangeGetAdjustment eventsVScrollbar
  adjustmentSetLower adj 0
  adjustmentSetStepIncrement adj 4

  widgetSetCanFocus eventsDrawingArea True

  on eventsDrawingArea configureEvent $ eventsWindowResize state

  on eventsDrawingArea exposeEvent $ updateEventsWindow state

  on eventsDrawingArea buttonPressEvent $ tryEvent $ do
      button <- eventButton
      (_,y)  <- eventCoordinates
      liftIO $ do
        widgetGrabFocus eventsDrawingArea
        setCursor state y

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

  onToolButtonClicked eventsFirstButton $ do
     putStrLn "eventsFirstButton"
     adjustmentSetValue adj 0

  onToolButtonClicked eventsLastButton $ do
     upper <- adjustmentGetUpper adj
     adjustmentSetValue adj upper

  onToolButtonClicked eventsHomeButton $ do
     cursorpos <- getCursorLine state
     page  <- adjustmentGetPageSize adj
     adjustmentSetValue adj (fromIntegral (max 0 (cursorpos - round page `quot` 2)))


  -- Button for adding the cursor position to the boomark list
  onToolButtonClicked addBookmarkButton  $ do
     when debug $ putStrLn "Add bookmark\n"
     cursorPos <- readIORef cursorIORef
     New.listStoreAppend bookmarkStore cursorPos
     return ()

  exts <- withImageSurface FormatARGB32 0 0 $ \s -> renderWith s eventsFont
  writeIORef eventsFontExtents exts

  return ()

-------------------------------------------------------------------------------

eventsWindowResize :: ViewerState -> EventM EConfigure Bool
eventsWindowResize state@ViewerState{..} = liftIO $ do
  (_,h) <- widgetGetSize eventsDrawingArea
  win <- widgetGetDrawWindow eventsDrawingArea
  exts <- readIORef eventsFontExtents
  let page = fromIntegral (truncate (fromIntegral h / fontExtentsHeight exts))
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return True
    Just hecs -> do
      let arr = hecEventArray hecs
      let (_, n_events) = bounds arr
      adjustmentSetPageIncrement eventsAdj page
      adjustmentSetPageSize eventsAdj page
      adjustmentSetUpper eventsAdj (fromIntegral n_events + 1)
      -- printf "eventsWindowResize: %f" page
      return True

-------------------------------------------------------------------------------

updateEventsWindow :: ViewerState -> EventM EExpose Bool
updateEventsWindow state@ViewerState{..} = liftIO $ do
  value <- adjustmentGetValue eventsAdj
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return True
    Just hecs -> do
      let arr = hecEventArray hecs
      win <- widgetGetDrawWindow eventsDrawingArea
      (w,h) <- widgetGetSize eventsDrawingArea
    
      cursorpos <- getCursorLine state
      when debug $ printf "cursorpos: %d\n" cursorpos
      renderWithDrawable win $ do
        drawEvents value arr w h cursorpos
      return True

-------------------------------------------------------------------------------

getCursorLine :: ViewerState -> IO Int
getCursorLine state@ViewerState{..} = do
  -- locate the cursor position as a line number
  current_cursor <- readIORef cursorIORef
  eventsCursor <- readIORef eventsCursorIORef
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return 0
    Just hecs -> do
      let arr = hecEventArray hecs
      case eventsCursor of
        Just (cursort, cursorpos) | cursort == current_cursor ->
              return cursorpos
        _other -> do
              let cursorpos = locateCursor arr current_cursor
              writeIORef eventsCursorIORef (Just (current_cursor, cursorpos))
              return cursorpos

-------------------------------------------------------------------------------
  
setCursor :: ViewerState -> Double -> IO ()
setCursor state@ViewerState{..} eventY = do
  val <- adjustmentGetValue eventsAdj
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return ()
    Just hecs -> do
      let arr = hecEventArray hecs
      exts <- readIORef eventsFontExtents
      let 
          line = truncate (val + eventY / fontExtentsHeight exts)
          t    = time (ce_event (arr!line))
      --
      writeIORef cursorIORef t
      writeIORef eventsCursorIORef (Just (t,line))
      widgetQueueDraw eventsDrawingArea

-- find the line that corresponds to the next event after the cursor
locateCursor :: Array Int GHC.CapEvent -> Timestamp -> Int
locateCursor arr cursor = search l (r+1)
  where
  (l,r) = bounds arr

  search !l !r
    | (r - l) <= 1  = if cursor > time (ce_event (arr!l)) then r else l
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
    let cursory = fromIntegral (cursor - val) * h + 3
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
    UserMessage msg -> msg

    _other -> showEventTypeSpecificInfo spec
