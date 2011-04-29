module GUI.EventsWindow (
    EventsWindow,
    eventsWindowNew,

    eventsWindowSetEvents,
    eventsWindowSetVisibility,
    eventsWindowGetCursorLine,
    eventsWindowJumpToBeginning,
    eventsWindowJumpToEnd,
    eventsWindowJumpToPosition,
  ) where

import GUI.ViewerColours

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import GHC.RTS.Events as GHC

import Control.Monad.Reader
import Data.Array
import Data.IORef
import Text.Printf

-------------------------------------------------------------------------------

data EventsWindow = EventsWindow {
       eventsFontExtents  :: FontExtents,
       eventsAdj          :: Adjustment,
       eventsDrawingArea  :: DrawingArea,
       eventsBox          :: Widget,

       eventsCursorIORef  :: IORef (Maybe (Timestamp, Int)),
       eventsIORef        :: IORef (Maybe (Array Int CapEvent))
     }

-------------------------------------------------------------------------------

eventsWindowSetVisibility :: EventsWindow -> Bool -> IO ()
eventsWindowSetVisibility eventWin visible =
  set (eventsBox eventWin) [ widgetVisible := visible ]

-------------------------------------------------------------------------------

eventsWindowSetEvents :: EventsWindow -> Maybe (Array Int GHC.CapEvent) -> IO ()
eventsWindowSetEvents eventWin@EventsWindow{..} mevents = do
  writeIORef eventsIORef mevents
  writeIORef eventsCursorIORef Nothing
  eventsWindowResize eventWin
  widgetQueueDraw eventsDrawingArea

-------------------------------------------------------------------------------

eventsWindowNew :: Builder -> IO EventsWindow
eventsWindowNew builder = do

  eventsIORef        <- newIORef Nothing
  eventsCursorIORef  <- newIORef Nothing

  let getWidget cast = builderGetObject builder cast
  eventsBox          <- getWidget castToWidget "eventsbox"
  eventsVScrollbar   <- getWidget castToVScrollbar "eventsVScroll"
  eventsAdj          <- rangeGetAdjustment eventsVScrollbar
  eventsDrawingArea  <- getWidget castToDrawingArea "eventsDrawingArea"

  -- make the background white
  widgetModifyBg eventsDrawingArea StateNormal (Color 0xffff 0xffff 0xffff)

  adjustmentSetLower         eventsAdj 0
  adjustmentSetStepIncrement eventsAdj 4

  widgetSetCanFocus eventsDrawingArea True

  eventsFontExtents <- withImageSurface FormatARGB32 0 0 $ \s ->
                         renderWith s eventsFont

  let eventsWin = EventsWindow {..}

  on eventsDrawingArea exposeEvent $ updateEventsWindow eventsWin

  on eventsDrawingArea buttonPressEvent $ tryEvent $ do
      (_,y)  <- eventCoordinates
      liftIO $ do
        widgetGrabFocus eventsDrawingArea
        setCursor eventsWin y

  on eventsDrawingArea scrollEvent $ do
      dir <- eventScrollDirection
      liftIO $ do
        val  <- adjustmentGetValue eventsAdj
        upper    <- adjustmentGetUpper eventsAdj
        pagesize <- adjustmentGetPageSize eventsAdj
        step <- adjustmentGetStepIncrement eventsAdj
        case dir of
           ScrollUp   -> set eventsAdj [ adjustmentValue := val - step ]
           ScrollDown -> set eventsAdj [ adjustmentValue := min (val + step) (upper - pagesize) ]
           _          -> return ()
        return True

  onValueChanged eventsAdj $
     widgetQueueDraw eventsDrawingArea

  return eventsWin

-------------------------------------------------------------------------------

--TODO: amagamate the following into one function eventsWindowSetCursorLine

eventsWindowJumpToBeginning :: EventsWindow -> IO ()
eventsWindowJumpToBeginning EventsWindow{..} =
  set eventsAdj [ adjustmentValue := 0 ]

eventsWindowJumpToEnd :: EventsWindow -> IO ()
eventsWindowJumpToEnd EventsWindow{..} = do
  upper    <- adjustmentGetUpper eventsAdj
  pagesize <- adjustmentGetPageSize eventsAdj
  let newval = max 0 (upper - pagesize)
  set eventsAdj [ adjustmentValue := newval ]

eventsWindowJumpToPosition :: EventsWindow -> Int -> IO ()
eventsWindowJumpToPosition EventsWindow{..} pos = do
  pagesize  <- adjustmentGetPageSize eventsAdj
  let newval = max 0 (pos - round pagesize `quot` 2)
  set eventsAdj [ adjustmentValue := fromIntegral newval ]


-------------------------------------------------------------------------------

eventsWindowResize :: EventsWindow -> IO ()
eventsWindowResize EventsWindow{..} = do
  (_,h) <- widgetGetSize eventsDrawingArea
  let exts = eventsFontExtents
  let page = fromIntegral (truncate (fromIntegral h / fontExtentsHeight exts))
  mb_events <- readIORef eventsIORef
  case mb_events of
    Nothing  -> return ()
    Just arr -> do
      let (_, n_events) = bounds arr
      adjustmentSetPageIncrement eventsAdj page
      adjustmentSetPageSize eventsAdj page
      adjustmentSetUpper eventsAdj (fromIntegral n_events + 1)
      -- printf "eventsWindowResize: %f" page
      return ()

-------------------------------------------------------------------------------

updateEventsWindow :: EventsWindow -> EventM EExpose Bool
updateEventsWindow eventsWin@EventsWindow{..} = liftIO $ do
  value <- adjustmentGetValue eventsAdj
  mb_events <- readIORef eventsIORef
  case mb_events of
    Nothing  -> return True
    Just arr -> do
      win <- widgetGetDrawWindow eventsDrawingArea
      (w,h) <- widgetGetSize eventsDrawingArea

      cursorpos <- eventsWindowGetCursorLine eventsWin
      renderWithDrawable win $ do
        drawEvents value arr w h cursorpos
      return True

-------------------------------------------------------------------------------

-- | Locate the cursor position as a line number
--
eventsWindowGetCursorLine :: EventsWindow -> IO Int
eventsWindowGetCursorLine EventsWindow{..} = do
  cursor <- readIORef eventsCursorIORef
  case cursor of
    Nothing                       -> return 0
    Just (_cursorTime, cursorPos) -> return cursorPos

-------------------------------------------------------------------------------

setCursor :: EventsWindow -> Double -> IO ()
setCursor EventsWindow{..} eventY = do
  val <- adjustmentGetValue eventsAdj
  mb_events <- readIORef eventsIORef
  case mb_events of
    Nothing  -> return ()
    Just arr -> do
      let line'   = truncate (val + eventY / fontExtentsHeight eventsFontExtents)
          arr_max = snd $ bounds arr
          line    = if line' > arr_max then arr_max else line'
          t       = time (ce_event (arr!line))
      --
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

-------------------------------------------------------------------------------

drawEvents :: Double -> Array Int GHC.CapEvent -> Int -> Int -> Int -> Render ()
drawEvents value arr width height cursor = do
  let val = truncate value :: Int
  exts <- eventsFont
  let h = fontExtentsHeight exts
      (_, upper) = bounds arr
      lines = ceiling (fromIntegral height / h)
      end = min upper (val + lines)

      draw y ev = do moveTo 0 y; showText (ppEvent' ev)

  zipWithM_ draw [ h, h*2 .. ] [ arr ! n | n <- [ val .. end ] ]

  when (val <= cursor && cursor <= end) $ do
    setLineWidth 3
    setOperator OperatorOver
    setSourceRGBAhex blue 1.0
    let cursory = fromIntegral (cursor - val) * h + 3
    moveTo 0                    cursory
    lineTo (fromIntegral width) cursory
    stroke

-------------------------------------------------------------------------------


ppEvent' :: CapEvent -> String
ppEvent' (CapEvent cap (GHC.Event time spec)) =
  printf "%9d: " time ++
  (case cap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent{ ref=ref } ->
      printf "unknown event; %d" ref

    Message msg -> msg
    UserMessage msg -> msg

    _other -> showEventTypeSpecificInfo spec
