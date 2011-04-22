module GUI.EventsWindow (
    EventsWindow,
    eventsWindowNew,
    eventsWindowSetVisibility,

    eventsWindowGetCursorLine,
    eventsWindowJumpToBeginning,
    eventsWindowJumpToEnd,
    eventsWindowJumpToPosition,
  ) where

import GUI.State (HECs(..))
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
       eventsCursorIORef  :: IORef (Maybe (Timestamp, Int)),
       eventsAdj          :: Adjustment,
       eventsDrawingArea  :: DrawingArea,
       eventsBox          :: Widget,

       --TODO: eliminate, these are **shared** not private IORefs !!
       -- Should instead have methods for updating the display state
       -- and events for when the cursor is changed. Let the interaction
       -- module hold the state.
       hecsIORef          :: IORef (Maybe HECs),
       cursorIORef        :: IORef Timestamp
     }

-------------------------------------------------------------------------------

eventsWindowSetVisibility :: EventsWindow -> Bool -> IO ()
eventsWindowSetVisibility sidebar visible =
  set (eventsBox sidebar) [ widgetVisible := visible ]

-------------------------------------------------------------------------------

eventsWindowNew :: Bool -> Builder
                -> IORef (Maybe HECs) -> IORef Timestamp --TODO: eliminate
                -> IO EventsWindow
eventsWindowNew debug builder hecsIORef cursorIORef = do

  let getWidget cast = builderGetObject builder cast
  eventsCursorIORef  <- newIORef Nothing
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

  on eventsDrawingArea configureEvent $ eventsWindowResize eventsWin

  on eventsDrawingArea exposeEvent $ updateEventsWindow eventsWin debug

  on eventsDrawingArea buttonPressEvent $ tryEvent $ do
      (_,y)  <- eventCoordinates
      liftIO $ do
        widgetGrabFocus eventsDrawingArea
        setCursor eventsWin y

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

eventsWindowResize :: EventsWindow -> EventM EConfigure Bool
eventsWindowResize EventsWindow{..} = liftIO $ do
  (_,h) <- widgetGetSize eventsDrawingArea
  let exts = eventsFontExtents
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

updateEventsWindow :: EventsWindow -> Bool-> EventM EExpose Bool
updateEventsWindow eventsWin@EventsWindow{..} debug = liftIO $ do
  value <- adjustmentGetValue eventsAdj
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return True
    Just hecs -> do
      let arr = hecEventArray hecs
      win <- widgetGetDrawWindow eventsDrawingArea
      (w,h) <- widgetGetSize eventsDrawingArea

      cursorpos <- eventsWindowGetCursorLine eventsWin
      when debug $ printf "cursorpos: %d\n" cursorpos
      renderWithDrawable win $ do
        drawEvents value arr w h cursorpos
      return True

-------------------------------------------------------------------------------

eventsWindowGetCursorLine :: EventsWindow -> IO Int
eventsWindowGetCursorLine EventsWindow{..} = do
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

setCursor :: EventsWindow -> Double -> IO ()
setCursor EventsWindow{..} eventY = do
  val <- adjustmentGetValue eventsAdj
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> return ()
    Just hecs -> do
      let arr = hecEventArray hecs
          line'   = truncate (val + eventY / fontExtentsHeight eventsFontExtents)
          arr_max = snd $ bounds arr
          line    = if line' > arr_max then arr_max else line'
          t       = time (ce_event (arr!line))
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
