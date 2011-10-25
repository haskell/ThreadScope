module GUI.EventsView (
    EventsView,
    eventsViewNew,
    EventsViewActions(..),

    eventsViewSetEvents,

    eventsViewGetCursor,
    eventsViewSetCursor,
    eventsViewScrollToLine,
  ) where

import GHC.RTS.Events

import Graphics.UI.Gtk
import qualified GUI.GtkExtras as GtkExt

import Control.Monad.Reader
import Data.Array
import Data.IORef
import Numeric

-------------------------------------------------------------------------------

data EventsView = EventsView {
       drawArea :: !Widget,
       adj      :: !Adjustment,
       stateRef :: !(IORef ViewState)
     }

data EventsViewActions = EventsViewActions {
       eventsViewCursorChanged :: Int -> IO ()
     }

data ViewState = ViewState {
       lineHeight  :: !Double,
       eventsState :: !EventsState
     }

data EventsState
   = EventsEmpty
   | EventsLoaded {
       cursorPos  :: !Int,
       mrange     :: !(Maybe (Int, Int)),
       eventsArr  :: Array Int CapEvent
     }

-------------------------------------------------------------------------------

eventsViewNew :: Builder -> EventsViewActions -> IO EventsView
eventsViewNew builder EventsViewActions{..} = do

  stateRef <- newIORef undefined

  let getWidget cast = builderGetObject builder cast
  drawArea     <- getWidget castToWidget "eventsDrawingArea"
  vScrollbar   <- getWidget castToVScrollbar "eventsVScroll"
  adj          <- get vScrollbar rangeAdjustment

  -- make the background white
  widgetModifyBg drawArea StateNormal (Color 0xffff 0xffff 0xffff)
  widgetSetCanFocus drawArea True
  --TODO: needs to be reset on each style change ^^

  -----------------------------------------------------------------------------
  -- Line height

  -- Calculate the height of each line based on the current font
  let getLineHeight = do
        pangoCtx <- widgetGetPangoContext drawArea
        fontDesc <- contextGetFontDescription pangoCtx
        metrics  <- contextGetMetrics pangoCtx fontDesc emptyLanguage
        return $ ascent metrics + descent metrics --TODO: padding?

  -- We cache the height of each line
  initialLineHeight <- getLineHeight
  -- but have to update it when the font changes
  on drawArea styleSet $ \_ -> do
    lineHeight' <- getLineHeight
    modifyIORef stateRef $ \viewstate -> viewstate { lineHeight = lineHeight' }

  -----------------------------------------------------------------------------

  writeIORef stateRef ViewState {
    lineHeight  = initialLineHeight,
    eventsState = EventsEmpty
  }

  let eventsView = EventsView {..}

  -----------------------------------------------------------------------------
  -- Drawing

  on drawArea exposeEvent $ liftIO $ do
    drawEvents eventsView =<< readIORef stateRef
    return True

  -----------------------------------------------------------------------------
  -- Key navigation

  on drawArea keyPressEvent $ do
    let scroll by = liftIO $ do
          ViewState{eventsState, lineHeight} <- readIORef stateRef
          pagesize <- get adj adjustmentPageSize
          let pagejump = max 1 (truncate (pagesize / lineHeight) - 1)
          case eventsState of
            EventsEmpty                        -> return ()
            EventsLoaded{cursorPos, eventsArr} ->
                eventsViewCursorChanged cursorPos'
              where
                cursorPos'    = clampBounds range (by pagejump end cursorPos)
                range@(_,end) = bounds eventsArr
          return True

    key <- eventKeyName
    case key of
      "Up"        -> scroll (\_page _end  pos -> pos-1)
      "Down"      -> scroll (\_page _end  pos -> pos+1)
      "Page_Up"   -> scroll (\ page _end  pos -> pos-page)
      "Page_Down" -> scroll (\ page _end  pos -> pos+page)
      "Home"      -> scroll (\_page _end _pos -> 0)
      "End"       -> scroll (\_page  end _pos -> end)
      "Left"      -> return True
      "Right"     -> return True
      _           -> return False

  -----------------------------------------------------------------------------
  -- Scrolling

  set adj [ adjustmentLower := 0 ]

  on drawArea sizeAllocate $ \_ ->
    updateScrollAdjustment eventsView =<< readIORef stateRef

  let hitpointToLine :: ViewState -> Double -> Double -> Maybe Int
      hitpointToLine ViewState{eventsState = EventsEmpty} _ _  = Nothing
      hitpointToLine ViewState{eventsState = EventsLoaded{eventsArr}, lineHeight}
                     yOffset eventY
        | hitLine > maxIndex = Nothing
        | otherwise          = Just hitLine
        where
          hitLine  = truncate ((yOffset + eventY) / lineHeight)
          maxIndex = snd (bounds eventsArr)

  on drawArea buttonPressEvent $ tryEvent $ do
    (_,y)  <- eventCoordinates
    liftIO $ do
      viewState <- readIORef stateRef
      yOffset <- get adj adjustmentValue
      widgetGrabFocus drawArea
      case hitpointToLine viewState yOffset y of
        Nothing -> return ()
        Just n  -> eventsViewCursorChanged n

  on drawArea scrollEvent $ do
    dir <- eventScrollDirection
    liftIO $ do
      val      <- get adj adjustmentValue
      upper    <- get adj adjustmentUpper
      pagesize <- get adj adjustmentPageSize
      step     <- get adj adjustmentStepIncrement
      case dir of
        ScrollUp   -> set adj [ adjustmentValue := val - step ]
        ScrollDown -> set adj [ adjustmentValue := min (val + step)
                                                       (upper - pagesize) ]
        _          -> return ()
    return True

  onValueChanged adj $
    widgetQueueDraw drawArea

  -----------------------------------------------------------------------------

  return eventsView

-------------------------------------------------------------------------------

eventsViewSetEvents :: EventsView -> Maybe (Array Int CapEvent) -> IO ()
eventsViewSetEvents eventWin@EventsView{drawArea, stateRef} mevents = do
  viewState <- readIORef stateRef
  let eventsState' = case mevents of
        Nothing     -> EventsEmpty
        Just events -> EventsLoaded {
                          cursorPos  = 0,
                          mrange = Nothing,
                          eventsArr  = events
                       }
      viewState' = viewState { eventsState = eventsState' }
  writeIORef stateRef viewState'
  updateScrollAdjustment eventWin viewState'
  widgetQueueDraw drawArea

-------------------------------------------------------------------------------

eventsViewGetCursor :: EventsView -> IO (Maybe Int)
eventsViewGetCursor EventsView{stateRef} = do
  ViewState{eventsState} <- readIORef stateRef
  case eventsState of
    EventsEmpty             -> return Nothing
    EventsLoaded{cursorPos} -> return (Just cursorPos)

eventsViewSetCursor :: EventsView -> Int -> Maybe (Int, Int) -> IO ()
eventsViewSetCursor eventsView@EventsView{drawArea, stateRef} n mrange = do
  viewState@ViewState{eventsState} <- readIORef stateRef
  case eventsState of
    EventsEmpty             -> return ()
    EventsLoaded{eventsArr} -> do
      let n' = clampBounds (bounds eventsArr) n
      writeIORef stateRef viewState {
        eventsState = eventsState { cursorPos = n', mrange }
      }
      eventsViewScrollToLine eventsView  n'
      widgetQueueDraw drawArea

eventsViewScrollToLine :: EventsView -> Int -> IO ()
eventsViewScrollToLine EventsView{adj, stateRef} n = do
  ViewState{lineHeight} <- readIORef stateRef
  -- make sure that the range [n..n+1] is within the current page:
  adjustmentClampPage adj
    (fromIntegral  n    * lineHeight)
    (fromIntegral (n+1) * lineHeight)

-------------------------------------------------------------------------------

updateScrollAdjustment :: EventsView -> ViewState -> IO ()
updateScrollAdjustment EventsView{drawArea, adj}
                       ViewState{lineHeight, eventsState} = do

  (_,windowHeight) <- widgetGetSize drawArea
  let numLines = case eventsState of
                   EventsEmpty             -> 0
                   EventsLoaded{eventsArr} -> snd (bounds eventsArr) + 1
      linesHeight = fromIntegral numLines * lineHeight
      upper       = max linesHeight (fromIntegral windowHeight)
      pagesize    = fromIntegral windowHeight

  set adj [
       adjustmentUpper         := upper,
       adjustmentPageSize      := pagesize,
       adjustmentStepIncrement := pagesize * 0.2,
       adjustmentPageIncrement := pagesize * 0.9
    ]
  val <- get adj adjustmentValue
  when (val > upper - pagesize) $
    set adj [ adjustmentValue := max 0 (upper - pagesize) ]

-------------------------------------------------------------------------------

drawEvents :: EventsView -> ViewState -> IO ()
drawEvents _ ViewState {eventsState = EventsEmpty} = return ()
drawEvents EventsView{drawArea, adj}
           ViewState {lineHeight, eventsState = EventsLoaded{..}} = do

  yOffset    <- get adj adjustmentValue
  pageSize   <- get adj adjustmentPageSize

  -- calculate which lines are visible
  let lower = truncate (yOffset / lineHeight)
      upper = ceiling ((yOffset + pageSize) / lineHeight)

      -- the array indexes [begin..end] inclusive
      -- are partially or fully visible
      begin = lower
      end   = min upper (snd (bounds eventsArr))

  win   <- widgetGetDrawWindow drawArea
  style <- get drawArea widgetStyle
  focused <- get drawArea widgetIsFocus
  let state | focused   = StateSelected
            | otherwise = StateActive

  pangoCtx <- widgetGetPangoContext drawArea
  layout   <- layoutEmpty pangoCtx
  layoutSetEllipsize layout EllipsizeEnd

  (width,clipHeight) <- widgetGetSize drawArea
  let clipRect = Rectangle 0 0 width clipHeight

  let -- With average char width, timeWidth is enough for 24 hours of logs
      -- (way more than TS can handle, currently). Aligns nicely with
      -- current timeline_yscale_area width, too.
      -- TODO: take timeWidth from the yScaleDrawingArea width
      -- TODO: perhaps make the timeWidth area grey, too?
      -- TODO: perhaps limit scroll to the selected interval (perhaps not strictly, but only so that the interval area does not completely vanish from the visible area)?
      timeWidth  = 105
      columnGap  = 20
      descrWidth = width - timeWidth - columnGap

  sequence_
    [ do when (inside || selected) $
           GtkExt.stylePaintFlatBox
             style win
             state1 ShadowNone
             clipRect
             drawArea ""
             0 (round y) width (round lineHeight)

         -- The event time
         layoutSetText layout (showEventTime event)
         layoutSetAlignment layout AlignRight
         layoutSetWidth layout (Just (fromIntegral timeWidth))
         GtkExt.stylePaintLayout
           style win
           state2 True
           clipRect
           drawArea ""
           0 (round y)
           layout

         -- The event description text
         layoutSetText layout (showEventDescr event)
         layoutSetAlignment layout AlignLeft
         layoutSetWidth layout (Just (fromIntegral descrWidth))
         GtkExt.stylePaintLayout
           style win
           state2 True
           clipRect
           drawArea ""
           (timeWidth + columnGap) (round y)
           layout

    | n <- [begin..end]
    , let y = fromIntegral n * lineHeight - yOffset
          event    = eventsArr ! n
          inside   = maybe False (\ (s, e) -> s <= n && n <= e) mrange
          selected = cursorPos == n
          (state1, state2)
            | inside    = (StatePrelight, StatePrelight)
            | selected  = (state, state)
            | otherwise = (state, StateNormal)
    ]

  where
    showEventTime  (CapEvent _cap (Event  time _spec)) =
      showFFloat (Just 6) (fromIntegral time / 1000000) "s"
    showEventDescr (CapEvent  cap (Event _time  spec)) =
        (case cap of
          Nothing -> ""
          Just c  -> "HEC " ++ show c ++ ": ")
     ++ case spec of
          UnknownEvent{ref} -> "unknown event; " ++ show ref
          Message     msg   -> msg
          UserMessage msg   -> msg
          _                 -> showEventInfo spec

-------------------------------------------------------------------------------

clampBounds :: Ord a => (a, a) -> a -> a
clampBounds (lower, upper) x
  | x <= lower = lower
  | x >  upper = upper
  | otherwise  = x
