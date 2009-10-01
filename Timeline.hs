module Timeline ( 
    setupTimelineView,
    updateTimelines,
    queueRedrawTimelines
 ) where

import Timeline.HEC
import State
import ViewerColours
import EventlogViewerCommon
import Key

import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Old
import Graphics.UI.Gtk.Gdk.EventM as New
import Graphics.Rendering.Cairo  as C

import Data.Maybe
import Data.IORef
import Control.Monad
import Text.Printf
import Debug.Trace

-----------------------------------------------------------------------------
-- The CPUs view

setupTimelineView :: ViewerState -> IO ()
setupTimelineView state@ViewerState{..} = do

  ------------------------------------------------------------------------
  -- Key presses
  onKeyPress mainWindow $ \Key { Old.eventKeyName = key, eventKeyChar = mch } -> do
    -- when debug $ putStrLn ("key " ++ key)
    case key of
      "Escape" -> mainQuit >> return True
      "Right"  -> do scrollRight state; return True
      "Left"   -> do scrollLeft  state; return True
      _ -> if isJust mch then
             case fromJust mch of 
               '+' -> do zoomIn  state; return True
               '-' -> do zoomOut state; return True
               _   -> return True
           else
             return True

  ------------------------------------------------------------------------
  -- Porgram the callback for the capability drawingArea
  timelineLabelDrawingArea `onExpose` updateCapabilityDrawingArea state

  ------------------------------------------------------------------------
  -- Set-up the key timelineDrawingArea.
  timelineKeyDrawingArea `onExpose` updateKeyDrawingArea timelineKeyDrawingArea

  ------------------------------------------------------------------------
  -- zoom buttons

  zoomInButton  `onToolButtonClicked` zoomIn    state
  zoomOutButton `onToolButtonClicked` zoomOut   state
  zoomFitButton `onToolButtonClicked` zoomToFit state

  firstButton  `onToolButtonClicked` scrollToBeginning state
  lastButton   `onToolButtonClicked` scrollToEnd state
  centreButton `onToolButtonClicked` centreOnCursor state

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  onScroll timelineDrawingArea $ \(Scroll _ _ _ _ dir _ _ )
    -> do case dir of
           ScrollUp   -> do zoomIn state;  return True
           ScrollDown -> do zoomOut state; return True
           _          -> return True
                 
  ------------------------------------------------------------------------
  -- Mouse button

  onButtonPress timelineDrawingArea $ \button -> do
     when debug $ putStrLn ("button pressed: " ++ show button)
     case button of
       Button{ Old.eventButton = LeftButton, Old.eventClick = SingleClick,
               -- eventModifier = [],  -- contains [Alt2] for me
               eventX = x } -> do
           setCursor state x
           return True
       _other -> do
           return False

  onValueChanged timelineAdj $ queueRedrawTimelines state

  on timelineDrawingArea exposeEvent $ tryEvent $ do
     exposeRegion <- New.eventRegion
     liftIO $ exposeTraceView state exposeRegion

  return ()

-------------------------------------------------------------------------------
-- Update the internal state and the timemline view after changing which
-- traces are displayed, or the order of traces.

updateTimelines :: ViewerState -> [Trace] -> IO ()
updateTimelines state@ViewerState{..} traces = do
  writeIORef timelineTraces traces
  queueRedrawTimelines state

queueRedrawTimelines :: ViewerState -> IO ()
queueRedrawTimelines state = do
  widgetQueueDraw (timelineDrawingArea state)
  widgetQueueDraw (timelineTicksDrawingArea state)
  widgetQueueDraw (timelineLabelDrawingArea state)

-------------------------------------------------------------------------------
-- Set the cursor to a new position

setCursor :: ViewerState -> Double -> IO ()
setCursor state@ViewerState{..} x = do
  hadjValue <- adjustmentGetValue timelineAdj
  scaleValue <- readIORef scaleIORef
  let cursor = round (hadjValue + x * scaleValue)
  when debug $ printf "cursor set to: %d" cursor
  writeIORef cursorIORef cursor
  queueRedrawTimelines state

-------------------------------------------------------------------------------
-- Zoom in works by expanding the current view such that the 
-- left hand edge of the original view remains at the same
-- position and the zoom in factor is 2.
-- For example, zoom into the time range 1.0 3.0
-- produces a new view with the time range 1.0 2.0

zoomIn :: ViewerState -> IO ()
zoomIn  = zoom (/2)

zoomOut :: ViewerState -> IO ()
zoomOut  = zoom (*2)

zoom :: (Double->Double) -> ViewerState -> IO ()
zoom factor state@ViewerState{..} = do
       scaleValue <- readIORef scaleIORef -- Halve the scale value
       let newScaleValue = factor scaleValue
       writeIORef scaleIORef newScaleValue

       cursor <- readIORef cursorIORef
       hadj <- rangeGetAdjustment timelineHScrollbar -- Get horizontal scrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj -- Get size of bar

       let newPageSize = factor hadj_pagesize
       adjustmentSetPageSize hadj newPageSize

       let cursord = fromIntegral cursor
       when (cursord >= hadj_value && cursord < hadj_value + hadj_pagesize) $
         adjustmentSetValue hadj (cursord - factor (cursord - hadj_value))

       let pageshift = 0.9 * newPageSize
       let nudge     = 0.1 * newPageSize

       rangeSetIncrements timelineHScrollbar  nudge pageshift

       scaleUpdateStatus state newScaleValue
       queueRedrawTimelines state

-------------------------------------------------------------------------------

zoomToFit :: ViewerState -> IO ()
zoomToFit state@ViewerState{..} = do
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> writeIORef scaleIORef (-1.0)
    Just hecs -> do
       let lastTx = hecLastEventTime hecs
       (w, _) <- widgetGetSize timelineDrawingArea
       let newScaleValue = fromIntegral lastTx / fromIntegral (w - 2*ox)
                           -- leave a gap of ox pixels at each end
       writeIORef scaleIORef newScaleValue

       -- Configure the horizontal scrollbar units to correspond to ns.
       -- leave a gap of ox pixels on the left and right of the full trace
       let gap   = fromIntegral ox * newScaleValue
           lower = -gap
           upper = fromIntegral lastTx + gap
           page  = upper + gap
           
       adjustmentSetLower    timelineAdj lower
       adjustmentSetValue    timelineAdj lower
       adjustmentSetUpper    timelineAdj upper
       adjustmentSetPageSize timelineAdj page
       rangeSetIncrements timelineHScrollbar 0 0

       scaleUpdateStatus state newScaleValue
       queueRedrawTimelines state

-------------------------------------------------------------------------------

scaleUpdateStatus :: ViewerState -> Double -> IO ()
scaleUpdateStatus state@ViewerState{..} newScaleValue = do
  ctx <- statusbarGetContextId statusBar "state"
  statusbarPush statusBar ctx ("Scale " ++ show newScaleValue)
  return ()

-------------------------------------------------------------------------------

scrollLeft, scrollRight, scrollToBeginning, scrollToEnd, centreOnCursor
  :: ViewerState -> IO ()

scrollLeft        = scroll (\val page l u -> l `max` (val - page/2))
scrollRight       = scroll (\val page l u -> (u - page) `min` (val + page/2))
scrollToBeginning = scroll (\_ _ l u -> l)
scrollToEnd       = scroll (\_ _ l u -> u)

centreOnCursor state@ViewerState{..} = do
  cursor <- readIORef cursorIORef
  scroll (\_ page l u -> max l (fromIntegral cursor - page/2)) state

scroll :: (Double -> Double -> Double -> Double -> Double)
       -> ViewerState -> IO ()
scroll adjust state@ViewerState{..}
  = do scaleValue <- readIORef scaleIORef
       hadj_value <- adjustmentGetValue timelineAdj
       hadj_pagesize <- adjustmentGetPageSize timelineAdj
       hadj_lower <- adjustmentGetLower timelineAdj
       hadj_upper <- adjustmentGetUpper timelineAdj
       let newValue = adjust hadj_value hadj_pagesize hadj_lower hadj_upper
       adjustmentSetValue timelineAdj newValue  
       adjustmentValueChanged timelineAdj       

-------------------------------------------------------------------------------
-- |The 'updateProfileDrawingArea' function is called when an expose event
--  occurs. This function redraws the currently visible part of the
--  main trace canvas plus related canvases.

exposeTraceView :: ViewerState -> Region -> IO ()
exposeTraceView state@ViewerState{..} exposeRegion = do
  maybeEventArray <- readIORef hecsIORef
  
  -- Check to see if an event trace has been loaded
  case maybeEventArray of
    Nothing   -> return ()
    Just hecs -> do
      -- Get state information from user-interface components
      bw_mode <- checkMenuItemGetActive bwToggle
      full_detail <- checkMenuItemGetActive fullDetailToggle
      labels_mode <- toggleToolButtonGetActive showLabelsToggle
      (width,height) <- widgetGetSize timelineDrawingArea
      when debug $ do
        putStrLn ("\n=== updateCanvas") 
        putStrLn (show exposeRegion)
        putStrLn ("width = " ++ show width ++ 
                  " height = " ++ show height)

      let lastTx = hecLastEventTime hecs
      scaleValue <- checkScaleValue state
      -- Get the scrollbar settings
      hadj_lower <- adjustmentGetLower timelineAdj
      hadj_upper <- adjustmentGetUpper timelineAdj
      hadj_value0 <- adjustmentGetValue timelineAdj

      -- snap the view to whole pixels, to avoid blurring
      let hadj_value = toWholePixels scaleValue hadj_value0

      hadj_pagesize <- adjustmentGetPageSize timelineAdj   
      let startTimeOfView = truncate hadj_value
          endTimeOfView = truncate (hadj_value + hadj_pagesize) `min` lastTx

      when debug $ do
        putStrLn ("lastTx = " ++ show lastTx)
        putStrLn ("start time of view = " ++ show startTimeOfView ++ " end time of view = " ++ show endTimeOfView)   

      ctx <- statusbarGetContextId statusBar "state"
      statusbarPush statusBar ctx ("Scale: " ++ show scaleValue ++ " width = " ++ show width ++ " height = " ++ show height ++ " hadj_value = " ++ printf "%1.3f" hadj_value ++ " hadj_pagesize = " ++ show hadj_pagesize ++ " hadj_low = " ++ show hadj_lower ++ " hadj_upper = " ++ show hadj_upper)

      let params = ViewParameters {
                            width     = width,
                            height    = height,
                            hadjValue = hadj_value,
                            scaleValue = scaleValue,
                            detail = 2, -- for now
                            bwMode = bw_mode,
                            labelsMode = labels_mode
                        }

      traces <- readIORef timelineTraces
      renderView state params traces hecs exposeRegion


renderView :: ViewerState -> ViewParameters -> [Trace] -> HECs -> Region
           -> IO ()

renderView state@ViewerState{..} params traces hecs exposeRegion
 = do
  prev_view <- readIORef timelinePrevView
  cursor_t <- readIORef cursorIORef
  rect <- regionGetClipbox exposeRegion

  win <- widgetGetDrawWindow timelineDrawingArea
  renderWithDrawable win $ do

  let renderToNewSurface = do
        new_surface <- withTargetSurface $ \surface -> 
                         liftIO $ createSimilarSurface surface ContentColor
                                    (width params) (height params)
        renderWith new_surface $ do 
             clearWhite
             renderTraces params traces hecs rect
        return new_surface

  surface <- 
    case prev_view of
      Nothing -> do
        when debug $ liftIO $ putStrLn "no old surface"
        renderToNewSurface

      Just (old_params, surface)
         | old_params == params
         -> do when debug $ liftIO $ putStrLn "using previously rendered view"
               return surface

         | width  old_params == width  params &&
           height old_params == height params
         -> do 
               if old_params { hadjValue = hadjValue params } == params
                  then do 
                       when debug $ liftIO $ putStrLn "scrolling"
                       scrollView surface old_params params traces hecs
                       
                  else do 
                       when debug $ liftIO $ putStrLn "using old surface"
                       renderWith surface $ do 
                          clearWhite; renderTraces params traces hecs rect
                       return surface

         | otherwise
         -> do when debug $ liftIO $ putStrLn "old surface no good"
               surfaceFinish surface
               renderToNewSurface

  liftIO $ writeIORef timelinePrevView (Just (params, surface))

  region exposeRegion
  clip
  clearWhite
  setSourceSurface surface 0 0
  setOperator OperatorSource
  paint
  drawCursor cursor_t params

clearWhite :: Render ()
clearWhite = do
  save
  setOperator OperatorSource
  setSourceRGBA 0xffff 0xffff 0xffff 0xffff
  paint
  restore

drawCursor :: Timestamp -> ViewParameters -> Render ()
drawCursor cursor_t param@ViewParameters{..} = do
  withViewScale param $ do
    setLineWidth (3*scaleValue)
    setOperator OperatorOver
    setSourceRGBAhex blue 1.0
    moveTo (fromIntegral cursor_t) 0
    lineTo (fromIntegral cursor_t) (fromIntegral height)
    stroke


-- parameters differ only in the hadjValue, we can scroll ...
scrollView :: Surface -> ViewParameters -> ViewParameters -> [Trace] -> HECs
           -> Render Surface
scrollView surface old new traces hecs = do

--   scrolling on the same surface seems not to work, I get garbled results.
--   Not sure what the best way to do this is.
--   let new_surface = surface
   new_surface <- withTargetSurface $ \surface -> 
                    liftIO $ createSimilarSurface surface ContentColor
                                (width new) (height new)

   renderWith new_surface $ do

       let 
           scale    = scaleValue new
           old_hadj = hadjValue old
           new_hadj = hadjValue new
           w        = fromIntegral (width new)
           h        = fromIntegral (height new)
           off      = (old_hadj - new_hadj) / scale

--   liftIO $ printf "scrollView: old: %f, new %f, dist = %f (%f pixels)\n"
--              old_hadj new_hadj (old_hadj - new_hadj) off

       -- copy the content from the old surface to the new surface, 
       -- shifted by the appropriate amount.
       setSourceSurface surface off 0
       if old_hadj > new_hadj 
          then do rectangle off 0 (w - off) h -- scroll right.
          else do rectangle 0   0 (w + off) h -- scroll left.
       C.fill

       let rect | old_hadj > new_hadj
                = Rectangle 0 0 (ceiling off) (height new)
                | otherwise
                = Rectangle (truncate (w + off)) 0 (ceiling (-off)) (height new)

       case rect of 
         Rectangle x y w h -> rectangle (fromIntegral x) (fromIntegral y) 
                                        (fromIntegral w) (fromIntegral h)
       setSourceRGBA 0xffff 0xffff 0xffff 0xffff
       C.fill

       renderTraces new traces hecs rect

   surfaceFinish surface
   return new_surface

toWholePixels :: Double -> Double -> Double
toWholePixels scale x = fromIntegral (truncate (x / scale)) * scale

-------------------------------------------------------------------------------
-- This function returns a value which can be used to scale
-- Timestamp event log values to pixels.
-- If the scale has previous been computed then it is resued.
-- An "uncomputed" scale value is represetned as -1.0 (defaultScaleValue)
-- We estimate the width of the vertical scrollbar at 20 pixels

checkScaleValue :: ViewerState -> IO Double
checkScaleValue state@ViewerState{..}
  = do scaleValue <- readIORef scaleIORef
       if scaleValue < 0.0 
          then do zoomToFit state
                  readIORef scaleIORef
          else return scaleValue

-------------------------------------------------------------------------------

updateCapabilityDrawingArea :: ViewerState -> Event -> IO Bool
updateCapabilityDrawingArea ViewerState{..} (Expose { Old.eventArea=rect }) 
   = do mb_hecs <- readIORef hecsIORef
        case mb_hecs of
          Nothing -> return ()
          Just hecs -> do
              win <- widgetGetDrawWindow timelineLabelDrawingArea
              gc <- gcNew win
              mapM_ (labelCapability timelineLabelDrawingArea gc) [0..hecCount hecs]
        return True
updateCapabilityDrawingArea _ _ = error "updateCapabilityDrawingArea"

-------------------------------------------------------------------------------

labelCapability :: DrawingArea -> GC -> Int -> IO ()
labelCapability canvas gc n
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` ("HEC " ++ show n)
       drawLayoutWithColors win gc 10 (oycap+6+tracePad*n) txt (Just black) Nothing
