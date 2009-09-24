{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
module UpdateCanvas
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo 

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-- Haskell library imports
import Control.Monad
import Data.IORef
import Data.Maybe
import Text.Printf 

-- ThreadScope imports
import State
import DrawCapabilityProfile
import EventlogViewerCommon
import ViewerColours
import Zoom

-------------------------------------------------------------------------------
-- |The 'updateCanvas' function is called when an expose event
--  occurs. This function redraws the currently visible part of the
--  main trace canvas plus related canvases.

updateProfileDrawingArea :: ViewerState -> Rectangle -> IO ()
updateProfileDrawingArea state@ViewerState{..} rect
   = do maybeEventArray <- readIORef hecsIORef
        -- Check to see if an event trace has been loaded
        case maybeEventArray of
          Nothing -> return ()
          Just hecs -> do
              -- Get state information from user-interface components
              bw_mode <- checkMenuItemGetActive bwToggle
              full_detail <- checkMenuItemGetActive fullDetailToggle
              labels_mode <- toggleToolButtonGetActive showLabelsToggle
              (width,height) <- widgetGetSize profileDrawingArea
              when debug $ do
                putStrLn ("\n=== updateCanvas") 
                putStrLn (show rect)
                putStrLn ("width = " ++ show width ++ 
                          " height = " ++ show height)
              -- Work out what portion of the trace is in view  
              -- Compute start time of view              
              let lastTx = findLastTxValue hecs
              scaleValue <- checkScaleValue state
              -- Get the scrollbar settings
              hadj <- rangeGetAdjustment profileHScrollbar
              hadj_lower <- adjustmentGetLower hadj
              hadj_upper <- adjustmentGetUpper hadj
              hadj_value <- adjustmentGetValue hadj
              hadj_pagesize <- adjustmentGetPageSize hadj   
              let startTimeOfView = truncate hadj_value
                  endTimeOfView = truncate (hadj_value + hadj_pagesize) `min` lastTx
                  -- The pixel duration in nanoseconds. This is used
                  -- to determine how much detail to draw.
                  pixelDuration :: Timestamp
                  pixelDuration = truncate hadj_pagesize `div` fromIntegral width
              when debug $ do
                putStrLn ("lastTx = " ++ show lastTx)
                putStrLn ("start time of view = " ++ show startTimeOfView ++ " end time of view = " ++ show endTimeOfView)   
                putStrLn ("pixel duration = " ++ show pixelDuration)

              ctx <- statusbarGetContextId statusBar "state"
              statusbarPush statusBar ctx ("Scale: " ++ show scaleValue ++ " width = " ++ show width ++ " height = " ++ show height ++ " hadj_value = " ++ printf "%1.3f" hadj_value ++ " hadj_pagesize = " ++ show hadj_pagesize ++ " hadj_low = " ++ show hadj_lower ++ " hadj_upper = " ++ show hadj_upper)
              -- widgetSetSizeRequest canvas (truncate (scaleValue * fromIntegral lastTx) + 2*ox) ((length capabilities)*gapcap+oycap)

              let params = ViewParameters {
                                width     = width,
                                height    = height,
                                hadjValue = hadj_value,
                                scaleValue = scaleValue,
                                detail = 2, -- for now
                                bwMode = bw_mode,
                                labelsMode = labels_mode
                            }

              renderView state params hecs

renderView :: ViewerState -> ViewParameters -> HECs -> IO ()
renderView state@ViewerState{..} params hecs = do
  
  prev_view <- readIORef profileIORef
  
  surface <- 
    case prev_view of
      Just (old_params, surface)
         | old_params == params
         -> do when debug $ putStrLn "using previously rendered view"
               return surface

         | width  old_params == width  params &&
           height old_params == height params
         -> do when debug $ putStrLn "using old surface"
               renderWith surface $ do clearWhite; currentView params hecs
               return surface

         | otherwise
         -> do when debug $ putStrLn "old surface no good"
               surfaceFinish surface
               new_surface <- createImageSurface FormatARGB32
                                  (width params) (height params)
               renderWith new_surface $ do clearWhite; currentView params hecs
               return new_surface

      Nothing -> do
        when debug $ putStrLn "no old surface"
        new_surface <- createImageSurface FormatARGB32
                           (width params) (height params)
        renderWith new_surface $ do clearWhite; currentView params hecs
        return new_surface

  writeIORef profileIORef (Just (params, surface))

  win <- widgetGetDrawWindow profileDrawingArea
  cursor_t <- readIORef cursorIORef
  renderWithDrawable win $ do
      setSourceSurface surface 0 0
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
