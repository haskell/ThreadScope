{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
module UpdateCanvas
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

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

-------------------------------------------------------------------------------
-- |The 'updateCanvas' function is called when an expose event
--  occurs. This function redraws the currently visible part of the
--  main trace canvas plus related canvases.

updateProfileDrawingArea :: ViewerState -> ContextId -> Rectangle -> IO ()
updateProfileDrawingArea state@ViewerState{..} ctx rect
   = do when debug $ putStrLn (show rect)
        maybeEventArray <- readIORef hecsIORef
        -- Check to see if an event trace has been loaded
        when (isJust maybeEventArray) $
           do let Just hecs = maybeEventArray
              -- Get state information from user-interface components
              bw_mode <- checkMenuItemGetActive bwToggle
              full_detail <- checkMenuItemGetActive fullDetailToggle
              labels_mode <- toggleToolButtonGetActive showLabelsToggle
              win <- widgetGetDrawWindow profileDrawingArea
              (width,height) <- widgetGetSize profileDrawingArea
              when debug $ do
                putStrLn ("\n=== updateCanvas") 
                putStrLn ("width = " ++ show width ++ 
                          " height = " ++ show height)
              -- Work out what portion of the trace is in view  
              -- Compute start time of view              
              let lastTx = findLastTxValue hecs
              scaleValue <- checkScaleValue scaleIORef profileDrawingArea profileHScrollbar lastTx
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
                putStrLn ("endTimeOfView' = " ++ show (truncate (hadj_value + hadj_pagesize)))
                putStrLn ("start time of view = " ++ show startTimeOfView ++ " end time of view = " ++ show endTimeOfView)   
                putStrLn ("pixel duration = " ++ show pixelDuration)
              statusbarPush statusBar ctx ("Scale: " ++ show scaleValue ++ " width = " ++ show width ++ " height = " ++ show height ++ " hadj_value = " ++ printf "%1.3f" hadj_value ++ " hadj_pagesize = " ++ show hadj_pagesize ++ " hadj_low = " ++ show hadj_lower ++ " hadj_upper = " ++ show hadj_upper)
              -- widgetSetSizeRequest canvas (truncate (scaleValue * fromIntegral lastTx) + 2*ox) ((length capabilities)*gapcap+oycap)
              drawWindowClear win
              renderWithDrawable win (currentView width height hadj_value 
                 hadj_pagesize scaleValue maybeEventArray
                 full_detail bw_mode labels_mode)

-------------------------------------------------------------------------------
-- This function returns a value which can be used to scale
-- Timestamp event log values to pixels.
-- If the scale has previous been computed then it is resued.
-- An "uncomputed" scale value is represetned as -1.0 (defaultScaleValue)
-- We estimate the width of the vertical scrollbar at 20 pixels

checkScaleValue :: IORef Double -> DrawingArea ->  HScrollbar -> Timestamp -> IO Double
checkScaleValue scale profileDrawingArea profileHScrollbar largestTimestamp 
  = do scaleValue <- readIORef scale
       if scaleValue < 0.0 then
         do (w, _) <- widgetGetSize profileDrawingArea
            let newScale = fromIntegral (w - 2*ox - 20 - barHeight) / (fromIntegral (largestTimestamp))
            writeIORef scale newScale
            -- Configure the horizontal scrollbar units to correspond to
            -- Timespec values
            hadj <- rangeGetAdjustment profileHScrollbar
            adjustmentSetUpper hadj (fromIntegral largestTimestamp)
            adjustmentSetPageSize hadj (fromIntegral largestTimestamp)
            rangeSetIncrements profileHScrollbar 0 0
            return newScale 
        else
         return scaleValue

-------------------------------------------------------------------------------
