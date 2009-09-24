{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
module Zoom (
     zoomIn, zoomOut, zoomToFit
  ) where

import State
import EventlogViewerCommon

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Control.Monad
import Data.IORef

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
       hadj <- rangeGetAdjustment profileHScrollbar -- Get horizontal scrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj -- Get size of bar

       let newPageSize = factor hadj_pagesize
       adjustmentSetPageSize hadj newPageSize

       let cursord = fromIntegral cursor
       when (cursord >= hadj_value && cursord < hadj_value + hadj_pagesize) $
         adjustmentSetValue hadj (cursord - factor (cursord - hadj_value))

       rangeSetIncrements profileHScrollbar 
           (0.1 * newPageSize) (0.9 * newPageSize)
       scaleUpdateStatus state newScaleValue
       widgetQueueDraw profileDrawingArea

-------------------------------------------------------------------------------

zoomToFit :: ViewerState -> IO ()
zoomToFit state@ViewerState{..} = do
  mb_hecs <- readIORef hecsIORef
  case mb_hecs of
    Nothing   -> writeIORef scaleIORef (-1.0)
    Just hecs -> do
       let lastTx = findLastTxValue hecs
       (w, _) <- widgetGetSize profileDrawingArea
       let newScaleValue = fromIntegral lastTx / 
                           fromIntegral (w - 2*ox - 20 - barHeight)
       writeIORef scaleIORef newScaleValue
       -- Configure the horizontal scrollbar units to correspond to
       -- Timespec values
       hadj <- rangeGetAdjustment profileHScrollbar
       adjustmentSetUpper hadj (fromIntegral lastTx)
       adjustmentSetPageSize hadj (fromIntegral lastTx)
       rangeSetIncrements profileHScrollbar 0 0
       scaleUpdateStatus state newScaleValue
       widgetQueueDraw profileDrawingArea

-------------------------------------------------------------------------------

scaleUpdateStatus :: ViewerState -> Double -> IO ()
scaleUpdateStatus state@ViewerState{..} newScaleValue = do
  ctx <- statusbarGetContextId statusBar "state"
  statusbarPush statusBar ctx ("Scale " ++ show newScaleValue)
  return ()
