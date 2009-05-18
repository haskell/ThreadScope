module Zoom
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

import Refresh

-------------------------------------------------------------------------------
-- Zoom in works by expanding the current view such that the 
-- left hand edge of the original view remains at the same
-- position and the zoom in factor is 2.
-- For example, zoom into the time range 1.0 3.0
-- produces a new view with the time range 1.0 2.0

zoomIn :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO ()
zoomIn scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale -- Double the scale value
       writeIORef scale (2*scaleValue)
       hadj <- viewportGetHAdjustment viewport -- Get horizontal scrollbar
       hadj_value <- adjustmentGetValue hadj -- Get position of bar
       hadj_pagesize <- adjustmentGetPageSize hadj -- Get size of bar
       hadj_upper <- adjustmentGetUpper hadj -- Get max value of scrollbar
       let maxVal = 2 * (hadj_upper - hadj_pagesize)
       -- adjustmentSetValue hadj (((hadj_value+hadj_pagesize/4)*2) `min` maxVal)
       statusbarPush statusbar ctx ("Scale " ++ show (2*scaleValue))           
       refresh canvas

-------------------------------------------------------------------------------
-- Zoom out works by contracting the current view such that the
-- left hand edge of the original view remains at the same
-- position and the zoom out factor is 2.
-- For example, zoom out of the time range 1.0 2.0
-- produces a new view with the time range 1.0 3.0

zoomOut :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO ()
zoomOut scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale
       writeIORef scale (scaleValue/2)
       hadj <- viewportGetHAdjustment viewport
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       -- adjustmentSetValue hadj ((hadj_value/2-hadj_pagesize/4) `max` 0)
       statusbarPush statusbar ctx ("Scale " ++ show (scaleValue/2))           
       refresh canvas

-------------------------------------------------------------------------------
