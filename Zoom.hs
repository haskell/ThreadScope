-------------------------------------------------------------------------------
--- $Id: Zoom.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Zoom.hs $
-------------------------------------------------------------------------------

module Zoom
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

import Refresh

-------------------------------------------------------------------------------

zoomIn :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO ()
zoomIn scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale
       writeIORef scale (2*scaleValue)
       hadj <- viewportGetHAdjustment viewport
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let maxVal = 2 * (hadj_upper - hadj_pagesize)
       adjustmentSetValue hadj (((hadj_value+hadj_pagesize/4)*2) `min` maxVal)
       statusbarPush statusbar ctx ("Scale " ++ show (2*scaleValue))           
       refresh canvas

-------------------------------------------------------------------------------

zoomOut :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO ()
zoomOut scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale
       writeIORef scale (scaleValue/2)
       hadj <- viewportGetHAdjustment viewport
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       adjustmentSetValue hadj ((hadj_value/2-hadj_pagesize/4) `max` 0)
       statusbarPush statusbar ctx ("Scale " ++ show (scaleValue/2))           
       refresh canvas

-------------------------------------------------------------------------------
