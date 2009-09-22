-------------------------------------------------------------------------------
--- $Id: Scrolling.hs#2 2009/03/30 13:46:44 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Scrolling.hs $
-------------------------------------------------------------------------------

module Scrolling
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

import Refresh

-------------------------------------------------------------------------------

scrollLeft :: IORef Double -> HScrollbar -> Statusbar -> ContextId -> DrawingArea
           -> IO Bool
scrollLeft scale profileHScrollbar statusbar ctx canvas
  = do scaleValue <- readIORef scale
       hadj <- rangeGetAdjustment profileHScrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = 0 `max` (hadj_value - hadj_pagesize/2)
       adjustmentSetValue hadj newValue  
       adjustmentValueChanged hadj       
       -- refresh canvas
       return True

-------------------------------------------------------------------------------

scrollRight :: IORef Double -> HScrollbar -> Statusbar -> ContextId -> DrawingArea
           -> IO Bool
scrollRight scale profileHScrollbar statusbar ctx canvas
  = do scaleValue <- readIORef scale
       hadj <- rangeGetAdjustment profileHScrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = (hadj_upper - hadj_pagesize) `min` (hadj_value + hadj_pagesize/2)
       adjustmentSetValue hadj newValue 
       adjustmentValueChanged hadj        
       -- refresh canvas
       return True

-------------------------------------------------------------------------------
