-------------------------------------------------------------------------------
--- $Id: Scrolling.hs#1 2009/03/20 16:13:19 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Scrolling.hs $
-------------------------------------------------------------------------------

module Scrolling
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

import Refresh

-------------------------------------------------------------------------------

scrollLeft :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO Bool
scrollLeft scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale
       hadj <- viewportGetHAdjustment viewport
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = 0 `max` (hadj_value - hadj_pagesize)
       adjustmentSetValue hadj newValue  
       adjustmentValueChanged hadj       
       refresh canvas
       return True

-------------------------------------------------------------------------------

scrollRight :: IORef Double -> Viewport -> Statusbar -> ContextId -> DrawingArea
           -> IO Bool
scrollRight scale viewport statusbar ctx canvas
  = do scaleValue <- readIORef scale
       hadj <- viewportGetHAdjustment viewport
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = (hadj_upper - hadj_pagesize) `min` (hadj_value + hadj_pagesize)
       adjustmentSetValue hadj newValue 
       adjustmentValueChanged hadj        
       refresh canvas
       return True

-------------------------------------------------------------------------------
