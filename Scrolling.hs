{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
-------------------------------------------------------------------------------
--- $Id: Scrolling.hs#2 2009/03/30 13:46:44 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Scrolling.hs $
-------------------------------------------------------------------------------

module Scrolling
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

import State

-------------------------------------------------------------------------------

scrollLeft :: ViewerState -> IO Bool
scrollLeft state@ViewerState{..}
  = do scaleValue <- readIORef scaleIORef
       hadj <- rangeGetAdjustment profileHScrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = 0 `max` (hadj_value - hadj_pagesize/2)
       adjustmentSetValue hadj newValue  
       adjustmentValueChanged hadj       
       return True

-------------------------------------------------------------------------------

scrollRight :: ViewerState -> IO Bool
scrollRight state@ViewerState{..}
  = do scaleValue <- readIORef scaleIORef
       hadj <- rangeGetAdjustment profileHScrollbar
       hadj_value <- adjustmentGetValue hadj
       hadj_pagesize <- adjustmentGetPageSize hadj
       hadj_upper <- adjustmentGetUpper hadj
       let newValue = (hadj_upper - hadj_pagesize) `min` (hadj_value + hadj_pagesize/2)
       adjustmentSetValue hadj newValue 
       adjustmentValueChanged hadj        
       return True

-------------------------------------------------------------------------------
