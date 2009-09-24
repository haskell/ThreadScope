{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}
-------------------------------------------------------------------------------
--- $Id: Scrolling.hs#2 2009/03/30 13:46:44 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Scrolling.hs $
-------------------------------------------------------------------------------

module Scrolling (
    scrollLeft, scrollRight, scrollToBeginning, scrollToEnd, centreOnCursor
  ) where

import State

-- Imports for GTK/Glade
import Graphics.UI.Gtk

import Data.IORef

-------------------------------------------------------------------------------

scrollLeft, scrollRight, scrollToBeginning, scrollToEnd, centreOnCursor
  :: ViewerState -> IO ()

scrollLeft  = scroll (\val page upper -> 0 `max` (val - page/2))
scrollRight = scroll (\val page upper -> (upper - page) `min` (val + page/2))
scrollToBeginning = scroll (\_ _ _ -> 0)
scrollToEnd       = scroll (\_ _ upper -> upper)

centreOnCursor state@ViewerState{..} = do
  cursor <- readIORef cursorIORef
  hadj_pagesize <- adjustmentGetPageSize profileAdj
  scroll (\_ _ _ -> max 0 (fromIntegral cursor - hadj_pagesize/2)) state

scroll :: (Double -> Double -> Double -> Double) -> ViewerState -> IO ()
scroll adjust state@ViewerState{..}
  = do scaleValue <- readIORef scaleIORef
       hadj_value <- adjustmentGetValue profileAdj
       hadj_pagesize <- adjustmentGetPageSize profileAdj
       hadj_upper <- adjustmentGetUpper profileAdj
       let newValue = adjust hadj_value hadj_pagesize hadj_upper
       adjustmentSetValue profileAdj newValue  
       adjustmentValueChanged profileAdj       

-------------------------------------------------------------------------------
