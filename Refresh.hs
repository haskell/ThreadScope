-------------------------------------------------------------------------------
--- $Id: Refresh.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Refresh.hs $
-------------------------------------------------------------------------------

module Refresh
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

-------------------------------------------------------------------------------

refresh canvas
  = do win <- widgetGetDrawWindow canvas 
       (width,height) <- widgetGetSize canvas
       -- drawWindowInvalidateRect win (Rectangle 0 0 width height) False
       drawWindowClearAreaExpose win 0 0 width height
       -- drawWindowProcessUpdates win False 

-------------------------------------------------------------------------------
