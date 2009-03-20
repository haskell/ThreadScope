-------------------------------------------------------------------------------
--- $Id: DrawCapabilityProfile.hs#17 2009/03/10 17:52:49 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/DrawCapabilityProfile.hs $
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
