-------------------------------------------------------------------------------
-- This module implements the drawing of bookmarks in the Cario timeline
-- canvas. It obtains the list of bookmarks from the list view of bookmarks
-- and then renders the bookmarks in view.
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module GUI.Timeline.RenderBookmarks (renderBookmarks)
where

import GUI.Timeline.WithViewScale

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import GUI.State
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------

renderBookmarks :: ViewerState -> ViewParameters -> Render ()
renderBookmarks ViewerState{bookmarkStore} params@ViewParameters{..}
  = withViewScale params $ do
         -- Get the list of bookmarks
         bookmarkList <- liftIO $ listStoreToList bookmarkStore
         -- Render the bookmarks
         -- First set the line width to one pixel and set the line colour
         (onePixel, _) <- deviceToUserDistance 1 0
         setLineWidth onePixel
         setSourceRGBAhex bookmarkColour 1.0
         mapM_ (drawBookmark height) bookmarkList
         return ()

-------------------------------------------------------------------------------

drawBookmark :: Int -> Timestamp -> Render ()
drawBookmark height bookmarkTime
  = draw_line (bookmarkTime, 0) (bookmarkTime, height)

-------------------------------------------------------------------------------

