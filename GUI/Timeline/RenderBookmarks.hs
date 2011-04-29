-------------------------------------------------------------------------------
-- This module implements the drawing of bookmarks in the Cario timeline
-- canvas. It obtains the list of bookmarks from the list view of bookmarks
-- and then renders the bookmarks in view.
-------------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
module GUI.Timeline.RenderBookmarks (renderBookmarks) where

import GUI.Timeline.WithViewScale

import GUI.Types
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import GHC.RTS.Events hiding (Event)

import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

renderBookmarks :: [Timestamp] -> ViewParameters -> Render ()
renderBookmarks bookmarkList params@ViewParameters{..}
  = withViewScale params $ do
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

