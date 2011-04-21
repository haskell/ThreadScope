module GUI.Timeline.WithViewScale
where

import GUI.State

import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

withViewScale :: ViewParameters -> Render () -> Render ()
withViewScale params@ViewParameters{..} inner = do
   save
   scale (1/scaleValue) 1.0
   translate (-hadjValue) 0
   inner
   restore

-------------------------------------------------------------------------------
