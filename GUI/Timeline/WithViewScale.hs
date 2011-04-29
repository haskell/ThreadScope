module GUI.Timeline.WithViewScale
where

import GUI.Types

import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

withViewScale :: ViewParameters -> Render () -> Render ()
withViewScale ViewParameters{..} inner = do
   save
   scale (1/scaleValue) 1.0
   translate (-hadjValue) 0
   inner
   restore

-------------------------------------------------------------------------------
