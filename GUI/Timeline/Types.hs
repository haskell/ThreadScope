module GUI.Timeline.Types (
    TimelineWindow(..),
 ) where


import GUI.Types
import GHC.RTS.Events

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.IORef

-----------------------------------------------------------------------------
-- The CPUs view

data TimelineWindow = TimelineWindow {
       timelineDrawingArea      :: DrawingArea,
       timelineLabelDrawingArea :: DrawingArea,
       timelineKeyDrawingArea   :: DrawingArea,
       timelineAdj              :: Adjustment,
       timelineVAdj             :: Adjustment,

       bwmodeIORef :: IORef Bool,

       timelinePrevView  :: IORef (Maybe (ViewParameters, Surface)),

       --TODO: this should be a bool state like the bwmodeIORef above
       showLabelsToggle  :: ToggleToolButton,

       bookmarkIORef     :: IORef [Timestamp],
       tracesIORef       :: IORef [Trace],
       scaleIORef        :: IORef Double, -- in ns/pixel
       cursorIORef       :: IORef Timestamp,
       hecsIORef         :: IORef (Maybe HECs)
     }

-----------------------------------------------------------------------------
