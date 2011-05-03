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

       --TODO: eliminate, these are **shared** not private IORefs !!
       -- Should instead have methods for updating the display state
       -- and events for when the cursor is changed. Let the interaction
       -- module hold the state.
       scaleIORef        :: IORef Double, -- in ns/pixel
       cursorIORef       :: IORef Timestamp,
       hecsIORef         :: IORef (Maybe HECs)
     }

-----------------------------------------------------------------------------
