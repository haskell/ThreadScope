module GUI.State (
    ViewerState(..),
    ViewParameters(..),
    Trace(..),
    HECs(..)
  ) where

import Events.EventTree

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef
import Data.Array

-----------------------------------------------------------------------------

data ViewerState = ViewerState {

  -- The loaded profile
  hecsIORef         :: IORef (Maybe HECs),
  scaleIORef        :: IORef Double, -- in ns/pixel
  cursorIORef       :: IORef Timestamp,

  -- WIDGETS

  -- Timeline view
  timelineDrawingArea      :: DrawingArea, --render, motion
  timelineLabelDrawingArea :: DrawingArea, --render, motion
  timelineAdj              :: Adjustment,  --render, motion
  timelineVAdj             :: Adjustment,  --render, motion
  showLabelsToggle         :: ToggleToolButton, --render

  timelinePrevView    :: IORef (Maybe (ViewParameters, Surface)),

  -- Bookmarks
  bookmarkStore      :: ListStore Timestamp, --renderbookmarks

  -- Traces
  tracesStore        :: TreeStore (Trace,Bool) --sidebar, traces
  }

-- all the data from a .eventlog file
data HECs = HECs {
    hecCount         :: Int,
    hecTrees         :: [(DurationTree,EventTree)],
    hecEventArray    :: Array Int GHCEvents.CapEvent,
    hecLastEventTime :: Timestamp
  }

data Trace
  = TraceHEC      Int
  | TraceThread   ThreadId
  | TraceGroup    String
  | TraceActivity
  -- more later ...
  deriving Eq

-- the parameters for a timeline render; used to figure out whether
-- we're drawing the same thing twice.
data ViewParameters = ViewParameters {
    width, height :: Int,
    viewTraces    :: [Trace],
    hadjValue     :: Double,
    scaleValue    :: Double,
    detail        :: Int,
    bwMode, labelsMode :: Bool
  }
  deriving Eq

