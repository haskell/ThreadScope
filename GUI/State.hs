module GUI.State (
    ViewerState(..),
    ViewParameters(..),
    Trace(..),
    HECs(..)
  ) where

import Events.EventTree
import GUI.ConcurrencyControl (ConcurrencyControl)

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef
import Data.Array

-----------------------------------------------------------------------------

data ViewerState = ViewerState {
  concCtl          :: ConcurrencyControl,
  filenameIORef    :: IORef (Maybe FilePath),
  debug            :: Bool,

  -- The loaded profile
  hecsIORef         :: IORef (Maybe HECs),
  scaleIORef        :: IORef Double, -- in ns/pixel
  cursorIORef       :: IORef Timestamp,

  -- WIDGETS

  -- main window
  mainWindow         :: Window,
  statusBar          :: Statusbar,

  -- menu items
  bwToggle           :: CheckMenuItem,

  -- Timeline view
  timelineDrawingArea      :: DrawingArea,
  timelineLabelDrawingArea :: DrawingArea,
  timelineKeyDrawingArea   :: DrawingArea,
  timelineAdj              :: Adjustment,
  timelineVAdj             :: Adjustment,
  showLabelsToggle         :: ToggleToolButton,

  timelinePrevView    :: IORef (Maybe (ViewParameters, Surface)),

  -- Bookmarks
  bookmarkStore      :: ListStore Timestamp,

  -- Traces
  tracesStore        :: TreeStore (Trace,Bool)
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

