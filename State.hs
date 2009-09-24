module State ( ViewerState(..), ViewParameters(..) ) where

import EventlogViewerCommon

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 

import Data.IORef
import Data.Array

-----------------------------------------------------------------------------

data ViewerState = ViewerState {
  filenameIORef    :: IORef (Maybe FilePath),
  debug            :: Bool,

  -- The loaded profile
  capabilitiesIORef :: IORef (Maybe [Int]),
  hecsIORef         :: MaybeHECsIORef,
  lastTxIORef       :: IORef Timestamp,
  eventArrayIORef   :: IORef (Array Int GHCEvents.CapEvent),
  scaleIORef        :: IORef Double, -- in ns/pixel
  cursorIORef       :: IORef Timestamp,

  -- WIDGETS
  
  -- main window
  mainWindow         :: Window,
  summaryBar         :: Statusbar,
  statusBar          :: Statusbar,

  -- menu items
  bwToggle           :: CheckMenuItem,
  fullDetailToggle   :: CheckMenuItem,
  openMenuItem       :: MenuItem,
  saveMenuItem       :: MenuItem,
  saveAsMenuItem     :: MenuItem,
  reloadMenuItem     :: MenuItem,
  quitMenuItem       :: MenuItem,
  aboutMenuItem      :: MenuItem,

  -- CPUs view
  profileIORef       :: IORef (Maybe (ViewParameters, Surface)),
    -- the currently rendered surface, if any

  profileDrawingArea :: DrawingArea,
  profileHScrollbar  :: HScrollbar,
  profileAdj         :: Adjustment,
  zoomInButton       :: ToolButton,
  zoomOutButton      :: ToolButton,
  zoomFitButton      :: ToolButton,
  firstButton        :: ToolButton,
  lastButton         :: ToolButton,
  centreButton       :: ToolButton,
  showLabelsToggle   :: ToggleToolButton,
  capDrawingArea     :: DrawingArea,
  keyDrawingArea     :: DrawingArea,

  -- Events view
  eventsVScrollbar   :: VScrollbar,
  eventsDrawingArea  :: DrawingArea

  }

data ViewParameters = ViewParameters {
    width, height :: Int,
    hadjValue     :: Double,
    scaleValue    :: Double,
    detail        :: Int,
    bwMode, labelsMode :: Bool
  }
  deriving Eq
