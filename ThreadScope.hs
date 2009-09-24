{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-unused-matches #-}

-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports from Haskell library
import System.Environment
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Function
import Data.List

import Paths_threadscope

-- Imports for ThreadScope
import State
import About
import CapabilityLabels
import DrawCapabilityProfile
import UpdateCanvas
import EventlogViewerCommon
import FileDialog
import Key
import Options
import ReadEvents
import Refresh
import Scrolling
import ViewerColours
import Zoom
import EventsWindow

-------------------------------------------------------------------------------

main :: IO ()
main 
  = do -- Deal with command line argument processing.
       -- This application accepts one optional argument specifying 
       -- the event log.
       args <- getArgs
       let options = parseOptions args

       initGUI
       state <- buildInitialState options

       startup options state

startup :: [Option] -> ViewerState -> IO ()
startup options state@ViewerState{..} 
  = do
       let
           filenames = [filename | Filename filename <- options]
           tracenames = [name | TestTrace name <- options]
       when (length filenames > 1)
         (putStrLn "usage: threadscope [eventlog_filename]")
       let filename = if filenames == [] then
                       ""
                      else
                        head filenames
           traceName = if tracenames == [] then
                         ""
                       else
                         head tracenames

       widgetSetAppPaintable mainWindow True
       logoPath <- getDataFileName "threadscope.png"
       windowSetIconFromFile mainWindow logoPath
       onValueChanged profileAdj $ widgetQueueDraw profileDrawingArea

       ------------------------------------------------------------------------
       -- Status bar functionality
       summary_ctx <- statusbarGetContextId summaryBar "state"
       statusbarPush summaryBar summary_ctx "No eventlog loaded."
       ctx <- statusbarGetContextId statusBar "state"
       statusbarPush statusBar ctx ("Scale " ++ show defaultScaleValue)

       ------------------------------------------------------------------------
       --- Get the label for the name of the event log

       -- B&W toggle button
       bwToggle `onToggle` do refresh debug profileDrawingArea

       -- No Labels toggle button
       showLabelsToggle `onToolButtonToggled` do refresh debug profileDrawingArea

       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for 
       -- the capabilities and event array.
       when (filename /= "") $
           registerEventsFromFile filename state summary_ctx

       -- Likewise for test traces
       when (traceName /= "") $
           registerEventsFromTrace traceName state summary_ctx

       -- B&W toggle button
                      
       -- The File:Open menu option can be used to specify an
       -- eventlog file.
       openMenuItem `onActivateLeaf` do
         filename <- openFileDialog mainWindow
         when (isJust filename) $
           do registerEventsFromFile (fromJust filename) state summary_ctx
              refresh debug profileDrawingArea
              refresh debug capDrawingArea
                                     
       ------------------------------------------------------------------------
       -- Save as PDF functionality
       saveMenuItem `onActivateLeaf` do
         (width, height) <- widgetGetSize profileDrawingArea
         scaleValue <- readIORef scaleIORef
         maybeEventArray <- readIORef hecsIORef
         hadj_value <- adjustmentGetValue profileAdj
         hadj_pagesize <- adjustmentGetPageSize profileAdj
         mfn <- readIORef filenameIORef
         case (mfn, maybeEventArray) of
           (Just fn, Just hecs) -> do
             bw_mode <- checkMenuItemGetActive bwToggle
             full_detail <- checkMenuItemGetActive fullDetailToggle
             labels_mode <- toggleToolButtonGetActive showLabelsToggle

             let params = ViewParameters {
                                width     = width,
                                height    = height,
                                hadjValue = hadj_value,
                                scaleValue = scaleValue,
                                detail = 2, -- for now
                                bwMode = bw_mode,
                                labelsMode = labels_mode
                            }

             withPDFSurface (fn++".pdf") (fromIntegral width) (fromIntegral height)
               (flip renderWith $ (translate (-hadj_value) 0 >> 
                                   currentView params hecs) >> showPage)
             withImageSurface C.FormatARGB32 (fromIntegral width) (fromIntegral height) $ \ result ->
             
               do  renderWith result (translate (-hadj_value) 0 >> 
                                      currentView params hecs)
                   surfaceWriteToPNG result (fn++".png")
             statusbarPush statusBar ctx ("Saved " ++ fn ++ ".pdf")
             return ()

           _other -> return ()

       ------------------------------------------------------------------------
       -- Reload functionality
       onActivateLeaf reloadMenuItem $
          do mb_filename <- readIORef filenameIORef
             case mb_filename of
               Nothing -> return ()
               Just filename -> do
                 registerEventsFromFile filename state summary_ctx
                 refresh debug profileDrawingArea

       ------------------------------------------------------------------------
       -- CPUs view

       setupCPUsView state

       ------------------------------------------------------------------------
       -- Event view

       setupEventsWindow state

       ------------------------------------------------------------------------
       -- Quit
       quitMenuItem `onActivateLeaf` mainQuit

       ------------------------------------------------------------------------
       -- Change background colour
       sty <- widgetGetModifierStyle profileDrawingArea
       widgetModifyBg profileDrawingArea StateNormal profileBackground
       widgetModifyStyle profileDrawingArea sty 

       ------------------------------------------------------------------------
       -- About dialog
       aboutMenuItem `onActivateLeaf` showAboutDialog mainWindow

       ------------------------------------------------------------------------
       -- Quit behaviour
       onDestroy mainWindow mainQuit

       ------------------------------------------------------------------------
       -- Show all windows
       widgetShowAll mainWindow

       ------------------------------------------------------------------------
       -- Enter main event loop for GUI.
       mainGUI

-------------------------------------------------------------------------------

buildInitialState :: [Option] -> IO ViewerState
buildInitialState options = do

       gladePath <- getDataFileName "threadscope.glade"
       Just xml <- xmlNew gladePath
       
       let debug = Debug `elem` options


       filenameIORef <- newIORef Nothing

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       capabilitiesIORef <- newIORef Nothing
       hecsIORef         <- newIORef Nothing
       lastTxIORef       <- newIORef 0
       eventArrayIORef   <- newIORef (error "eventArrayIORef")
       scaleIORef        <- newIORef defaultScaleValue
       cursorIORef       <- newIORef 0

       mainWindow         <- xmlGetWidget xml castToWindow "main_window"
       summaryBar         <- xmlGetWidget xml castToStatusbar "summary"
       statusBar          <- xmlGetWidget xml castToStatusbar "statusbar"

       bwToggle           <- xmlGetWidget xml castToCheckMenuItem "black_and_white"
       fullDetailToggle   <- xmlGetWidget xml castToCheckMenuItem "fullDetail"
       openMenuItem       <- xmlGetWidget xml castToMenuItem "openMenuItem"
       saveMenuItem       <- xmlGetWidget xml castToMenuItem "saveMenuItem"
       saveAsMenuItem     <- xmlGetWidget xml castToMenuItem "saveAsMenuItem"
       reloadMenuItem     <- xmlGetWidget xml castToMenuItem "view_reload"
       quitMenuItem       <- xmlGetWidget xml castToMenuItem "quitMenuItem"
       aboutMenuItem      <- xmlGetWidget xml castToMenuItem "aboutMenuItem"

       profileIORef       <- newIORef Nothing
       profileDrawingArea <- xmlGetWidget xml castToDrawingArea "profileDrawingArea"
       profileHScrollbar  <- xmlGetWidget xml castToHScrollbar "profileHScrollbar"
       profileAdj         <- rangeGetAdjustment profileHScrollbar 
       zoomInButton       <- xmlGetWidget xml castToToolButton "cpus_zoomin"
       zoomOutButton      <- xmlGetWidget xml castToToolButton "cpus_zoomout"
       zoomFitButton      <- xmlGetWidget xml castToToolButton "cpus_zoomfit"

       showLabelsToggle   <- xmlGetWidget xml castToToggleToolButton "cpus_showlabels"
       firstButton        <- xmlGetWidget xml castToToolButton "cpus_first"
       lastButton         <- xmlGetWidget xml castToToolButton "cpus_last"
       centreButton       <- xmlGetWidget xml castToToolButton "cpus_centre"

       capDrawingArea     <- xmlGetWidget xml castToDrawingArea "capabilities"
       keyDrawingArea     <- xmlGetWidget xml castToDrawingArea "key"

       eventsVScrollbar   <- xmlGetWidget xml castToVScrollbar "eventsVScroll"
       eventsDrawingArea  <- xmlGetWidget xml castToDrawingArea "eventsDrawingArea"

       return ViewerState { .. }

-----------------------------------------------------------------------------
-- The CPUs view

setupCPUsView :: ViewerState -> IO ()
setupCPUsView state@ViewerState{..} = do

  ------------------------------------------------------------------------
  -- Key presses
  onKeyPress mainWindow $ \Key { eventKeyName = key, eventKeyChar = mch } -> do
    -- when debug $ putStrLn ("key " ++ key)
    case key of
      "Escape" -> mainQuit >> return True
      "Right"  -> do scrollRight state; return True
      "Left"   -> do scrollLeft  state; return True
      _ -> if isJust mch then
             case fromJust mch of 
               '+' -> do zoomIn  state; return True
               '-' -> do zoomOut state; return True
               _   -> return True
           else
             return True

  ------------------------------------------------------------------------
  -- Porgram the callback for the capability profileDrawingArea
  capDrawingArea `onExpose` updateCapabilityDrawingArea state

  ------------------------------------------------------------------------
  -- Set-up the key profileDrawingArea.
  keyDrawingArea `onExpose` updateKeyDrawingArea keyDrawingArea

  ------------------------------------------------------------------------
  -- zoom buttons

  zoomInButton  `onToolButtonClicked` zoomIn    state
  zoomOutButton `onToolButtonClicked` zoomOut   state
  zoomFitButton `onToolButtonClicked` zoomToFit state

  firstButton  `onToolButtonClicked` scrollToBeginning state
  lastButton   `onToolButtonClicked` scrollToEnd state
  centreButton `onToolButtonClicked` centreOnCursor state

  ------------------------------------------------------------------------
  -- Allow mouse wheel to be used for zoom in/out
  onScroll profileDrawingArea $ \(Scroll _ _ _ _ dir _ _ )
    -> do case dir of
           ScrollUp   -> do zoomIn state;  return True
           ScrollDown -> do zoomOut state; return True
           _          -> return True
                 
  ------------------------------------------------------------------------
  -- Mouse button

  onButtonPress profileDrawingArea $ \button -> do
     when debug $ putStrLn ("button pressed: " ++ show button)
     case button of
       Button{ eventButton = LeftButton, eventClick = SingleClick,
               -- eventModifier = [],  -- contains [Alt2] for me
               eventX = x } -> do
           setCursor state x
           return True
       _other -> do
           return False

  ------------------------------------------------------------------------
  -- Program the callback for the main drawing profileDrawingArea
  profileDrawingArea `onExposeRect` updateProfileDrawingArea state

  return ()


setCursor :: ViewerState -> Double -> IO ()
setCursor ViewerState{..} x = do
  hadjValue <- adjustmentGetValue profileAdj
  scaleValue <- readIORef scaleIORef
  writeIORef cursorIORef (round (hadjValue + x * scaleValue))
  widgetQueueDraw profileDrawingArea
