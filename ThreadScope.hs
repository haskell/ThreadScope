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

       ------------------------------------------------------------------------
       -- View menu
       fullDetailToggle `onActivateLeaf` do refresh debug profileDrawingArea
 
       ------------------------------------------------------------------------
       -- Porgram the callback for the capability profileDrawingArea
       capDrawingArea `onExpose` updateCapabilityDrawingArea state

       ------------------------------------------------------------------------
       -- Set-up the key profileDrawingArea.
       keyDrawingArea `onExpose` updateKeyDrawingArea keyDrawingArea
 
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
       -- Zoom in button
       zoomInButton `onToolButtonClicked`
          zoomIn scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
                                            
       ------------------------------------------------------------------------
       -- Zoom out button
       zoomOutButton `onToolButtonClicked`
          zoomOut scaleIORef profileHScrollbar statusBar ctx profileDrawingArea

       ------------------------------------------------------------------------
       -- Save as PDF functionality
       saveMenuItem `onActivateLeaf` do
         (width, height) <- widgetGetSize profileDrawingArea
         scaleValue <- readIORef scaleIORef
         maybeEventArray <- readIORef hecsIORef
         hadj_value <- adjustmentGetValue profileAdj
         hadj_pagesize <- adjustmentGetPageSize profileAdj
         mfn <- readIORef filenameIORef
         case mfn of
           Nothing -> return ()
           Just fn -> do
             bw_mode <- checkMenuItemGetActive bwToggle
             full_detail <- checkMenuItemGetActive fullDetailToggle
             labels_mode <- toggleToolButtonGetActive showLabelsToggle
             withPDFSurface (fn++".pdf") (fromIntegral width) (fromIntegral height)
               (flip renderWith $ (translate (-hadj_value) 0 >> 
                                   currentView width height hadj_value hadj_pagesize 
                                   scaleValue maybeEventArray
                                   full_detail bw_mode labels_mode) >> showPage)
             withImageSurface C.FormatARGB32 (fromIntegral width) (fromIntegral height) $ \ result ->
             
               do  renderWith result (translate (-hadj_value) 0 >> 
                                      currentView width height hadj_value hadj_pagesize 
                                      scaleValue maybeEventArray
                                      full_detail bw_mode labels_mode)
                   surfaceWriteToPNG result (fn++".png")
             statusbarPush statusBar ctx ("Saved " ++ fn ++ ".pdf")
             return ()

       ------------------------------------------------------------------------
       -- Allow mouse wheel to be used for zoom in/out
       onScroll profileDrawingArea (\(Scroll _ _ _ _ dir _ _ )
         -> do case dir of
                ScrollUp -> do -- zoom in
                               zoomIn scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
                               return True
                ScrollDown -> do -- zoom out
                               zoomOut scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
                               return True
                _ -> return True)
                      
       ------------------------------------------------------------------------
       -- Default scale functionality
       onToolButtonClicked zoomFitButton $
          do writeIORef scaleIORef 0.1 
             statusbarPush statusBar ctx ("Scale 0.1")
             --hadj <- viewportGetHAdjustment viewport 
             --adjustmentValueChanged hadj
             refresh debug profileDrawingArea

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
       -- Key presses
       onKeyPress mainWindow $ \Key { eventKeyName = key, eventKeyChar = mch } -> do
         -- when debug $ putStrLn ("key " ++ key)
         case key of
           "Escape" -> mainQuit >> return True
           "Right" -> scrollRight scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
           "Left" -> scrollLeft scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
           _ -> if isJust mch then
                  case fromJust mch of 
                    '+' -> do zoomIn scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
                              return True
                    '-' -> do zoomOut scaleIORef profileHScrollbar statusBar ctx profileDrawingArea
                              return True
                    _   -> return True
                else
                  return True

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
       -- Program the callback for the main drawing profileDrawingArea
       profileDrawingArea `onExposeRect` updateProfileDrawingArea state ctx

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
                               -- How to scale ns to pixels

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

       profileDrawingArea <- xmlGetWidget xml castToDrawingArea "profileDrawingArea"
       profileHScrollbar  <- xmlGetWidget xml castToHScrollbar "profileHScrollbar"
       profileAdj         <- rangeGetAdjustment profileHScrollbar 
       zoomInButton       <- xmlGetWidget xml castToToolButton "zoom_in"
       zoomOutButton      <- xmlGetWidget xml castToToolButton "zoom_out"
       zoomFitButton      <- xmlGetWidget xml castToToolButton "default_zoom"

       showLabelsToggle   <- xmlGetWidget xml castToToggleToolButton "labels"
       capDrawingArea     <- xmlGetWidget xml castToDrawingArea "capabilities"
       keyDrawingArea     <- xmlGetWidget xml castToDrawingArea "key"

       eventsVScrollbar   <- xmlGetWidget xml castToVScrollbar "eventsVScroll"
       eventsDrawingArea  <- xmlGetWidget xml castToDrawingArea "eventsDrawingArea"

       return ViewerState { .. }
