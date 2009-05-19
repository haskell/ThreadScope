{-# LANGUAGE RecordWildCards #-}

-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)


-- Imports from Haskell library
import System.Environment
import Text.Printf
import Control.Monad
import Data.Array
import Data.IORef
import Data.Maybe
import qualified Data.Function
import Data.List

import Paths_threadscope

-- Imports for ThreadScope
import About
import CairoDrawing
import CapabilityLabels
import DrawCapabilityProfile
import UpdateCanvas
import EventDuration
import EventlogViewerCommon
import FileDialog
import Key
import Options
import ReadEvents
import Refresh
import Scrolling
import Ticks
import ViewerColours
import Zoom

-------------------------------------------------------------------------------

main 
  = do -- Deal with command line argument processing.
       -- This application accepts one optional argument specifying 
       -- the event log.
       args <- getArgs
       let options = parseOptions args
           filenames = [filename | Filename filename <- options]
       when (length filenames > 1)
         (putStrLn "usage: threadscope [eventlog_filename]")
       let filename = if filenames == [] then
                       ""
                      else
                        head filenames
           debug = Debug `elem` options
       filenameRef <- newIORef filename

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       capabilitiesIORef <- newIORef Nothing
       eventArrayIORef <- newIORef Nothing
       lastTxIORef <- newIORef 0

       ------------------------------------------------------------------------
       -- Get main window and viewport
       initGUI
       gladePath <- getDataFileName "threadscope.glade"
       Just xml <- xmlNew gladePath
       window   <- xmlGetWidget xml castToWindow "main_window"
       viewport <- xmlGetWidget xml castToViewport "viewport1"
       scale <- newIORef defaultScaleValue -- How to scale ms to pixels

       ------------------------------------------------------------------------
       -- Status bar functionality
       summarybar <- xmlGetWidget xml castToStatusbar "summary"
       summary_ctx <- statusbarGetContextId summarybar "state"
       statusbarPush summarybar summary_ctx "No eventlog loaded."
       statusbar <- xmlGetWidget xml castToStatusbar "statusbar"
       ctx <- statusbarGetContextId statusbar "state"
       statusbarPush statusbar ctx ("Scale " ++ show defaultScaleValue)

       ------------------------------------------------------------------------
       --- Get the label for the name of the event log
       profileNameLabel <- xmlGetWidget xml castToLabel "profile_name"
 

       -- Set-up the drawing canvas.
       canvas <- xmlGetWidget xml castToDrawingArea "drawingarea1"
 
       -- B&W toggle button
       bw_button <- xmlGetWidget xml castToToggleButton "black_and_white"
       bw_button `onToggled` do refresh canvas

       -- No Labels toggle button
       labels_button <- xmlGetWidget xml castToToggleButton "labels"
       labels_button `onToggled` do refresh canvas

       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for 
       -- the capabilities and event array.
       when (filename /= "") $
           do registerEventsFromFile debug filename capabilitiesIORef
                                     eventArrayIORef scale lastTxIORef 
                                     window viewport profileNameLabel 
                                     summarybar
                                     summary_ctx

       ------------------------------------------------------------------------
       -- View menu
       full_detail_menu_item
         <- xmlGetWidget xml castToCheckMenuItem "fullDetail"
       full_detail_menu_item `onActivateLeaf` do refresh canvas
 
       ------------------------------------------------------------------------
       -- Porgram the callback for the capability canvas
       capability_canvas <- xmlGetWidget xml castToDrawingArea "capabilities"
       capability_canvas `onExpose` 
                  updateCapabilityCanvas capability_canvas capabilitiesIORef


       ------------------------------------------------------------------------
       -- Set-up the key canvas.
       key_canvas <- xmlGetWidget xml castToDrawingArea "key"
       key_canvas `onExpose` updateKeyCanvas key_canvas
 
       -- B&W toggle button
                      
       -- The File:Open menu option can be used to specify an
       -- eventlog file.
       openMenuItem <- xmlGetWidget xml castToMenuItem "openMenuItem"
       openMenuItem `onActivateLeaf` do
         filename <- openFileDialog window
         when (isJust filename) $
           do registerEventsFromFile debug 
                                     (fromJust filename) capabilitiesIORef
                                     eventArrayIORef scale lastTxIORef 
                                     window viewport profileNameLabel 
                                     summarybar
                                     summary_ctx
              refresh canvas
              refresh capability_canvas
                                     
       ------------------------------------------------------------------------
       -- Zoom in button
       zoomInButton <- xmlGetWidget xml castToButton "zoom_in"
       zoomInButton `onClicked`
          zoomIn scale viewport statusbar ctx canvas
                                            
       ------------------------------------------------------------------------
       -- Zoom out button
       zoomOutButton <- xmlGetWidget xml castToButton "zoom_out"
       zoomOutButton `onClicked` 
          zoomOut scale viewport statusbar ctx canvas

       ------------------------------------------------------------------------
       -- Save as PDF functionality
       saveMenuItem <- xmlGetWidget xml castToMenuItem "saveMenuItem"
       saveMenuItem `onActivateLeaf` do
         (width, height) <- widgetGetSize viewport
         scaleValue <- readIORef scale
         maybeEventArray <- readIORef eventArrayIORef
         maybeCapabilities <- readIORef capabilitiesIORef
         hadj <- viewportGetHAdjustment viewport
         hadj_value <- adjustmentGetValue hadj
         hadj_pagesize <- adjustmentGetPageSize hadj
         fn <- readIORef filenameRef
         bw_mode <- toggleButtonGetActive bw_button
         full_detail <- checkMenuItemGetActive full_detail_menu_item
         labels_mode <- toggleButtonGetActive labels_button
         withPDFSurface (fn++".pdf") (fromIntegral width) (fromIntegral height)
           (flip renderWith $ (translate (-hadj_value) 0 >> 
                               currentView width height hadj_value hadj_pagesize 
                               scaleValue maybeEventArray maybeCapabilities 
                               full_detail bw_mode labels_mode) >> showPage)
         statusbarPush statusbar ctx ("Saved " ++ fn ++ ".pdf")
         return ()

       ------------------------------------------------------------------------
       -- Allow mouse wheel to be used for zoom in/out
       onScroll canvas (\(Scroll _ _ _ _ dir _ _ )
         -> do case dir of
                ScrollUp -> do -- zoom in
                               zoomIn scale viewport statusbar ctx canvas
                               return True
                ScrollDown -> do -- zoom out
                               zoomOut scale viewport statusbar ctx canvas
                               return True
                _ -> return True)
                      
       ------------------------------------------------------------------------
       -- Default scale functionality
       defaultScaleButton <- xmlGetWidget xml castToButton "default_scale"
       onClicked defaultScaleButton $
          do writeIORef scale 0.1 
             statusbarPush statusbar ctx ("Scale 0.1")
             --hadj <- viewportGetHAdjustment viewport 
             --adjustmentValueChanged hadj
             refresh canvas

       ------------------------------------------------------------------------
       -- Reload functionality
       reloadButton <- xmlGetWidget xml castToButton "reload_button"
       onClicked reloadButton $
          do filename <- readIORef filenameRef
             when (filename /= "") $
              do registerEventsFromFile  debug filename capabilitiesIORef
                                         eventArrayIORef scale lastTxIORef 
                                         window viewport profileNameLabel summarybar
                                         summary_ctx
                 refresh canvas

       ------------------------------------------------------------------------
       -- Key presses
       onKeyPress window $ \Key { eventKeyName = key, eventKeyChar = mch } -> do
         -- when debug $ putStrLn ("key " ++ key)
         case key of
           "Escape" -> mainQuit >> return True
           "Right" -> scrollRight scale viewport statusbar ctx canvas
           "Left" -> scrollLeft scale viewport statusbar ctx canvas
           _ -> if isJust mch then
                  case fromJust mch of 
                    '+' -> do zoomIn scale viewport statusbar ctx canvas
                              return True
                    '-' -> do zoomOut scale viewport statusbar ctx canvas
                              return True
                    _   -> return True
                else
                  return True

       ------------------------------------------------------------------------
       -- Quit
       quitMenuItem <- xmlGetWidget xml castToMenuItem "quitMenuItem"
       quitMenuItem `onActivateLeaf` mainQuit

       ------------------------------------------------------------------------
       -- Change background colour
       sty <- widgetGetModifierStyle canvas
       widgetModifyBg canvas StateNormal profileBackground
       widgetModifyStyle canvas sty 

       ------------------------------------------------------------------------
       -- Program the callback for the main drawing canvas
       canvas `onExpose` updateCanvas 
                  debug canvas viewport statusbar full_detail_menu_item 
                  bw_button
                  labels_button ctx scale 
                  capabilitiesIORef eventArrayIORef

       ------------------------------------------------------------------------
       -- About dialog
       aboutMenuItem <- xmlGetWidget xml castToMenuItem "aboutMenuItem"
       aboutMenuItem `onActivateLeaf` showAboutDialog window

       ------------------------------------------------------------------------
       -- Quit behaviour
       onDestroy window mainQuit

       ------------------------------------------------------------------------
       -- Show all windows
       widgetShowAll window

       ------------------------------------------------------------------------
       -- Enter main event loop for GUI.
       mainGUI

-------------------------------------------------------------------------------

