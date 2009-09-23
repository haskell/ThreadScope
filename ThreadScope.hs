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

-- Imports from Haskell library
import System.Environment
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Function
import Data.List

import Paths_threadscope

-- Imports for ThreadScope
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
           debug = Debug `elem` options
       filenameRef <- newIORef filename

       ------------------------------------------------------------------------
       -- Get main window and viewport
       initGUI
       gladePath <- getDataFileName "threadscope.glade"
       Just xml <- xmlNew gladePath
       window   <- xmlGetWidget xml castToWindow "main_window"
       widgetSetAppPaintable window True
       logoPath <- getDataFileName "threadscope.png"
       windowSetIconFromFile window logoPath
       
       profileDrawingArea <- xmlGetWidget xml castToDrawingArea "profileDrawingArea"
       profileHScrollbar <- xmlGetWidget xml castToHScrollbar "profileHScrollbar"
       hadj <- rangeGetAdjustment profileHScrollbar 
       onValueChanged hadj $ widgetQueueDraw profileDrawingArea

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
 

       -- B&W toggle button
       bw_button <- xmlGetWidget xml castToToggleButton "black_and_white"
       bw_button `onToggled` do refresh debug profileDrawingArea

       -- No Labels toggle button
       labels_button <- xmlGetWidget xml castToToggleButton "labels"
       labels_button `onToggled` do refresh debug profileDrawingArea

       -- IORefs are used to communicate informaiton about the eventlog
       -- to the callback functions for windows, buttons etc.
       caps_ref <- newIORef Nothing
       hecs_ref <- newIORef Nothing
       lasttx_ref <- newIORef 0
       arr_ref <- newIORef (error "eventArrayIORef")
       scale_ref <- newIORef defaultScaleValue -- How to scale ns to pixels

       let state = ViewerState { capabilitiesIORef = caps_ref,
				 hecsIORef         = hecs_ref,
				 scaleIORef        = scale_ref,
				 lastTxIORef       = lasttx_ref,
				 eventArrayIORef   = arr_ref }

       -- When a filename for an event log is specified open and
       -- parse the event log file and update the IORefs for 
       -- the capabilities and event array.
       when (filename /= "") $
           do registerEventsFromFile debug filename state
                                     window 
                                     profileNameLabel 
                                     summarybar
                                     summary_ctx

       -- Likewise for test traces
       when (traceName /= "") $
           do registerEventsFromTrace debug traceName state
                                     window
                                     profileNameLabel 
                                     summarybar
                                     summary_ctx



       ------------------------------------------------------------------------
       -- View menu
       full_detail_menu_item
         <- xmlGetWidget xml castToCheckMenuItem "fullDetail"
       full_detail_menu_item `onActivateLeaf` do refresh debug profileDrawingArea
 
       ------------------------------------------------------------------------
       -- Porgram the callback for the capability profileDrawingArea
       capabilityDrawingArea <- xmlGetWidget xml castToDrawingArea "capabilities"
       capabilityDrawingArea `onExpose` 
                  updateCapabilityDrawingArea capabilityDrawingArea caps_ref


       ------------------------------------------------------------------------
       -- Set-up the key profileDrawingArea.
       keyDrawingArea <- xmlGetWidget xml castToDrawingArea "key"
       keyDrawingArea `onExpose` updateKeyDrawingArea keyDrawingArea
 
       -- B&W toggle button
                      
       -- The File:Open menu option can be used to specify an
       -- eventlog file.
       openMenuItem <- xmlGetWidget xml castToMenuItem "openMenuItem"
       openMenuItem `onActivateLeaf` do
         filename <- openFileDialog window
         when (isJust filename) $
           do registerEventsFromFile debug 
                                     (fromJust filename) state
                                     window 
                                     profileNameLabel 
                                     summarybar
                                     summary_ctx
              refresh debug profileDrawingArea
              refresh debug capabilityDrawingArea
                                     
       ------------------------------------------------------------------------
       -- Zoom in button
       zoomInButton <- xmlGetWidget xml castToButton "zoom_in"
       zoomInButton `onClicked`
          zoomIn scale_ref profileHScrollbar statusbar ctx profileDrawingArea
                                            
       ------------------------------------------------------------------------
       -- Zoom out button
       zoomOutButton <- xmlGetWidget xml castToButton "zoom_out"
       zoomOutButton `onClicked` 
          zoomOut scale_ref profileHScrollbar statusbar ctx profileDrawingArea

       ------------------------------------------------------------------------
       -- Save as PDF functionality
       saveMenuItem <- xmlGetWidget xml castToMenuItem "saveMenuItem"
       saveMenuItem `onActivateLeaf` do
         (width, height) <- widgetGetSize profileDrawingArea
         scaleValue <- readIORef scale_ref
         maybeEventArray <- readIORef hecs_ref
         hadj <- rangeGetAdjustment profileHScrollbar
         hadj_value <- adjustmentGetValue hadj
         hadj_pagesize <- adjustmentGetPageSize hadj
         fn <- readIORef filenameRef
         bw_mode <- toggleButtonGetActive bw_button
         full_detail <- checkMenuItemGetActive full_detail_menu_item
         labels_mode <- toggleButtonGetActive labels_button
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
         statusbarPush statusbar ctx ("Saved " ++ fn ++ ".pdf")
         return ()

       ------------------------------------------------------------------------
       -- Allow mouse wheel to be used for zoom in/out
       onScroll profileDrawingArea (\(Scroll _ _ _ _ dir _ _ )
         -> do case dir of
                ScrollUp -> do -- zoom in
                               zoomIn scale_ref profileHScrollbar statusbar ctx profileDrawingArea
                               return True
                ScrollDown -> do -- zoom out
                               zoomOut scale_ref profileHScrollbar statusbar ctx profileDrawingArea
                               return True
                _ -> return True)
                      
       ------------------------------------------------------------------------
       -- Default scale functionality
       defaultScaleButton <- xmlGetWidget xml castToButton "default_scale"
       onClicked defaultScaleButton $
          do writeIORef scale_ref 0.1 
             statusbarPush statusbar ctx ("Scale 0.1")
             --hadj <- viewportGetHAdjustment viewport 
             --adjustmentValueChanged hadj
             refresh debug profileDrawingArea

       ------------------------------------------------------------------------
       -- Reload functionality
       reloadButton <- xmlGetWidget xml castToButton "reload_button"
       onClicked reloadButton $
          do filename <- readIORef filenameRef
             when (filename /= "") $
              do registerEventsFromFile  debug filename state
                                         window profileNameLabel summarybar
                                         summary_ctx
                 refresh debug profileDrawingArea

       ------------------------------------------------------------------------
       -- Key presses
       onKeyPress window $ \Key { eventKeyName = key, eventKeyChar = mch } -> do
         -- when debug $ putStrLn ("key " ++ key)
         case key of
           "Escape" -> mainQuit >> return True
           "Right" -> scrollRight scale_ref profileHScrollbar statusbar ctx profileDrawingArea
           "Left" -> scrollLeft scale_ref profileHScrollbar statusbar ctx profileDrawingArea
           _ -> if isJust mch then
                  case fromJust mch of 
                    '+' -> do zoomIn scale_ref profileHScrollbar statusbar ctx profileDrawingArea
                              return True
                    '-' -> do zoomOut scale_ref profileHScrollbar statusbar ctx profileDrawingArea
                              return True
                    _   -> return True
                else
                  return True

       ------------------------------------------------------------------------
       -- Event view

       setupEventsWindow debug state xml

       ------------------------------------------------------------------------
       -- Quit
       quitMenuItem <- xmlGetWidget xml castToMenuItem "quitMenuItem"
       quitMenuItem `onActivateLeaf` mainQuit

       ------------------------------------------------------------------------
       -- Change background colour
       sty <- widgetGetModifierStyle profileDrawingArea
       widgetModifyBg profileDrawingArea StateNormal profileBackground
       widgetModifyStyle profileDrawingArea sty 

       ------------------------------------------------------------------------
       -- Program the callback for the main drawing profileDrawingArea
       profileDrawingArea `onExposeRect` updateProfileDrawingArea 
                  debug profileDrawingArea profileHScrollbar statusbar full_detail_menu_item 
                  bw_button
                  labels_button ctx scale_ref
		  caps_ref hecs_ref

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

