module UpdateCanvas
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-- Haskell library imports
import Control.Monad
import Data.Array
import Data.IORef
import Data.Maybe

-- ThreadScope imports
import About
import DrawCapabilityProfile
import CairoDrawing
import EventDuration
import EventlogViewerCommon
import StartTimes
import Ticks
import ViewerColours

-------------------------------------------------------------------------------
-- |The 'updateCanvas' function is called when an expose event
--  occurs. This function redraws the currently visible part of the
--  main trace canvas plus related canvases.

updateCanvas :: Bool -> DrawingArea -> Viewport -> Statusbar -> 
                CheckMenuItem -> ToggleButton ->
                ToggleButton -> ContextId ->  IORef Double ->
                IORef (Maybe [Int])  -> MaybeHECsIORef -> Event ->
                IO Bool
updateCanvas debug canvas viewport statusbar  full_detail_menu_item 
             bw_button labels_button ctx scale 
             capabilitiesIORef eventArrayIORef
             event@(Expose _ area region count)
   = do when debug $ putStrLn (show event)
        maybeCapabilities <- readIORef capabilitiesIORef
        maybeEventArray <- readIORef eventArrayIORef
        -- Check to see if an event trace has been loaded
        when (isJust maybeEventArray) $
           do let Just capabilities = maybeCapabilities
                  Just hecs = maybeEventArray
              -- Get state information from user-interface components
              bw_mode <- toggleButtonGetActive bw_button
              full_detail <- checkMenuItemGetActive full_detail_menu_item
              labels_mode <- toggleButtonGetActive labels_button
              win <- widgetGetDrawWindow canvas 
              (width,height) <- widgetGetSize viewport
              -- Clear the drawing window
              drawWindowClearArea win x y width height
              -- Get the scrollbar settings
              hadj <- viewportGetHAdjustment viewport
              hadj_lower <- adjustmentGetLower hadj
              hadj_upper <- adjustmentGetUpper hadj
              hadj_value <- adjustmentGetValue hadj
              hadj_pagesize <- adjustmentGetPageSize hadj   
              -- Work out what portion of the trace is in view         
              let lastTx = findLastTxValue hecs
              scaleValue <- checkScaleValue scale viewport lastTx
              statusbarPush statusbar ctx ("Scale: " ++ show scaleValue ++ " width = " ++ show width ++ " height = " ++ show height ++ " hadj_value = " ++ show (truncate hadj_value) ++ " hadj_pagesize = " ++ show hadj_pagesize ++ " hadj_low = " ++ show hadj_lower ++ " hadj_upper = " ++ show hadj_upper)
              widgetSetSizeRequest canvas (truncate (scaleValue * fromIntegral lastTx) + 2*ox) ((length capabilities)*gapcap+oycap)
              renderWithDrawable win (currentView width height hadj_value 
                 hadj_pagesize scaleValue maybeEventArray
                 maybeCapabilities full_detail bw_mode labels_mode)
        return True
      where
      Rectangle x y _ _ = area 
updateCanvas debug _ _ _ _ _ _ _ _ _ _ other
   = do when debug $ putStrLn ("Ignorning event " ++ show other) -- Debug rendering errors
        return True

-------------------------------------------------------------------------------
-- This function returns a value which can be used to scale
-- Timestamp event log values to pixels.
-- If the scale has previous been computed then it is resued.
-- An "uncomputed" scale value is represetned as -1.0 (defaultScaleValue)
-- We estimate the width of the vertical scrollbar at 20 pixels

checkScaleValue :: IORef Double -> Viewport -> Timestamp -> IO Double
checkScaleValue scale viewport largestTimestamp
  = do scaleValue <- readIORef scale
       if scaleValue < 0.0 then
         do (w, _) <- widgetGetSize viewport
            let newScale = fromIntegral (w - 2*ox - 20 - barHeight) / (fromIntegral (largestTimestamp))
            writeIORef scale newScale
            return newScale 
        else
         return scaleValue

-------------------------------------------------------------------------------
