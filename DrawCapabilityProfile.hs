-------------------------------------------------------------------------------
--- $Id: DrawCapabilityProfile.hs#7 2009/03/30 13:46:44 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/DrawCapabilityProfile.hs $
-------------------------------------------------------------------------------

module DrawCapabilityProfile
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
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Function

import Control.Monad
import Data.Array
import Data.IORef
import Data.Maybe

-- ThreadScope imports
import About
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

updateCanvas :: DrawingArea -> Viewport -> Statusbar -> ToggleButton ->
                ToggleButton -> ContextId ->  IORef Double ->
                IORef (Maybe [Int])  -> MaybeHECsIORef -> Event ->
                IO Bool
updateCanvas canvas viewport statusbar bw_button labels_button ctx scale 
             capabilitiesIORef eventArrayIORef
             event@(Expose _ area region count)
   = do -- putStrLn (show event)
        maybeCapabilities <- readIORef capabilitiesIORef
        maybeEventArray <- readIORef eventArrayIORef
        -- Check to see if an event trace has been loaded
        when (isJust maybeEventArray) $
           do let Just capabilities = maybeCapabilities
                  Just hecs = maybeEventArray
              -- Get state information from user-interface components
              bw_mode <- toggleButtonGetActive bw_button
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
              renderWithDrawable win (currentView height hadj_value 
                 hadj_pagesize scaleValue maybeEventArray
                 maybeCapabilities bw_mode labels_mode)
        return True
      where
      Rectangle x y _ _ = area 
updateCanvas _ _ _ _ _ _ _ _ _ other
   = do putStrLn ("Ignorning event " ++ show other) -- Debug rendering errors
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
-- This function draws the current view of all the HECs with Cario

currentView :: Int -> Double -> Double -> Double -> Maybe HECs -> Maybe [Int]
               -> Bool -> Bool -> Render ()
currentView height hadj_value hadj_pagesize scaleValue 
            maybeEventArray maybeCapabilities bw_mode labels_mode
  = do -- If an event trace has been loaded then render it
       when (isJust maybeEventArray) $ do
         let Just hecs = maybeEventArray
             Just capabilities = maybeCapabilities
             lastTx = findLastTxValue hecs
             startPos :: Timestamp
             startPos = fromInteger (truncate (hadj_value / scaleValue))
             endPos :: Timestamp
             endPos = fromInteger (truncate ((hadj_value + hadj_pagesize) / scaleValue)) `min` lastTx 
             tickAdj = tickScale scaleValue
         selectFontFace "times" FontSlantNormal FontWeightNormal
         setFontSize 12
         setSourceRGBAhex blue 1.0
         setLineWidth 1.0
         draw_line (ox, oy) 
                   (ox+ scaleIntegerBy (toInteger endPos) scaleValue, oy)
         drawTicks height scaleValue (toInteger startPos) (10*tickAdj) (100*tickAdj) (toInteger endPos)
         mapM_ (hecView bw_mode labels_mode height scaleValue startPos endPos) 
               (map snd hecs)

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

hecView :: Bool -> Bool -> Int -> Double -> Timestamp -> Timestamp
           -> EventTree -> Render ()
hecView bw_mode labels_mode height scaleValue startPos endPos
        (EventSplit s splitTime e lhs rhs nrEvents)
  = do when (startPos <= splitTime)
         (hecView bw_mode labels_mode height scaleValue startPos endPos lhs)
       when (endPos > splitTime)
         (hecView bw_mode labels_mode height scaleValue startPos endPos rhs)
hecView bw_mode labels_mode height scaleValue startPos endPos 
        (EventTreeLeaf eventList)
  = mapM_ (drawDuration bw_mode labels_mode scaleValue) eventsInView
    where
    eventsInView = [e | e <- eventList, inView startPos endPos e]

-------------------------------------------------------------------------------
-- An event is in view if either its start-edge is in view or its end-edge
-- is in view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event
  = startInView || endInView
    where
    eStart = timeOfEventDuration event
    eEnd   = endTimeOfEventDuration event
    startInView = eStart >= viewStart && eStart <= viewEnd
    endInView   = eEnd   >= viewStart && eEnd   <= viewEnd

-------------------------------------------------------------------------------

nudgeDown :: Integer -> Integer
nudgeDown n
  = if n == 0 then
      0
    else
      n-1

-------------------------------------------------------------------------------

drawDuration :: Bool -> Bool -> Double -> EventDuration -> Render ()

drawDuration bw_mode labels_mode scaleValue (ThreadRun t c s startTime endTime)
  = do setSourceRGBAhex (if not bw_mode then runningColour else black) 0.8
       draw_rectangle_opt False
                      (ox + tsScale startTime scaleValue) -- x
                      (oycap+c*gapcap)           -- y
                      (tsScale (endTime - startTime) scaleValue) -- w
                       barHeight       
       -- Optionally label the bar with the threadID if there is room
       tExtent <- textExtents tStr
       when (textExtentsWidth tExtent < fromIntegral rectWidth) 
         $ do --setSourceRGBAhex black 1.0
              --draw_rectangle_outline
              --        (ox + tsScale startTime scaleValue) -- x
              --        (oycap+c*gapcap)           -- y
              --        (tsScale (endTime - startTime) scaleValue) -- w
              --         barHeight       
              move_to (ox+ tsScale startTime scaleValue, oycap+c*gapcap) 
              setSourceRGBAhex labelTextColour 1.0
              relMoveTo 4 13
              textPath tStr
              C.fill        
        -- Optionally write the reason for the thread being stopped
        -- depending on the zoom value
       when (scaleValue >= subscriptThreashold && not labels_mode)
         $ do setSourceRGB 0 0.0 0.0
              move_to (ox+tsScale endTime scaleValue, oycap+c*gapcap+barHeight+12)
              textPath (show t ++ " " ++ showThreadStopStatus s)
              C.fill
    where
    rectWidth = tsScale (endTime - startTime) scaleValue
    tStr = show t

drawDuration bw_mode _ scaleValue (GC c startTime endTime)
  = do setSourceRGBAhex (if not bw_mode then gcColour else black) 1.0
       draw_rectangle_opt False
                      (ox + tsScale startTime scaleValue) -- x
                      (oycap+c*gapcap+barHeight)                    -- y
                      (tsScale (endTime - startTime) scaleValue) -- w
                      (barHeight `div` 2)                       -- h

drawDuration bw_mode labels_mode scaleValue (EV event)
  = case spec event of 
      CreateThread{cap=c, thread=t} -> 
        when (scaleValue >= 0.25) $ do
          setSourceRGBAhex lightBlue 0.8 
          setLineWidth 2.0
          draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
          when (scaleValue >= 4.0 && not labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                textPath (show t ++ " created")
                C.fill
            )
      RunThread{cap=c, thread=t} -> return ()
      RunSpark{cap=c, thread=t} -> 
           when (scaleValue >= 0.25) $
             do setSourceRGBAhex magenta 0.8 
                setLineWidth 2.0
                draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
      StopThread{cap=c, thread=t, GHC.RTS.Events.status=s} -> return ()
      ThreadRunnable{cap=c, thread=t} ->
        when (scaleValue >= 0.1) $ do
           setSourceRGBAhex darkGreen 0.8 
           setLineWidth 2.0
           draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= 0.2 && not labels_mode)
            (do setSourceRGB 0.0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath (show t ++ " runnable")
                C.fill
            )
      RequestSeqGC{cap=c} -> 
        when (scaleValue >= 0.1) $ do
           setSourceRGBAhex cyan 0.8 
           setLineWidth 2.0
           draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= subscriptThreashold && not labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath ("seq GC req")
                C.fill
            )
      RequestParGC{cap=c} -> 
         when (scaleValue >= 0.1) $ do
           setSourceRGBA 1.0 0.0 1.0 0.8 
           setLineWidth 2.0
           draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= subscriptThreashold && not labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath ("par GC req")
                C.fill
            )
      StartGC _ -> return ()
      MigrateThread {cap=oldc, thread=t, newCap=c}
        -> when (scaleValue >= 0.1) $ do
              setSourceRGBAhex darkRed 0.8 
              setLineWidth 2.0
              draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
              when (scaleValue >= subscriptThreashold && not labels_mode)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                   textPath (show t ++ " migrated from " ++ show oldc)
                   C.fill
               )
      WakeupThread {cap=c, thread=t, otherCap=otherc}
        -> when (scaleValue >= 0.1) $ do 
              setSourceRGBAhex purple 0.8 
              setLineWidth 2.0
              draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
              when (scaleValue >= subscriptThreashold && not labels_mode)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                   textPath (show t ++ " woken from " ++ show otherc)
                   C.fill
               )
      Shutdown{cap=c} ->
         do setSourceRGBAhex shutdownColour 0.8
            draw_rectangle (ox+ eScale event scaleValue) (oycap+c*gapcap) barHeight barHeight
      _ -> return () 

-------------------------------------------------------------------------------

eScale event scaleValue
  = truncate ((fromIntegral (time event) * scaleValue))

-------------------------------------------------------------------------------

updateCapabilityCanvas :: DrawingArea -> IORef (Maybe [Int]) -> Event ->
                          IO Bool
updateCapabilityCanvas canvas capabilitiesIORef (Expose { eventArea=rect }) 
   = do maybeCapabilities <- readIORef capabilitiesIORef
        when (maybeCapabilities /= Nothing)
          (do let Just capabilities = maybeCapabilities
              win <- widgetGetDrawWindow canvas 
              gc <- gcNew win
              mapM_ (labelCapability canvas gc) capabilities
          )
        return True

-------------------------------------------------------------------------------

labelCapability :: DrawingArea -> GC -> Int -> IO ()
labelCapability canvas gc n
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` ("HEC " ++ show n)
       drawLayoutWithColors win gc 10 (oycap+6+gapcap*n) txt (Just black) Nothing

-------------------------------------------------------------------------------

subscriptThreashold :: Double
subscriptThreashold = 0.2  

-------------------------------------------------------------------------------
-- tickScale adjusts the spacing between ticks to avoid collisions

tickScale :: Double -> Integer
tickScale scaleValue
  = if scaleValue <= 2.89e-5 then
      10000000
    else if scaleValue <= 2.31e-4 then
      1000000
    else if scaleValue <= 3.125e-3 then
      100000
    else if scaleValue <= 0.0625 then
      10000
    else if scaleValue <= 0.25 then
      1000
    else 
      100

-------------------------------------------------------------------------------

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "yielding"
showThreadStopStatus ThreadBlocked  = "blocked"
showThreadStopStatus ThreadFinished = "finished"
showThreadStopStatus ForeignCall    = "foreign call"

-------------------------------------------------------------------------------
