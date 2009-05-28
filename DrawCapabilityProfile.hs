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
-- This function draws the current view of all the HECs with Cario

currentView :: Int -> Int -> Double -> Double -> Double -> Maybe HECs -> Maybe [Int] ->
               Bool -> Bool -> Bool -> Render ()
currentView width height hadj_value hadj_pagesize scaleValue 
            maybeEventArray maybeCapabilities full_detail bw_mode labels_mode
  = do -- If an event trace has been loaded then render it
       when (isJust maybeEventArray) $ do
         let Just hecs = maybeEventArray
             Just capabilities = maybeCapabilities
             lastTx = findLastTxValue hecs
             startPos :: Timestamp
             startPos = fromInteger (truncate (hadj_value / scaleValue))
             endPos :: Timestamp
             endPos = fromInteger (truncate ((hadj_value + fromIntegral width) / scaleValue)) `min` lastTx 
             tickAdj = tickScale scaleValue
         
         selectFontFace "times" FontSlantNormal FontWeightNormal
         setFontSize 12
         setSourceRGBAhex blue 1.0
         setLineWidth 1.0
         draw_line (ox, oy) 
                   (ox+ scaleIntegerBy (toInteger endPos) scaleValue, oy)
         let widthInPixelsContainingTrace = truncate (fromIntegral (endPos-startPos)*scaleValue)
         drawTicks height scaleValue (toInteger startPos) (10*tickAdj) (100*tickAdj) (toInteger endPos)
         sequence_ [hecView c full_detail bw_mode labels_mode widthInPixelsContainingTrace height scaleValue startPos endPos eventTree | (c, eventTree) <- hecs]
         C.translate (-hadj_value) 0

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

hecView :: Int -> Bool -> Bool -> Bool -> Int -> Int -> Double -> Timestamp -> 
           Timestamp -> EventTree -> Render ()
hecView c full_detail bw_mode labels_mode width height scaleValue startPos endPos
        event@(EventSplit s splitTime e lhs rhs nrEvents _ _) 
        | straddleView startPos endPos s splitTime e && width <= 100 && 
          not full_detail
  = -- View spans both left and right sub-tree.
    drawAverageDuration c bw_mode labels_mode scaleValue event
hecView c full_detail bw_mode labels_mode width height scaleValue startPos endPos
        (EventSplit s splitTime e lhs rhs nrEvents _ _)
  = do when (startPos <= splitTime)
         (hecView c full_detail bw_mode labels_mode (width `div` 2) height scaleValue startPos endPos lhs)
       when (endPos > splitTime)
         (hecView c full_detail bw_mode labels_mode (width `div` 2) height scaleValue startPos endPos rhs)
hecView c full_detail bw_mode labels_mode width height scaleValue startPos endPos 
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
-- Check to see if view spans both sub-trees

straddleView :: Timestamp -> Timestamp -> Timestamp -> Timestamp 
                -> Timestamp ->  Bool
straddleView viewStart viewEnd eStart splitTime eEnd
  = startInLHS && endInRHS
    where
    startInLHS = eStart >= viewStart && eStart <= splitTime
    endInRHS   = eEnd   > splitTime

-------------------------------------------------------------------------------

drawAverageDuration :: Int -> Bool -> Bool -> Double -> EventTree -> Render ()
drawAverageDuration c bw_mode labels_mode scaleValue
                    (EventSplit startTime splitTime endTime lhs rhs 
                                nrEvents runAv gcAv)
  = do setSourceRGBAhex (if not bw_mode then darkPurple else black) runRatio
       draw_outlined_rectangle (ox + tsScale startTime scaleValue) -- x
                      (oycap+c*gapcap)           -- y
                      (tsScale (endTime - startTime) scaleValue) -- w
                       barHeight
       setSourceRGBAhex black 1.0
       move_to (ox+ tsScale startTime scaleValue, oycap+c*gapcap)
       relMoveTo 4 13
       textPath (show nrEvents)
       C.fill  
       setSourceRGBAhex (if not bw_mode then gcColour else black) gcRatio
       draw_rectangle (ox + tsScale startTime scaleValue) -- x
                      (oycap+c*gapcap+barHeight)                    -- y
                      (tsScale (endTime - startTime) scaleValue) -- w
                      (barHeight `div` 2)                       -- h

    where
    duration = endTime - startTime
    runRatio :: Double
    runRatio = (fromIntegral runAv) / (fromIntegral duration)
    gcRatio :: Double
    gcRatio = (fromIntegral gcAv) / (fromIntegral duration)
    
    

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
         $ do move_to (ox+ tsScale startTime scaleValue, oycap+c*gapcap) 
              setSourceRGBAhex labelTextColour 1.0
              relMoveTo 4 13
              textPath tStr
              C.fill        
        -- Optionally write the reason for the thread being stopped
        -- depending on the zoom value
       when (scaleValue >= subscriptThreashold && not labels_mode)
         $ do setSourceRGBAhex black 1.0
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
