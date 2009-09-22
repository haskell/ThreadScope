module DrawCapabilityProfile
where

-- Imports for GTK/Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-- Haskell library imports
import Data.List
import qualified Data.Function

import Control.Monad
import Data.Maybe

-- import Text.Printf
-- import Debug.Trace

-- ThreadScope imports
import CairoDrawing
import EventlogViewerCommon
import Ticks
import ViewerColours


-------------------------------------------------------------------------------
-- This function draws the current view of all the HECs with Cario

currentView :: Int -> Int -> Double -> Double -> Double -> 
               Maybe HECs -> Bool -> Bool -> Bool -> Render ()
currentView width height hadj_value hadj_pagesize scaleValue 
            maybeEventArray full_detail bw_mode labels_mode
  = do -- If an event trace has been loaded then render it
       when (isJust maybeEventArray) $ do
         let Just hecs = maybeEventArray
             lastTx :: Timestamp
             lastTx = findLastTxValue hecs
             startPos :: Timestamp
             startPos = truncate hadj_value
             endPos :: Timestamp
             endPos = truncate (hadj_value + hadj_pagesize) `min` lastTx 
         selectFontFace "times" FontSlantNormal FontWeightNormal
         setFontSize 12
         setSourceRGBAhex blue 1.0
         setLineWidth 1.0
         C.save
         C.translate (-hadj_value*scaleValue) 0
         C.scale scaleValue 1.0
         draw_line (oxs, oy) (oxs + endPos, oy)
         let 
             timestampFor100Pixels = truncate (100 / scaleValue) -- ns time for 100 pixels
             snappedTickDuration :: Timestamp
             snappedTickDuration = 10 ^ truncate (logBase 10 (fromIntegral timestampFor100Pixels) :: Double)
             tickWidthInPixels :: Int
             tickWidthInPixels = truncate ((fromIntegral snappedTickDuration) * scaleValue)
             firstTick :: Timestamp
             firstTick = snappedTickDuration * (startPos `div` snappedTickDuration)        
         drawTicks tickWidthInPixels height scaleValue firstTick snappedTickDuration  (10*snappedTickDuration) endPos
         sequence_ [hecView c full_detail bw_mode labels_mode scaleValue pixelDuration startPos endPos eventTree | (c, eventTree) <- hecs]
         C.restore
     where
     oxs = truncate ((fromIntegral ox) / scaleValue) -- x origin as Timestamp
     pixelDuration :: Timestamp
     pixelDuration = truncate hadj_pagesize `div` fromIntegral width

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

hecView :: Int -> Bool -> Bool -> Bool -> Double -> 
           Timestamp -> Timestamp -> 
           Timestamp -> EventTree -> Render ()
hecView c full_detail bw_mode labels_mode scale pixelDuration startPos endPos
        (EventSplit s splitTime e _ _ nrEvents runAv gcAv) 
        | startPos < splitTime && endPos >= splitTime && 
	  ((e - s) `quot` pixelDuration) <= 2 && nrEvents > 2 &&
          not full_detail
  = -- View spans both left and right sub-tree.
    -- trace (printf "hecView (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration c bw_mode labels_mode scale s e runAv gcAv

hecView c full_detail bw_mode labels_mode scale pixelDuration startPos endPos
       (EventSplit _ splitTime _ lhs rhs _ _ _)
  = -- trace (printf "hecView: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         hecView c full_detail bw_mode labels_mode 
                  scale pixelDuration startPos endPos lhs
       when (endPos >= splitTime) $
         hecView c full_detail bw_mode labels_mode 
                  scale pixelDuration  startPos endPos rhs
hecView c _full_detail bw_mode labels_mode scale pixelDuration
        startPos endPos  (EventTreeLeaf eventList)
  = mapM_ (drawDuration c bw_mode labels_mode scale pixelDuration) 
          eventsInView
    where
    eventsInView = [e | e <- eventList, inView startPos endPos e]

-------------------------------------------------------------------------------
-- An event is in view if it is not outside the view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event
  = not (eStart > viewEnd || eEnd <= viewStart)
  where
    eStart = timeOfEventDuration event
    eEnd   = endTimeOfEventDuration event

-------------------------------------------------------------------------------

drawAverageDuration :: Int -> Bool -> Bool -> Double
		    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
		    -> Render ()
drawAverageDuration c bw_mode _ scaleValue startTime endTime _runAv gcAv
  = do setSourceRGBAhex (if not bw_mode then runningColour else black) 1.0
       draw_outlined_rectangle (oxs + startTime)      -- x
                      (oycap+c*gapcap)                -- y
                      (endTime - startTime)           -- w
                       barHeight
       setSourceRGBAhex black 1.0
       --move_to (oxs + startTime, oycap+c*gapcap)
       --relMoveTo (4/scaleValue) 13
       --unscaledText scaleValue (show nrEvents)
       setSourceRGBAhex (if not bw_mode then gcColour else black) gcRatio
       draw_rectangle (oxs + startTime)               -- x
                      (oycap+c*gapcap+barHeight)      -- y
                      (endTime - startTime)           -- w
                      (barHeight `div` 2)             -- h

    where
    duration = endTime - startTime
--    runRatio :: Double
--    runRatio = (fromIntegral runAv) / (fromIntegral duration)
    gcRatio :: Double
    gcRatio = (fromIntegral gcAv) / (fromIntegral duration)
    oxs = truncate (fromIntegral ox / scaleValue) -- x origin as Timestamp

-------------------------------------------------------------------------------

nudgeDown :: Integer -> Integer
nudgeDown n
  = if n == 0 then
      0
    else
      n-1

-------------------------------------------------------------------------------

unscaledText :: Double -> String -> Render ()
unscaledText _scaleValue text
  = do m <- getMatrix
       identityMatrix
       textPath text
       C.fill        
       setMatrix m

-------------------------------------------------------------------------------

textWidth :: Double -> String -> Render TextExtents
textWidth _scaleValue text
  = do m <- getMatrix
       identityMatrix
       tExtent <- textExtents text
       setMatrix m
       return tExtent

-------------------------------------------------------------------------------
-- For this scale value (and higher) display discrete events.

detailThreshold :: Timestamp
detailThreshold = 3000

-------------------------------------------------------------------------------

drawDuration :: Int -> Bool -> Bool -> Double -> Timestamp -> 
                EventDuration -> Render ()

drawDuration c bw_mode _labels_mode scaleValue _pixelDuration
             (ThreadRun t s startTime endTime)
  = do setSourceRGBAhex (if not bw_mode then runningColour else black) 0.8
       setLineWidth (1/scaleValue)
       draw_rectangle_opt False
                      (oxs + startTime)          -- x
                      (oycap+c*gapcap)           -- y
                      (endTime - startTime)      -- w
                       barHeight                 -- h
       -- Optionally label the bar with the threadID if there is room
       tExtent <- textWidth scaleValue tStr
       when (textExtentsWidth tExtent + 6 < fromIntegral rectWidth)
         $ do move_to (oxs + startTime, oycap+c*gapcap) 
              setSourceRGBAhex labelTextColour 1.0
              relMoveTo (4/scaleValue) 13
              unscaledText scaleValue tStr
        -- Optionally write the reason for the thread being stopped
        -- depending on the zoom value
       when False
         $ do setSourceRGBAhex black 1.0
              move_to (oxs + endTime, oycap+c*gapcap+barHeight+12)
              unscaledText scaleValue (show t ++ " " ++ showThreadStopStatus s)
    where
    rectWidth = truncate (fromIntegral (endTime - startTime) * scaleValue) -- as pixels
    tStr = show t
    oxs = truncate (fromIntegral ox / scaleValue) -- x origin as Timestamp 


drawDuration c bw_mode _ scaleValue _pixelDuration (GC startTime endTime)
  = do setSourceRGBAhex (if not bw_mode then gcColour else black) 1.0
       draw_rectangle_opt False
                      (oxs + startTime)              -- x
                      (oycap+c*gapcap+barHeight)     -- y
                      (endTime - startTime)          -- w
                      (barHeight `div` 2)            -- h
    where
    oxs = truncate (fromIntegral ox / scaleValue) -- x origin as Timestamp

drawDuration c _bw_mode labels_mode scaleValue pixelDuration (EV event)
  = case spec event of 
      CreateThread{thread=t} -> 
        when (pixelDuration <= detailThreshold) $ do
          setSourceRGBAhex lightBlue 1.0 
          setLineWidth (3/scaleValue)
          draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
          when (True && labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (oxs + time event, oycap+c*gapcap+barHeight+12)
                unscaledText scaleValue (show t ++ " created")
            )
      RunThread{}  -> return ()
      StopThread{} -> return ()
      RunSpark{} -> 
           when (pixelDuration <= detailThreshold) $
             do setSourceRGBAhex magenta 0.8 
                setLineWidth (3/scaleValue)
                draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
      ThreadRunnable{thread=t} ->
        when (pixelDuration <= detailThreshold) $ do
           setSourceRGBAhex darkGreen 0.8 
           setLineWidth (3/scaleValue)
           draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= 0.2 && not labels_mode)
            (do setSourceRGB 0.0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                unscaledText scaleValue (show t ++ " runnable")
            )
      RequestSeqGC{} -> 
        when (pixelDuration <= detailThreshold) $ do
           setSourceRGBAhex cyan 0.8 
           setLineWidth (3/scaleValue)
           draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= subscriptThreashold && not labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (oxs + time event, oycap+c*gapcap-5)
                unscaledText scaleValue ("seq GC req")
            )
      RequestParGC{} -> 
         when (pixelDuration <= detailThreshold) $ do
           setSourceRGBA 1.0 0.0 1.0 0.8 
           setLineWidth (3/scaleValue)
           draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
           when (labels_mode)
            (do setSourceRGB 0 0.0 0.0
                move_to (oxs + time event, oycap+c*gapcap-5)
                unscaledText scaleValue ("par GC req")
            )
      StartGC -> return ()
      MigrateThread {thread=t, newCap=newc}
        -> when (pixelDuration <= detailThreshold) $ do
              setSourceRGBAhex darkRed 0.8 
              setLineWidth (3/scaleValue)
              draw_line (oxs + time event, oycap+newc*gapcap-4) (oxs + time event, oycap+newc*gapcap+barHeight+4)
              when (labels_mode)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (oxs + time event, oycap+newc*gapcap+barHeight+12)
                   unscaledText scaleValue (show t ++ " migrated from " ++ show c)
               )
      WakeupThread {thread=t, otherCap=otherc}
        -> when (pixelDuration <= detailThreshold) $ do 
              setSourceRGBAhex purple 0.8 
              setLineWidth (3/scaleValue)
              draw_line (oxs + time event, oycap+c*gapcap-4) (oxs + time event, oycap+c*gapcap+barHeight+4)
              when (labels_mode)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (oxs + time event, oycap+c*gapcap+barHeight+12)
                   unscaledText scaleValue (show t ++ " woken from " ++ show otherc)
               )
      Shutdown{} ->
         do setSourceRGBAhex shutdownColour 0.8
            draw_rectangle (oxs + time event) (oycap+c*gapcap) (truncate (fromIntegral barHeight / scaleValue) :: Int) barHeight
      _ -> return () 
    where
    oxs = truncate (fromIntegral ox / scaleValue) -- x origin as Timestamp

-------------------------------------------------------------------------------

eScale :: GHCEvents.Event -> Double -> Int
eScale event scaleValue
  = truncate ((fromIntegral (time event) * scaleValue))

-------------------------------------------------------------------------------

subscriptThreashold :: Double
subscriptThreashold = 1000000  

-------------------------------------------------------------------------------

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "yielding"
showThreadStopStatus ThreadBlocked  = "blocked"
showThreadStopStatus ThreadFinished = "finished"
showThreadStopStatus ForeignCall    = "foreign call"
showThreadStopStatus _              = "unknown status"

-------------------------------------------------------------------------------
