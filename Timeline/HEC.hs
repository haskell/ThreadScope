module Timeline.HEC (
    renderHEC
  ) where

import Timeline.Render.Constants

import EventTree
import EventDuration
import State
import CairoDrawing
import ViewerColours

import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event, GCWork, GCIdle)

import Control.Monad

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

renderHEC :: Int -> ViewParameters
          -> Timestamp -> Timestamp -> EventTree -> Render ()

renderHEC !c params@ViewParameters{..} !startPos !endPos
        (EventSplit s splitTime e _ _ nrEvents runAv gcAv) 
        | startPos < splitTime && endPos >= splitTime && 
	  (fromIntegral (e - s) / scaleValue) <= fromIntegral detail &&
          nrEvents > 2
  = -- View spans both left and right sub-tree.
    -- trace (printf "hecView (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration c params s e runAv gcAv

renderHEC c params@ViewParameters{..} startPos endPos
       (EventSplit _ splitTime _ lhs rhs _ _ _)
  = -- trace (printf "hecView: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         renderHEC c params startPos endPos lhs
       when (endPos >= splitTime) $
         renderHEC c params startPos endPos rhs

renderHEC c  params@ViewParameters{..} startPos endPos
       (EventTreeLeaf eventList)
  = mapM_ (drawDuration c params) eventsInView
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

drawAverageDuration :: Int -> ViewParameters
		    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
		    -> Render ()
drawAverageDuration c ViewParameters{..} startTime endTime runAv gcAv
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
       when (runAv > 0) $
         draw_outlined_rectangle startTime hecBarOff -- x, y
                        (endTime - startTime)        -- w
                         hecBarHeight
       setSourceRGBAhex black 1.0
       --move_to (oxs + startTime, 0)
       --relMoveTo (4/scaleValue) 13
       --unscaledText scaleValue (show nrEvents)
       setSourceRGBAhex (if not bwMode then gcColour else black) gcRatio
       draw_rectangle startTime      -- x
                      (hecBarOff+hecBarHeight)      -- y
                      (endTime - startTime)           -- w
                      (hecBarHeight `div` 2)             -- h

    where
    duration = endTime - startTime
--    runRatio :: Double
--    runRatio = (fromIntegral runAv) / (fromIntegral duration)
    gcRatio :: Double
    gcRatio = (fromIntegral gcAv) / (fromIntegral duration)

-------------------------------------------------------------------------------

unscaledText :: String -> Render ()
unscaledText text
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

detailThreshold :: Double
detailThreshold = 3000

-------------------------------------------------------------------------------

drawDuration :: Int -> ViewParameters -> EventDuration -> Render ()

drawDuration c ViewParameters{..}
                (ThreadRun t s startTime endTime)
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 0.8
       setLineWidth (1/scaleValue)
       draw_rectangle_opt False
                      startTime                  -- x
                      hecBarOff                  -- y
                      (endTime - startTime)      -- w
                      hecBarHeight               -- h
       -- Optionally label the bar with the threadID if there is room
       tExtent <- textWidth scaleValue tStr
       let tw = textExtentsWidth  tExtent
           th = textExtentsHeight tExtent
       when (tw + 6 < fromIntegral rectWidth)
         $ do setSourceRGBAhex labelTextColour 1.0
              move_to (fromIntegral startTime + truncate (4*scaleValue),
                       hecBarOff + (hecBarHeight + round th) `quot` 2)
              unscaledText tStr

        -- Optionally write the reason for the thread being stopped
        -- depending on the zoom value
       labelAt labelsMode endTime $ 
             show t ++ " " ++ showThreadStopStatus s
    where
    rectWidth = truncate (fromIntegral (endTime - startTime) / scaleValue) -- as pixels
    tStr = show t

drawDuration c ViewParameters{..} (GCStart startTime endTime)
  = gcBar (if bwMode then black else gcStartColour) startTime endTime

drawDuration c ViewParameters{..} (GCWork startTime endTime)
  = gcBar (if bwMode then black else gcWorkColour) startTime endTime

drawDuration c ViewParameters{..} (GCIdle startTime endTime)
  = gcBar (if bwMode then black else gcIdleColour) startTime endTime

drawDuration c ViewParameters{..} (GCEnd startTime endTime)
  = gcBar (if bwMode then black else gcEndColour) startTime endTime

drawDuration c params@ViewParameters{..} (EV event)
  = case spec event of 
      CreateThread{}   -> renderInstantEvent params event createThreadColour
      RunSpark{}       -> renderInstantEvent params event runSparkColour
      StealSpark{}     -> renderInstantEvent params event stealSparkColour
      ThreadRunnable{} -> renderInstantEvent params event threadRunnableColour
      RequestSeqGC{}   -> renderInstantEvent params event seqGCReqColour
      RequestParGC{}   -> renderInstantEvent params event parGCReqColour
      MigrateThread{}  -> renderInstantEvent params event migrateThreadColour
      WakeupThread{}   -> renderInstantEvent params event threadRunnableColour
      Shutdown{}       -> renderInstantEvent params event shutdownColour

      RunThread{}  -> return ()
      StopThread{} -> return ()
      StartGC{}    -> return ()

      _ -> return () 

gcBar :: Color -> Timestamp -> Timestamp -> Render ()
gcBar col !startTime !endTime
  = do setSourceRGBAhex col 1.0
       draw_rectangle_opt False
                      startTime                      -- x
                      (hecBarOff+hecBarHeight)       -- y
                      (endTime - startTime)          -- w
                      (hecBarHeight `div` 2)         -- h

renderInstantEvent :: ViewParameters -> GHC.Event -> Color -> Render ()
renderInstantEvent ViewParameters{..} event color = 
  when (scaleValue <= detailThreshold) $ do
     setSourceRGBAhex color 1.0 
     setLineWidth (3 * scaleValue)
     let t = time event
     draw_line (t, hecBarOff-4) (t, hecBarOff+hecBarHeight+4)
     labelAt labelsMode t $ showEventTypeSpecificInfo (spec event)

labelAt :: Bool -> Timestamp -> String -> Render ()
labelAt labelsMode t str
  | not labelsMode = return ()
  | otherwise = do
       setSourceRGB 0.0 0.0 0.0
       move_to (t, hecBarOff+hecBarHeight+12)
       save
       identityMatrix
       rotate (pi/4)
       textPath str
       C.fill        
       restore
                    
-------------------------------------------------------------------------------
