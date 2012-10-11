module GUI.Timeline.HEC (
    renderHEC,
    renderInstantHEC,
  ) where

import GUI.Timeline.Render.Constants

import Events.EventTree
import Events.EventDuration
import GUI.Types
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event, GCWork, GCIdle)

import qualified Data.IntMap as IM
import Data.Maybe
import Control.Monad

renderHEC :: ViewParameters -> Timestamp -> Timestamp
          -> IM.IntMap String -> (DurationTree,EventTree)
          -> Render ()
renderHEC params@ViewParameters{..} start end perfNames (dtree,etree) = do
  renderDurations params start end dtree
  when (scaleValue < detailThreshold) $
     case etree of
       EventTree ltime etime tree -> do
         renderEvents params ltime etime start end (fromIntegral detail)
           perfNames tree
         return ()

renderInstantHEC :: ViewParameters -> Timestamp -> Timestamp
                 -> IM.IntMap String -> EventTree
                 -> Render ()
renderInstantHEC params@ViewParameters{..} start end
                 perfNames (EventTree ltime etime tree) = do
  let instantDetail = 1
  renderEvents params ltime etime start end instantDetail perfNames tree
  return ()

detailThreshold :: Double
detailThreshold = 3

-------------------------------------------------------------------------------
-- draws the trace for a single HEC

renderDurations :: ViewParameters
                -> Timestamp -> Timestamp -> DurationTree
                -> Render ()

renderDurations _ _ _ DurationTreeEmpty = return ()

renderDurations params@ViewParameters{..} startPos endPos (DurationTreeLeaf e)
  | inView startPos endPos e = drawDuration params e
  | otherwise                = return ()

renderDurations params@ViewParameters{..} !startPos !endPos
        (DurationSplit s splitTime e lhs rhs runAv gcAv)
  | startPos < splitTime && endPos >= splitTime &&
          (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = -- View spans both left and right sub-tree.
    -- trace (printf "renderDurations (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration params s e runAv gcAv

  | otherwise
  = -- trace (printf "renderDurations: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         renderDurations params startPos endPos lhs
       when (endPos >= splitTime) $
         renderDurations params startPos endPos rhs

-------------------------------------------------------------------------------

renderEvents :: ViewParameters
             -> Timestamp -- start time of this tree node
             -> Timestamp -- end   time of this tree node
             -> Timestamp -> Timestamp -> Double
             -> IM.IntMap String -> EventNode
             -> Render Bool

renderEvents params@ViewParameters{..} !_s !_e !startPos !endPos ewidth
             perfNames (EventTreeLeaf es)
  = let within = [ e | e <- es, let t = time e, t >= startPos && t < endPos ]
        untilTrue _ [] = return False
        untilTrue f (x : xs) = do
          b <- f x
          if b then return b else untilTrue f xs
    in untilTrue (drawEvent params ewidth perfNames) within

renderEvents params@ViewParameters{..} !_s !_e !startPos !endPos ewidth
        perfNames (EventTreeOne ev)
  | t >= startPos && t < endPos = drawEvent params ewidth perfNames ev
  | otherwise = return False
  where t = time ev

renderEvents params@ViewParameters{..} !s !e !startPos !endPos ewidth
        perfNames (EventSplit splitTime lhs rhs)
  | startPos < splitTime && endPos >= splitTime &&
        (fromIntegral (e - s) / scaleValue) <= ewidth
  = do drawnLhs <-
           renderEvents params s splitTime startPos endPos ewidth perfNames lhs
       if not drawnLhs
         then
           renderEvents params splitTime e startPos endPos ewidth perfNames rhs
         else return True
  | otherwise
  = do drawnLhs <-
         if startPos < splitTime
         then
           renderEvents params s splitTime startPos endPos ewidth perfNames lhs
         else return False
       drawnRhs <-
         if endPos >= splitTime
         then
           renderEvents params splitTime e startPos endPos ewidth perfNames rhs
         else return False
       return $ drawnLhs || drawnRhs

-------------------------------------------------------------------------------
-- An event is in view if it is not outside the view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event =
  not (eStart > viewEnd || eEnd <= viewStart)
 where
  eStart = startTimeOf event
  eEnd   = endTimeOf event

-------------------------------------------------------------------------------

drawAverageDuration :: ViewParameters
                    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
                    -> Render ()
drawAverageDuration ViewParameters{..} startTime endTime runAv gcAv = do
  setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
  when (runAv > 0) $
    draw_rectangle startTime hecBarOff         -- x, y
                   (endTime - startTime)       -- w
                    hecBarHeight
  setSourceRGBAhex black 1.0
  --move_to (oxs + startTime, 0)
  --relMoveTo (4/scaleValue) 13
  --unscaledText scaleValue (show nrEvents)
  setSourceRGBAhex (if not bwMode then gcColour else black) gcRatio
  draw_rectangle startTime      -- x
                 (hecBarOff+hecBarHeight)      -- y
                 (endTime - startTime)         -- w
                 (hecBarHeight `div` 2)        -- h

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
       showText text
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

drawDuration :: ViewParameters -> EventDuration -> Render ()
drawDuration ViewParameters{..} (ThreadRun t s startTime endTime) = do
  setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
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
  when (tw + 6 < fromIntegral rectWidth) $ do
    setSourceRGBAhex labelTextColour 1.0
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

drawDuration ViewParameters{..} (GCStart startTime endTime)
  = gcBar (if bwMode then black else gcStartColour) startTime endTime

drawDuration ViewParameters{..} (GCWork startTime endTime)
  = gcBar (if bwMode then black else gcWorkColour) startTime endTime

drawDuration ViewParameters{..} (GCIdle startTime endTime)
  = gcBar (if bwMode then black else gcIdleColour) startTime endTime

drawDuration ViewParameters{..} (GCEnd startTime endTime)
  = gcBar (if bwMode then black else gcEndColour) startTime endTime

gcBar :: Color -> Timestamp -> Timestamp -> Render ()
gcBar col !startTime !endTime = do
  setSourceRGBAhex col 1.0
  draw_rectangle_opt False
                     startTime                      -- x
                     (hecBarOff+hecBarHeight)       -- y
                     (endTime - startTime)          -- w
                     (hecBarHeight `div` 2)         -- h

labelAt :: Bool -> Timestamp -> String -> Render ()
labelAt labelsMode t str
  | not labelsMode = return ()
  | otherwise = do
       setSourceRGB 0.0 0.0 0.0
       move_to (t, hecBarOff+hecBarHeight+12)
       save
       identityMatrix
       rotate (pi/4)
       showText str
       restore

drawEvent :: ViewParameters -> Double -> IM.IntMap String -> GHC.Event
          -> Render Bool
drawEvent params@ViewParameters{..} ewidth perfNames event =
  let renderI = renderInstantEvent params perfNames event ewidth
  in case spec event of
    CreateThread{}  -> renderI createThreadColour
    RequestSeqGC{}  -> renderI seqGCReqColour
    RequestParGC{}  -> renderI parGCReqColour
    MigrateThread{} -> renderI migrateThreadColour
    WakeupThread{}  -> renderI threadWakeupColour
    Shutdown{}      -> renderI shutdownColour

    SparkCreate{}   -> renderI createdConvertedColour
    SparkDud{}      -> renderI fizzledDudsColour
    SparkOverflow{} -> renderI overflowedColour
    SparkRun{}      -> renderI createdConvertedColour
    SparkSteal{}    -> renderI createdConvertedColour
    SparkFizzle{}   -> renderI fizzledDudsColour
    SparkGC{}       -> renderI gcColour

    UserMessage{}   -> renderI userMessageColour

    PerfCounter{}    -> renderI createdConvertedColour
    PerfTracepoint{} -> renderI shutdownColour
    PerfName{}       -> return False

    RunThread{}  -> return False
    StopThread{} -> return False
    StartGC{}    -> return False

    _ -> return False

renderInstantEvent :: ViewParameters -> IM.IntMap String -> GHC.Event
                   -> Double -> Color
                   -> Render Bool
renderInstantEvent ViewParameters{..} perfNames event ewidth color = do
  setSourceRGBAhex color 1.0
  setLineWidth (ewidth * scaleValue)
  let t = time event
  draw_line (t, hecBarOff-4) (t, hecBarOff+hecBarHeight+4)
  let numToLabel PerfCounter{perfNum, period} | period == 0 =
        IM.lookup (fromIntegral perfNum) perfNames
      numToLabel PerfCounter{perfNum, period} =
        fmap (++ " <" ++ show (period + 1) ++ " times>") $
          IM.lookup (fromIntegral perfNum) perfNames
      numToLabel PerfTracepoint{perfNum} =
        fmap ("tracepoint: " ++) $ IM.lookup (fromIntegral perfNum) perfNames
      numToLabel _ = Nothing
      showLabel espec = fromMaybe (showEventInfo espec) (numToLabel espec)
  labelAt labelsMode t $ showLabel (spec event)
  return True

-------------------------------------------------------------------------------
