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

import Control.Monad

renderHEC :: ViewParameters
          -> Timestamp -> Timestamp -> (DurationTree,EventTree)
          -> Render ()
renderHEC params@ViewParameters{..} start end (dtree,etree) = do
  renderDurations params start end dtree
  when (scaleValue < detailThreshold) $
     case etree of
       EventTree ltime etime tree ->
           renderEvents params ltime etime start end tree

renderInstantHEC :: ViewParameters -> Timestamp -> Timestamp
                    -> EventTree
                    -> Render ()
renderInstantHEC params@ViewParameters{..} start end
                 (EventTree ltime etime tree) =
  renderEvents params ltime etime start end tree

detailThreshold :: Double
detailThreshold = 3

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

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
    -- trace (printf "hecView (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration params s e runAv gcAv

  | otherwise
  = -- trace (printf "hecView: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         renderDurations params startPos endPos lhs
       when (endPos >= splitTime) $
         renderDurations params startPos endPos rhs

-------------------------------------------------------------------------------

renderEvents :: ViewParameters
             -> Timestamp -- start time of this tree node
             -> Timestamp -- end   time of this tree node
             -> Timestamp -> Timestamp -> EventNode
             -> Render ()

renderEvents params@ViewParameters{..} !_s !_e !startPos !endPos
        (EventTreeLeaf es)
  = sequence_ [ drawEvent params e
              | e <- es, let t = time e, t >= startPos && t < endPos ]
renderEvents params@ViewParameters{..} !_s !_e !startPos !endPos
        (EventTreeOne ev)
  | t >= startPos && t < endPos = drawEvent params ev
  | otherwise = return ()
  where t = time ev

renderEvents params@ViewParameters{..} !s !e !startPos !endPos
        (EventSplit splitTime lhs rhs)
  | startPos < splitTime && endPos >= splitTime &&
        (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = drawTooManyEvents params s e

  | otherwise
  = do when (startPos < splitTime) $
         renderEvents params s splitTime startPos endPos lhs
       when (endPos >= splitTime) $
         renderEvents params splitTime e startPos endPos rhs

-------------------------------------------------------------------------------
-- An event is in view if it is not outside the view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event
  = not (eStart > viewEnd || eEnd <= viewStart)
  where
    eStart = startTimeOf event
    eEnd   = endTimeOf event

-------------------------------------------------------------------------------

drawAverageDuration :: ViewParameters
                    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
                    -> Render ()
drawAverageDuration ViewParameters{..} startTime endTime runAv gcAv
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
       when (runAv > 0) $
         draw_rectangle startTime hecBarOff -- x, y
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

drawDuration ViewParameters{..}
                (ThreadRun t s startTime endTime)
  = do setSourceRGBAhex (if not bwMode then runningColour else black) 1.0
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

drawDuration ViewParameters{..} (GCStart startTime endTime)
  = gcBar (if bwMode then black else gcStartColour) startTime endTime

drawDuration ViewParameters{..} (GCWork startTime endTime)
  = gcBar (if bwMode then black else gcWorkColour) startTime endTime

drawDuration ViewParameters{..} (GCIdle startTime endTime)
  = gcBar (if bwMode then black else gcIdleColour) startTime endTime

drawDuration ViewParameters{..} (GCEnd startTime endTime)
  = gcBar (if bwMode then black else gcEndColour) startTime endTime

gcBar :: Color -> Timestamp -> Timestamp -> Render ()
gcBar col !startTime !endTime
  = do setSourceRGBAhex col 1.0
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

drawEvent :: ViewParameters -> GHC.Event -> Render ()
drawEvent params@ViewParameters{..} event
  = case spec event of
      CreateThread{}   -> renderInstantEvent params event createThreadColour
      ThreadRunnable{} -> renderInstantEvent params event threadRunnableColour
      RequestSeqGC{}   -> renderInstantEvent params event seqGCReqColour
      RequestParGC{}   -> renderInstantEvent params event parGCReqColour
      MigrateThread{}  -> renderInstantEvent params event migrateThreadColour
      WakeupThread{}   -> renderInstantEvent params event threadRunnableColour
      Shutdown{}       -> renderInstantEvent params event shutdownColour

      SparkCreate{}    -> renderInstantEvent params event createdConvertedColour
      SparkDud{}       -> renderInstantEvent params event fizzledDudsColour
      SparkOverflow{}  -> renderInstantEvent params event overflowedColour
      SparkRun{}       -> renderInstantEvent params event createdConvertedColour
      SparkSteal{}     -> renderInstantEvent params event createdConvertedColour
      SparkFizzle{}    -> renderInstantEvent params event fizzledDudsColour
      SparkGC{}        -> renderInstantEvent params event fizzledDudsColour

      UserMessage{}    -> renderInstantEvent params event userMessageColour

      RunThread{}  -> return ()
      StopThread{} -> return ()
      StartGC{}    -> return ()

      _ -> return ()

renderInstantEvent :: ViewParameters -> GHC.Event -> Color -> Render ()
renderInstantEvent ViewParameters{..} event color = do
     setSourceRGBAhex color 1.0
     setLineWidth (3 * scaleValue)
     let t = time event
     draw_line (t, hecBarOff-4) (t, hecBarOff+hecBarHeight+4)
     labelAt labelsMode t $ showEventInfo (spec event)


drawTooManyEvents :: ViewParameters -> Timestamp -> Timestamp
                  -> Render ()
drawTooManyEvents _params@ViewParameters{..} _start _end = do
     return ()
--     setSourceRGBAhex grey 1.0
--     setLineWidth (3 * scaleValue)
--     draw_rectangle start (hecBarOff-4) (end - start) 4
--     draw_rectangle start (hecBarOff+hecBarHeight) (end - start) 4

-------------------------------------------------------------------------------
