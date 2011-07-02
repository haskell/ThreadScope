module GUI.Timeline.HEC (
    renderHEC
  ) where

import GUI.Timeline.Render.Constants

import Events.EventTree
import Events.EventDuration
import GUI.Types
import GUI.Timeline.CairoDrawing
import GUI.ViewerColours

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk

import qualified GHC.RTS.Events as GHC
import GHC.RTS.Events hiding (Event, GCWork, GCIdle)

import Control.Monad

renderHEC :: Int -> ViewParameters
          -> Timestamp -> Timestamp -> (DurationTree,EventTree)
          -> Render ()
renderHEC cap params@ViewParameters{..} start end (dtree,etree) = do
  renderDurations cap params start end dtree
  when (scaleValue < detailThreshold) $
     case etree of
       EventTree ltime etime tree ->
           renderEvents cap params ltime etime start end tree

detailThreshold :: Double
detailThreshold = 3000

-------------------------------------------------------------------------------
-- hecView draws the trace for a single HEC

renderDurations :: Int -> ViewParameters
                -> Timestamp -> Timestamp -> DurationTree
                -> Render ()

renderDurations _ _ _ _ DurationTreeEmpty = return ()

renderDurations c params@ViewParameters{..} startPos endPos (DurationTreeLeaf e)
  | inView startPos endPos e = drawDuration c params e
  | otherwise                = return ()

renderDurations !c params@ViewParameters{..} !startPos !endPos
        (DurationSplit s splitTime e lhs rhs runAv gcAv)
  | startPos < splitTime && endPos >= splitTime &&
          (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = -- View spans both left and right sub-tree.
    -- trace (printf "hecView (average): start:%d end:%d s:%d e:%d" startPos endPos s e) $
    drawAverageDuration c params s e runAv gcAv

  | otherwise
  = -- trace (printf "hecView: start:%d end:%d s:%d e:%d" startPos endPos s e) $
    do when (startPos < splitTime) $
         renderDurations c params startPos endPos lhs
       when (endPos >= splitTime) $
         renderDurations c params startPos endPos rhs

-------------------------------------------------------------------------------

renderSparkCreation :: ViewParameters -> Timestamp -> Timestamp -> SparkTree
                       -> Double -> Render ()
renderSparkCreation params !start0 !end0 t !maxSparkValue = do
  let f1 c =        SparkStats.rateDud c
      f2 c = f1 c + SparkStats.rateCreated c
      f3 c = f2 c + SparkStats.rateOverflowed c
  renderSpark params start0 end0 t
    f1 fizzledDudsColour f2 createdConvertedColour f3 overflowedColour
    maxSparkValue

renderSparkConversion :: ViewParameters -> Timestamp -> Timestamp -> SparkTree
                         -> Double -> Render ()
renderSparkConversion params !start0 !end0 t !maxSparkValue = do
  let f1 c =        SparkStats.rateFizzled c
      f2 c = f1 c + SparkStats.rateGCd c
      f3 c = f2 c + SparkStats.rateConverted c
  renderSpark params start0 end0 t
    f1 fizzledDudsColour f2 gcColour f3 createdConvertedColour
    maxSparkValue

renderSparkPool :: ViewParameters -> Timestamp -> Timestamp -> SparkTree
                         -> Double -> Render ()
renderSparkPool params@ViewParameters{..} !start0 !end0 t !maxSparkPool = do
  let slice = round (fromIntegral spark_detail * scaleValue)
      -- round the start time down, and the end time up, to a slice boundary
      start = (start0 `div` slice) * slice
      end   = ((end0 + slice) `div` slice) * slice
      prof  = sparkProfile slice start end t
      f1 c = SparkStats.minPool c
      f2 c = SparkStats.meanPool c
      f3 c = SparkStats.maxPool c
  addSparks outerPercentilesColour maxSparkPool f1 f2 start slice prof
  addSparks outerPercentilesColour maxSparkPool f2 f3 start slice prof
  -- TODO: make f2 median, not mean; add other percentiles
  outlineSparks maxSparkPool f2 start slice prof
  outlineSparks maxSparkPool (const 0) start slice prof
  when (start0 == 0) $ addScale params maxSparkPool start end

renderSpark :: ViewParameters -> Timestamp -> Timestamp -> SparkTree
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> (SparkStats.SparkStats -> Double) -> Color
               -> Double -> Render ()
renderSpark params@ViewParameters{..} start0 end0 t f1 c1 f2 c2 f3 c3 maxSparkValue = do
  let slice = round (fromIntegral spark_detail * scaleValue)
      -- round the start time down, and the end time up, to a slice boundary
      start = (start0 `div` slice) * slice
      end   = ((end0 + slice) `div` slice) * slice
      prof  = sparkProfile slice start end t
      -- Maximum number of sparks per slice for current data.
      maxSliceSpark = fromIntegral slice * maxSparkValue
      -- Maximum spark transition rate in spark/ms.
      maxSlice = maxSparkValue * 1000000
  outlineSparks maxSliceSpark f3 start slice prof
  addSparks c1 maxSliceSpark (const 0) f1 start slice prof
  addSparks c2 maxSliceSpark f1 f2 start slice prof
  addSparks c3 maxSliceSpark f2 f3 start slice prof
  when (start0 == 0) $ addScale params maxSlice start end

spark_detail :: Int
spark_detail = 4 -- in pixels

off :: Double -> (SparkStats.SparkStats -> Double)
       -> SparkStats.SparkStats
       -> Double
off maxSliceSpark f t = fromIntegral hecSparksHeight * (1 - f t / maxSliceSpark)

dashedLine1 :: Render ()
dashedLine1 = do
  save
  identityMatrix
  setDash [10,10] 0.0
  setLineWidth 1
  stroke
  restore

outlineSparks :: Double
                 -> (SparkStats.SparkStats -> Double)
                 -> Timestamp -> Timestamp
                 -> [SparkStats.SparkStats]
                 -> Render ()
outlineSparks maxSliceSpark f start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t = zip points (map (off maxSliceSpark f) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t)
      mapM_ (uncurry lineTo) t
      setSourceRGBAhex black 1.0
      save
      identityMatrix
      setLineWidth 1
      stroke
      restore

addSparks :: Color
             -> Double
             -> (SparkStats.SparkStats -> Double)
             -> (SparkStats.SparkStats -> Double)
             -> Timestamp -> Timestamp
             -> [SparkStats.SparkStats]
             -> Render ()
addSparks colour maxSliceSpark f0 f1 start slice ts = do
  case ts of
    [] -> return ()
    ts -> do
      -- liftIO $ printf "ts: %s\n" (show (map f1 (ts)))
      -- liftIO $ printf "off: %s\n" (show (map (off maxSliceSpark f1) (ts) :: [Double]))
      let dstart = fromIntegral start
          dslice = fromIntegral slice
          points = [dstart-dslice/2, dstart+dslice/2 ..]
          t0 = zip points (map (off maxSliceSpark f0) ts)
          t1 = zip points (map (off maxSliceSpark f1) ts)
      newPath
      moveTo (dstart-dslice/2) (snd $ head t1)
      mapM_ (uncurry lineTo) t1
      mapM_ (uncurry lineTo) (reverse t0)
      setSourceRGBAhex colour 1.0
      fill


-- There are ten minor ticks to a major tick and a semi-major tick
-- occurs half way through a major tick (overlapping the corresponding
-- minor tick).
-- The timestamp values are in nanos-seconds (1e-9) i.e.
-- a timestamp value of 1000000000 represents 1s.
-- The x-position on the drawing canvas is in milliseconds (ms) (1e-3).
-- scaleValue is used to divide a timestamp value to yield a pixel value.
addScale :: ViewParameters -> Double -> Timestamp -> Timestamp -> Render ()
addScale ViewParameters{..} maxSpark start end = do
  let dstart = fromIntegral start
      dend = fromIntegral end
      dheight = fromIntegral hecSparksHeight
      -- TODO: this is slightly incorrect, but probably at most 1 pixel off
      maxS = if maxSpark < 100
             then maxSpark  -- to small, accuracy would suffer
             else fromIntegral (2 * (ceiling maxSpark ` div` 2))
      -- TODO: divide maxSpark instead, for nicer round numbers display
      incr = hecSparksHeight `div` 10
      majorTick = 10 * incr
  newPath
  moveTo dstart 0
  lineTo dstart dheight
  setSourceRGBAhex blue 1.0
  save
  identityMatrix
  setLineWidth 1
  stroke
  restore

  setSourceRGBAhex black 0.3
  save
  forM_ [0 .. 1] $ \h -> do
    let y = fromIntegral (floor (fromIntegral h * fromIntegral majorTick / 2)) - 0.5
    moveTo dstart y
    lineTo dend y
    dashedLine1
  restore

  selectFontFace "sans serif" FontSlantNormal FontWeightNormal
  setFontSize 12
  setSourceRGBAhex blue 1.0
  save
  scale scaleValue 1.0
  setLineWidth 0.5
  drawTicks maxS start scaleValue 0 incr majorTick hecSparksHeight
  restore

-- TODO: make it more robust when parameters change, e.g., if incr is too small
drawTicks :: Double -> Timestamp -> Double -> Int -> Int -> Int -> Int -> Render ()
drawTicks maxS offset scaleValue pos incr majorTick endPos
  = if pos <= endPos then do
      draw_line (x0, hecSparksHeight - y0) (x1, hecSparksHeight - y1)
      when (pos > 0
            && (atMajorTick || atMidTick || tickWidthInPixels > 30)) $ do
            move_to (offset + 15,
                     fromIntegral hecSparksHeight - pos + 4)
            m <- getMatrix
            identityMatrix
            tExtent <- textExtents tickText
            (fourPixels, _) <- deviceToUserDistance 4 0
            when (textExtentsWidth tExtent + fourPixels < fromIntegral tickWidthInPixels || atMidTick || atMajorTick) $ do
              textPath tickText
              C.fill
            setMatrix m
      drawTicks maxS offset scaleValue (pos+incr) incr majorTick endPos
    else
      return ()
    where
    tickWidthInPixels :: Int
    tickWidthInPixels = truncate ((fromIntegral incr) / scaleValue)
    tickText = showTickText (maxS * fromIntegral pos
                             / fromIntegral hecSparksHeight)
    atMidTick = pos `mod` (majorTick `div` 2) == 0
    atMajorTick = pos `mod` majorTick == 0
    (x0, y0, x1, y1) = if atMajorTick then
                         (offset, pos, offset+13, pos)
                       else
                         if atMidTick then
                           (offset, pos, offset+10, pos)
                         else
                           (offset, pos, offset+6, pos)

showTickText :: Double -> String
showTickText pos
  = deZero (printf "%.2f" pos)

deZero :: String -> String
deZero str
  = if length str >= 4 && take 3 revstr == "00." then
      reverse (drop 3 revstr)
    else
      str
    where
    revstr = reverse str

-------------------------------------------------------------------------------

renderEvents :: Int -> ViewParameters
             -> Timestamp -- start time of this tree node
             -> Timestamp -- end   time of this tree node
             -> Timestamp -> Timestamp -> EventNode
             -> Render ()

renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventTreeLeaf es)
  = sequence_ [ drawEvent c params e
              | e <- es, let t = time e, t >= startPos && t < endPos ]
renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventTreeOne ev)
  | t >= startPos && t < endPos = drawEvent c params ev
  | otherwise = return ()
  where t = time ev

renderEvents !c params@ViewParameters{..} !s !e !startPos !endPos
        (EventSplit splitTime lhs rhs)
  | startPos < splitTime && endPos >= splitTime &&
        (fromIntegral (e - s) / scaleValue) <= fromIntegral detail
  = drawTooManyEvents c params s e

  | otherwise
  = do when (startPos < splitTime) $
         renderEvents c params s splitTime startPos endPos lhs
       when (endPos >= splitTime) $
         renderEvents c params splitTime e startPos endPos rhs

-------------------------------------------------------------------------------
-- An event is in view if it is not outside the view.

inView :: Timestamp -> Timestamp -> EventDuration -> Bool
inView viewStart viewEnd event
  = not (eStart > viewEnd || eEnd <= viewStart)
  where
    eStart = startTimeOf event
    eEnd   = endTimeOf event

-------------------------------------------------------------------------------

drawAverageDuration :: Int -> ViewParameters
                    -> Timestamp -> Timestamp -> Timestamp -> Timestamp
                    -> Render ()
drawAverageDuration c ViewParameters{..} startTime endTime runAv gcAv
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

drawDuration :: Int -> ViewParameters -> EventDuration -> Render ()

drawDuration c ViewParameters{..}
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

drawDuration c ViewParameters{..} (GCStart startTime endTime)
  = gcBar (if bwMode then black else gcStartColour) startTime endTime

drawDuration c ViewParameters{..} (GCWork startTime endTime)
  = gcBar (if bwMode then black else gcWorkColour) startTime endTime

drawDuration c ViewParameters{..} (GCIdle startTime endTime)
  = gcBar (if bwMode then black else gcIdleColour) startTime endTime

drawDuration c ViewParameters{..} (GCEnd startTime endTime)
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
       textPath str
       C.fill
       restore

drawEvent :: Int -> ViewParameters -> GHC.Event -> Render ()
drawEvent c params@ViewParameters{..} event
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


drawTooManyEvents :: Int -> ViewParameters -> Timestamp -> Timestamp
                  -> Render ()
drawTooManyEvents c params@ViewParameters{..} start end = do
     return ()
--     setSourceRGBAhex grey 1.0
--     setLineWidth (3 * scaleValue)
--     draw_rectangle start (hecBarOff-4) (end - start) 4
--     draw_rectangle start (hecBarOff+hecBarHeight) (end - start) 4

-------------------------------------------------------------------------------
