module GUI.Timeline.Activity (
      renderActivity
  ) where

import GUI.Timeline.Render.Constants

import Events.HECs
import Events.EventTree
import Events.EventDuration
import GUI.Types
import GUI.ViewerColours

import Graphics.Rendering.Cairo

import Control.Monad
import Data.List

-- ToDo:
--  - we average over the slice, but the point is drawn at the beginning
--    of the slice rather than in the middle.

-----------------------------------------------------------------------------

renderActivity :: ViewParameters -> HECs -> Timestamp -> Timestamp
               -> Render ()

renderActivity ViewParameters{..} hecs start0 end0 = do
  let
      slice = ceiling (fromIntegral activity_detail * scaleValue)

      -- round the start time down, and the end time up, to a slice boundary
      start = (start0 `div` slice) * slice
      end   = ((end0 + slice) `div` slice) * slice

      hec_profs  = map (actProfile slice start end)
                     (map (\ (t, _, _) -> t) (hecTrees hecs))
      total_prof = map sum (transpose hec_profs)

--  liftIO $ printf "%s\n" (show (map length hec_profs))
--  liftIO $ printf "%s\n" (show (map (take 20) hec_profs))
  drawActivity hecs start end slice total_prof
               (if not bwMode then runningColour else black)

activity_detail :: Int
activity_detail = 4 -- in pixels

-- for each timeslice, the amount of time spent in the mutator
-- during that period.
actProfile :: Timestamp -> Timestamp -> Timestamp -> DurationTree -> [Timestamp]
actProfile slice start0 end0 t
  = {- trace (show flat) $ -} chopped

  where
   -- do an extra slice at both ends
   start = if start0 < slice then start0 else start0 - slice
   end   = end0 + slice

   flat = flatten start t []
   chopped0 = chop 0 start flat

   chopped | start0 < slice = 0 : chopped0
           | otherwise      = chopped0

   flatten :: Timestamp -> DurationTree -> [DurationTree] -> [DurationTree]
   flatten _start DurationTreeEmpty rest = rest
   flatten start t@(DurationSplit s split e l r _run _) rest
     | e   <= start   = rest
     | end <= s       = rest
     | start >= split = flatten start r rest
     | end   <= split = flatten start l rest
     | e - s > slice  = flatten start l $ flatten start r rest
     | otherwise      = t : rest
   flatten _start t@(DurationTreeLeaf _) rest
     = t : rest

   chop :: Timestamp -> Timestamp -> [DurationTree] -> [Timestamp]
   chop sofar start _ts
     | start >= end = if sofar > 0 then [sofar] else []
   chop sofar start []
     = sofar : chop 0 (start+slice) []
   chop sofar start (t : ts)
     | e <= start
     = if sofar /= 0
          then error "chop"
          else chop sofar start ts
     | s >= start + slice
     = sofar : chop 0 (start + slice) (t : ts)
     | e > start + slice
     = (sofar + time_in_this_slice t) : chop 0 (start + slice) (t : ts)
     | otherwise
     = chop (sofar + time_in_this_slice t) start ts
    where
      (s, e)
        | DurationTreeLeaf ev <- t           = (startTimeOf ev, endTimeOf ev)
        | DurationSplit s _ e _ _ _run _ <- t = (s, e)

      mi = min (start + slice) e
      ma = max start s
      duration = if mi < ma then 0 else mi - ma

      time_in_this_slice t = case t of
        DurationTreeLeaf ThreadRun{}  -> duration
        DurationTreeLeaf _            -> 0
        DurationSplit _ _ _ _ _ run _ ->
          round (fromIntegral (run * duration) / fromIntegral (e-s))
        DurationTreeEmpty             -> error "time_in_this_slice"

drawActivity :: HECs -> Timestamp -> Timestamp -> Timestamp -> [Timestamp]
             -> Color
             -> Render ()
drawActivity hecs start end slice ts color = do
  case ts of
   [] -> return ()
   t:ts -> do
--     liftIO $ printf "ts: %s\n" (show (t:ts))
--     liftIO $ printf "off: %s\n" (show (map off (t:ts) :: [Double]))
     let dstart = fromIntegral start
         dend   = fromIntegral end
         dslice = fromIntegral slice
         dheight = fromIntegral activityGraphHeight

-- funky gradients don't seem to work:
--     withLinearPattern 0 0 0 dheight $ \pattern -> do
--        patternAddColorStopRGB pattern 0   0.8 0.8 0.8
--        patternAddColorStopRGB pattern 1.0 1.0 1.0 1.0
--        rectangle dstart 0 dend dheight
--        setSource pattern
--        fill

     newPath
     moveTo (dstart-dslice/2) (off t)
     zipWithM_ lineTo (tail [dstart-dslice/2, dstart+dslice/2 ..]) (map off ts)
     setSourceRGBAhex black 1.0
     setLineWidth 1
     strokePreserve

     lineTo dend   dheight
     lineTo dstart dheight
     setSourceRGBAhex color 1.0
     fill

-- funky gradients don't seem to work:
--      save
--      withLinearPattern 0 0 0 dheight $ \pattern -> do
--        patternAddColorStopRGB pattern 0   0   1.0 0
--        patternAddColorStopRGB pattern 1.0 1.0 1.0 1.0
--        setSource pattern
-- --       identityMatrix
-- --       setFillRule FillRuleEvenOdd
--        fillPreserve
--      restore

     save
     forM_ [0 .. hecCount hecs - 1] $ \h -> do
       let y = fromIntegral (floor (fromIntegral h * dheight / fromIntegral (hecCount hecs))) - 0.5
       setSourceRGBAhex black 0.3
       moveTo dstart y
       lineTo dend y
       dashedLine1
     restore

 where
  off t = fromIntegral activityGraphHeight -
            fromIntegral (t * fromIntegral activityGraphHeight) /
            fromIntegral (fromIntegral (hecCount hecs) * slice)

-- | Draw a dashed line along the current path.
dashedLine1 :: Render ()
dashedLine1 = do
  save
  identityMatrix
  let dash = fromIntegral ox
  setDash [dash, dash] 0.0
  setLineWidth 1
  stroke
  restore
