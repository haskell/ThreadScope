module GUI.Timeline.Motion (
    zoomIn, zoomOut, zoomToFit,
    scrollLeft, scrollRight, scrollToBeginning, scrollToEnd, scrollTo, centreOnCursor,
    vscrollDown, vscrollUp,
  ) where

import GUI.Timeline.Types
import GUI.Timeline.Sparks
import Events.HECs

import Graphics.UI.Gtk

import Data.IORef
import Control.Monad
-- import Text.Printf
-- import Debug.Trace

-------------------------------------------------------------------------------
-- Zoom in works by expanding the current view such that the
-- left hand edge of the original view remains at the same
-- position and the zoom in factor is 2.
-- For example, zoom into the time range 1.0 3.0
-- produces a new view with the time range 1.0 2.0

zoomIn :: TimelineState -> Timestamp -> IO ()
zoomIn  = zoom (/2)

zoomOut :: TimelineState -> Timestamp -> IO ()
zoomOut  = zoom (*2)

zoom :: (Double -> Double) -> TimelineState -> Timestamp -> IO ()
zoom factor TimelineState{timelineAdj, scaleIORef} cursor = do
  scaleValue <- readIORef scaleIORef
  -- TODO: we'd need HECs, as below, to fit maxScale to graphs at hand
  let maxScale = 10000000000  -- big enough for hours of eventlogs
      clampedFactor =
        if factor scaleValue < 0.2 || factor scaleValue > maxScale
        then id
        else factor
      newScaleValue = clampedFactor scaleValue
  writeIORef scaleIORef newScaleValue

  hadj_value <- adjustmentGetValue timelineAdj
  hadj_pagesize <- adjustmentGetPageSize timelineAdj -- Get size of bar

  let newPageSize = clampedFactor hadj_pagesize
  adjustmentSetPageSize timelineAdj newPageSize

  let cursord = fromIntegral cursor
  when (cursord >= hadj_value && cursord < hadj_value + hadj_pagesize) $
    adjustmentSetValue timelineAdj $
        cursord - clampedFactor (cursord - hadj_value)

  let pageshift = 0.9 * newPageSize
  let nudge     = 0.1 * newPageSize

  adjustmentSetStepIncrement timelineAdj nudge
  adjustmentSetPageIncrement timelineAdj pageshift

-------------------------------------------------------------------------------

zoomToFit :: TimelineState -> Maybe HECs -> IO ()
zoomToFit TimelineState{scaleIORef, maxSpkIORef,timelineAdj,
                        timelineDrawingArea} mb_hecs = do
  case mb_hecs of
    Nothing   -> return ()
    Just hecs -> do
      let lastTx = hecLastEventTime hecs
          upper = fromIntegral lastTx
          lower = 0
      (w, _) <- widgetGetSize timelineDrawingArea
      let newScaleValue = upper / fromIntegral w
          (sliceAll, profAll) = treesProfile newScaleValue 0 lastTx hecs
          -- TODO: verify that no empty lists possible below
          maxmap l = maximum (0 : map (maxSparkRenderedValue sliceAll) l)
          maxAll = map maxmap profAll
          newMaxSpkValue = maximum (0 : maxAll)

      writeIORef scaleIORef newScaleValue
      writeIORef maxSpkIORef newMaxSpkValue

      -- Configure the horizontal scrollbar units to correspond to micro-secs.
      adjustmentSetLower    timelineAdj lower
      adjustmentSetValue    timelineAdj lower
      adjustmentSetUpper    timelineAdj upper
      adjustmentSetPageSize timelineAdj upper
      -- TODO: this seems suspicious:
      adjustmentSetStepIncrement timelineAdj 0
      adjustmentSetPageIncrement timelineAdj 0

-------------------------------------------------------------------------------

scrollLeft, scrollRight, scrollToBeginning, scrollToEnd :: TimelineState -> IO ()

scrollLeft        = scroll (\val page l _ -> l `max` (val - page/2))
scrollRight       = scroll (\val page _ u -> (u - page) `min` (val + page/2))
scrollToBeginning = scroll (\_   _    l _ ->  l)
scrollToEnd       = scroll (\_   _    _ u ->  u)

scrollTo :: TimelineState -> Double -> IO ()
scrollTo s x      = scroll (\_   _    _ _ ->  x) s

centreOnCursor :: TimelineState -> Timestamp -> IO ()

centreOnCursor state cursor =
  scroll (\_ page l _u -> max l (fromIntegral cursor - page/2)) state

scroll :: (Double -> Double -> Double -> Double -> Double)
       -> TimelineState -> IO ()
scroll adjust TimelineState{timelineAdj} = do
  hadj_value <- adjustmentGetValue timelineAdj
  hadj_pagesize <- adjustmentGetPageSize timelineAdj
  hadj_lower <- adjustmentGetLower timelineAdj
  hadj_upper <- adjustmentGetUpper timelineAdj
  let newValue = adjust hadj_value hadj_pagesize hadj_lower hadj_upper
      newValue' = max hadj_lower (min (hadj_upper - hadj_pagesize) newValue)
  adjustmentSetValue timelineAdj newValue'

vscrollDown, vscrollUp :: TimelineState -> IO ()
vscrollDown = vscroll (\val page _l  u -> (u - page) `min` (val + page/8))
vscrollUp   = vscroll (\val page  l _u -> l `max` (val - page/8))

vscroll :: (Double -> Double -> Double -> Double -> Double)
        -> TimelineState -> IO ()
vscroll adjust TimelineState{timelineVAdj} = do
  hadj_value <- adjustmentGetValue timelineVAdj
  hadj_pagesize <- adjustmentGetPageSize timelineVAdj
  hadj_lower <- adjustmentGetLower timelineVAdj
  hadj_upper <- adjustmentGetUpper timelineVAdj
  let newValue = adjust hadj_value hadj_pagesize hadj_lower hadj_upper
  adjustmentSetValue timelineVAdj newValue
  adjustmentValueChanged timelineVAdj

-- -----------------------------------------------------------------------------
