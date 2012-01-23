module GUI.SummaryView (
    SummaryView,
    summaryViewNew,
    summaryViewSetEvents,
    summaryViewSetInterval,
  ) where

import GHC.RTS.Events

import GUI.Timeline.Render.Constants
import GUI.Types

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.Array
import Data.IORef
import Data.Maybe
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Exception (assert)
import Text.Printf

------------------------------------------------------------------------------

data SummaryView = SummaryView
  { gtkLayout      :: !Layout
  , defaultInfoRef :: !(IORef String)
  , meventsRef     :: !(IORef (Maybe (Array Int CapEvent)))
  , mintervalIORef :: !(IORef (Maybe Interval))
  }

------------------------------------------------------------------------------

summaryViewNew :: Builder -> IO SummaryView
summaryViewNew builder = do
  defaultInfoRef <- newIORef ""
  meventsRef <- newIORef Nothing
  mintervalIORef <- newIORef Nothing
  let getWidget cast = builderGetObject builder cast
  gtkLayout  <- getWidget castToLayout "eventsLayoutSummary"
  let infoView = SummaryView{..}
  -- Drawing
  on gtkLayout exposeEvent $ liftIO $ do
    defaultInfo <- readIORef defaultInfoRef
    mevents <- readIORef meventsRef
    minterval <- readIORef mintervalIORef
    drawSummary gtkLayout defaultInfo mevents minterval
    return True
  return infoView

------------------------------------------------------------------------------

drawSummary :: Layout -> String -> Maybe (Array Int CapEvent)
            -> Maybe Interval -> IO ()
drawSummary gtkLayout defaultInfo mevents minterval = do
  let info = case minterval of
        Nothing -> defaultInfo  -- speedup
        _       -> fst (summaryViewProcessEvents minterval mevents)
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx info
  layoutSetAttributes layout [AttrFamily minBound maxBound "monospace"]
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 10)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout

------------------------------------------------------------------------------

summaryViewSetInterval :: SummaryView -> Maybe Interval -> IO ()
summaryViewSetInterval SummaryView{gtkLayout, mintervalIORef} minterval = do
  writeIORef mintervalIORef minterval
  widgetQueueDraw gtkLayout

------------------------------------------------------------------------------

genericSetEvents :: (Maybe (Array Int CapEvent)
                     -> (String, Maybe (Array Int CapEvent)))
                 -> SummaryView -> Maybe (Array Int CapEvent) -> IO ()
genericSetEvents processEvents SummaryView{..} mev = do
  let (defaultInfo, mevents) = processEvents mev
  writeIORef defaultInfoRef defaultInfo
  writeIORef meventsRef mevents
  writeIORef mintervalIORef Nothing  -- the old interval may make no sense
  widgetQueueDraw gtkLayout

data RtsSparkCounters = RtsSparkCounters
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Timestamp
 }

data GcMode = ModePar | ModeSeq | ModeInitial | ModeEnded

data RtsGCCounters = RtsGCCounters
  { gcMode      :: !GcMode
  , gcLastEvent :: !EventInfo
  , gcLastStart :: !Timestamp
  , gcSeq       :: !Int
  , gcPar       :: !Int
  , gcElapsed   :: !Timestamp
  , gcMaxPause  :: !Timestamp
  }

data RtsState = RtsState
  { rtsGC     :: !(IM.IntMap RtsGCCounters)
  , rtsSparks :: !(IM.IntMap (RtsSparkCounters, RtsSparkCounters))
  }

-- TODO: Split up some more.
summaryViewProcessEvents :: Maybe Interval -> Maybe (Array Int CapEvent)
                         -> (String, Maybe (Array Int CapEvent))
summaryViewProcessEvents _ Nothing = ("", Nothing)
summaryViewProcessEvents minterval (Just events) =
  let start = RtsState
        { rtsGC    = IM.empty
        , rtsSparks = IM.empty
        }
      RtsState{rtsGC, rtsSparks = rtsSparksRaw} =
        L.foldl' step start $ elems $ events
      diffSparks (RtsSparkCounters crt1 dud1 ovf1 cnv1 fiz1 gcd1,
                  RtsSparkCounters crt2 dud2 ovf2 cnv2 fiz2 gcd2) =
        RtsSparkCounters (crt2 - crt1) (dud2 - dud1) (ovf2 - ovf1)
                         (cnv2 - cnv1) (fiz2 - fiz1) (gcd2 - gcd1)
      rtsSparks = IM.map diffSparks rtsSparksRaw
      totalElapsedS = timeToSecondsDbl $ iend - istart
      gcLine :: Int -> RtsGCCounters -> String
      gcLine k = displayGCCounter (printf "GC HEC %d" k)
      gcSum = sumGCCounters $ IM.elems rtsGC
      gcLines =
        [ (-300,        "                                            Tot elapsed time   Avg pause  Max pause")] ++
        (map (\ (k, gc) -> (-200 + k, gc)) $
           IM.assocs (IM.mapWithKey gcLine rtsGC)) ++
        [(-100, displayGCCounter "GC TOTAL" gcSum)] ++
        [(-1, "")]
      sparkLine :: Int -> RtsSparkCounters -> String
      sparkLine k = displaySparkCounter (printf "SPARKS HEC %d" k)
      sparkSum = sumSparkCounters $ IM.elems rtsSparks
      sparkLines =
        IM.assocs (IM.mapWithKey sparkLine rtsSparks) ++
        [(100, displaySparkCounter "SPARKS TOTAL" sparkSum)] ++
        [(200, "")]
      timeLines =
        [ (201, printf "  GC      time  %6.2fs elapsed"
                  (timeToSecondsDbl (gcElapsed gcSum)))
        , (202, printf "  Total   time  %6.2fs elapsed" totalElapsedS)
        ]
      infoLines = gcLines ++ sparkLines ++ timeLines
      info = unlines $ map snd $ L.sort infoLines
  in (info, Just events)
 where
  eventBlockEnd e | EventBlock{ end_time=t } <- spec $ ce_event e = t
  eventBlockEnd e = time $ ce_event e
  -- Warning: stack overflow when done like in ReadEvents.hs:
  lastTx =
    L.foldl' (\ acc e -> max acc (eventBlockEnd e)) 1 (elems $ events)
  (istart, iend) = fromMaybe (0, lastTx) minterval
  tIME_RESOLUTION = 1000000
  timeToSecondsDbl :: Integral a => a -> Double
  timeToSecondsDbl t = fromIntegral t / tIME_RESOLUTION
  sumGCCounters l =
    let sumPr proj = L.sum $ L.map proj l
    in RtsGCCounters
         ModePar EndGC 0 (sumPr gcSeq) (sumPr gcPar) (sumPr gcElapsed)
         (L.maximum $ 0 : map gcMaxPause l)
  displayGCCounter :: String -> RtsGCCounters -> String
  displayGCCounter header RtsGCCounters{..} =
    let gcColls = gcSeq + gcPar
        gcElapsedS = timeToSecondsDbl gcElapsed
        gcMaxPauseS = timeToSecondsDbl gcMaxPause
        gcAvgPauseS
          | gcColls == 0 = 0
          | otherwise = gcElapsedS / fromIntegral gcColls
    in printf "  %s  Gen 0+1  %5d colls, %5d par      %5.2fs          %3.4fs    %3.4fs" header gcColls gcPar gcElapsedS gcAvgPauseS gcMaxPauseS
  sumSparkCounters l =
    let sumPr proj = L.sum $ L.map proj l
    in RtsSparkCounters
         (sumPr sparkCreated) (sumPr sparkDud) (sumPr sparkOverflowed)
         (sumPr sparkConverted) (sumPr sparkFizzled) (sumPr sparkGCd)
  displaySparkCounter :: String -> RtsSparkCounters -> String
  displaySparkCounter header RtsSparkCounters{..} =
    printf "  %s: %7d (%7d converted, %7d overflowed, %7d dud, %7d GC'd, %7d fizzled)" header (sparkCreated + sparkDud + sparkOverflowed) sparkConverted sparkOverflowed sparkDud sparkGCd sparkFizzled
  step !state (CapEvent _ ev) | time ev < istart || time ev > iend = state
  step !state (CapEvent mcap ev) =
    let defaultGC time = RtsGCCounters
          { gcMode = ModeInitial
          , gcLastEvent = EndGC
          , gcLastStart = time
          , gcSeq = 0
          , gcPar = 0
          , gcElapsed = 0
          , gcMaxPause = 0
          }
        -- We ignore GCWork, GCIdle and GCDone. Too detailed for the summary.
        stateNew cap !rtsstate@RtsState{rtsGC, rtsSparks} (Event time spec) =
         let defGC@RtsGCCounters{..} =
               IM.findWithDefault (defaultGC time) cap rtsGC
             _lastGcEnded RtsGCCounters{gcLastEvent} = case gcLastEvent of
                                                         EndGC -> True
                                                         _     -> False
         in case spec of
          -- TODO: check EventBlock elsewhere, define {map,fold}EventBlock, etc.
          EventBlock {cap = bcap, block_events} ->
            L.foldl' (stateNew bcap) rtsstate block_events
          RequestSeqGC ->
            -- JaffaCake says that all the other caps must stop, hence:
            assert (L.all _lastGcEnded $ IM.elems rtsGC) $
            rtsstate { rtsGC = IM.insert cap (defGC { gcMode = ModeSeq }) rtsGC
                     }
          RequestParGC ->
            -- JaffaCake says that RequestParGC is enough to distinguish
            -- between seq and par GC; only one cap issues a RequestParGC,
            -- the others will all StartGC at some point.
            -- Unfortunately, RequestParGC may register after other caps
            -- have already sent their StartGC events, so the following
            -- does not hold:
--            assert (L.all _lastGcEnded $ IM.elems rtsGC) $
            -- TODO: The following tip from JaffaCake can help: Actually you could try moving the interruptAllCapabilities() call in Schedule.c:1500 down below the traceEvent calls.
            rtsstate { rtsGC = IM.map (\ dGC -> dGC { gcMode = ModePar }) rtsGC
                     }
          StartGC ->
            assert (_lastGcEnded defGC) $
            let newGC = defGC { gcLastEvent = StartGC
                              , gcLastStart = time }
            in rtsstate { rtsGC = IM.insert cap newGC rtsGC
                        }
          EndGC ->
-- TODO: not true for intervals; check in ghc-events verify:
--          assert (case gcLastEvent of
--                    StartGC -> True
--                    _       -> False) $
            let duration = time - gcLastStart
                endedGC = defGC { gcMode = ModeEnded
                                , gcLastEvent = EndGC
                                }
                timeGC = endedGC { gcElapsed = gcElapsed + duration
                                 , gcMaxPause = max gcMaxPause duration
                                 }
                -- Par/seq GC counts are incremented here, not under StartGC,
                -- to work around RequestParGC being issued _after_ StartGC.
                -- It would be best if RequestParGC was always issued
                -- before EndGC, so that we know if the GC is par or seq
                -- by the time we have to add the GC to statistics.
                -- Alas this is not the case and the error raised
                -- at ModeEnded below that guards this property
                -- has to be commented out, until the issue is sorted out.
                -- TODO: validate eventlogs, checking that each RequestParGC
                -- is matched by a later EndGC (and by a later
                -- or an earlier StartGC).
                collsGC = case gcMode of
                  ModeSeq -> timeGC { gcSeq = gcSeq + 1 }
                  ModePar -> timeGC { gcPar = gcPar + 1 }
                  -- We don't know if this GC requested before the selected
                  -- interval is par or seq --- skip it.
                  ModeInitial -> endedGC
-- TODO: investigate, fix and reenable the error
--                  ModeEnded   -> error "EndGC found before Request*GC"
                  ModeEnded   -> endedGC
            in case gcLastEvent of
              StartGC -> rtsstate { rtsGC = IM.insert cap collsGC rtsGC
                                  }
              -- We don't know the exact timing of this GC started before
              -- the selected interval --- skip it. We don't know if StartGC
              -- or RequestParGC/RequestSeqGC gets issued first,
              -- so we have to check both gcMode above and gcLastEvent here.
              _       -> rtsstate { rtsGC = IM.insert cap endedGC rtsGC
                                  }
          SparkCounters crt dud ovf cnv fiz gcd _rem -> -- TODO
            let current = RtsSparkCounters crt dud ovf cnv fiz gcd
                alter Nothing = Just (current, current)
                alter (Just (first, _previous)) = Just (first, current)
            in rtsstate { rtsSparks = IM.alter alter cap rtsSparks }
          _ -> rtsstate
    in stateNew (fromJust mcap) state ev

summaryViewSetEvents :: SummaryView -> Maybe (Array Int CapEvent) -> IO ()
summaryViewSetEvents = genericSetEvents (summaryViewProcessEvents Nothing)
