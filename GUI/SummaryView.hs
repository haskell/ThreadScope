module GUI.SummaryView (
    InfoView,
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

-------------------------------------------------------------------------------

data InfoView = InfoView
  { gtkLayout      :: !Layout
  , defaultInfoRef :: !(IORef String)
  , meventsRef     :: !(IORef (Maybe (Array Int CapEvent)))
  , mintervalIORef :: !(IORef (Maybe Interval))
  }

-------------------------------------------------------------------------------

infoViewNew :: String -> Builder -> IO InfoView
infoViewNew widgetName builder = do

  defaultInfoRef <- newIORef ""
  meventsRef <- newIORef Nothing
  mintervalIORef <- newIORef Nothing
  let getWidget cast = builderGetObject builder cast
  gtkLayout  <- getWidget castToLayout widgetName
  let infoView = InfoView{..}

  -- Drawing
  on gtkLayout exposeEvent $ liftIO $ do
    defaultInfo <- readIORef defaultInfoRef
    mevents <- readIORef meventsRef
    minterval <- readIORef mintervalIORef
    drawInfo gtkLayout defaultInfo mevents minterval
    return True

  return infoView

summaryViewNew :: Builder -> IO InfoView
summaryViewNew = infoViewNew "eventsLayoutSummary"

-------------------------------------------------------------------------------

infoViewSetEvents :: (Maybe (Array Int CapEvent)
                      -> (String, Maybe (Array Int CapEvent)))
                  -> InfoView -> Maybe (Array Int CapEvent) -> IO ()
infoViewSetEvents f InfoView{..} mev = do
  let (defaultInfo, mevents) = f mev
  writeIORef defaultInfoRef defaultInfo
  writeIORef meventsRef mevents
  writeIORef mintervalIORef Nothing  -- the old interval may make no sense
  widgetQueueDraw gtkLayout

runViewProcessEvents :: Maybe (Array Int CapEvent)
                     -> (String, Maybe (Array Int CapEvent))
runViewProcessEvents Nothing = ("", Nothing)
runViewProcessEvents (Just events) =
  let showEnv env = (5, "Program environment:") : zip [6..] (map ("   " ++) env)
      showEvent (CapEvent _cap (Event _time spec)) acc =
        case spec of
          RtsIdentifier _ i  ->
            (2, "Haskell RTS name:  " ++ "\"" ++ i ++ "\"") : acc
          ProgramArgs _ args ->
            (3, "Program name:  " ++ "\"" ++ head args ++ "\"") :
            (4, "Program arguments:  " ++ show (tail args)) :
            acc
          ProgramEnv _ env   -> acc ++ showEnv env
          _                  -> acc
      start = [(1, "Program start time: TODO: get it from a new event")]
      showInfo = unlines . map snd . L.sort . foldr showEvent start . elems
  in (showInfo events, Nothing)

runViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> IO ()
runViewSetEvents = infoViewSetEvents runViewProcessEvents

-- TODO: change RTS to Rts, gcpar to gcParetc.
data RTSSparkCounters = RTSSparkCounters
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Timestamp
 }

data GcMode = ModePar | ModeSeq | ModeInitial | ModeEnded

data RTSGCCounters = RTSGCCounters
  { gcMode      :: !GcMode
  , gclastEvent :: !EventInfo
  , gclastStart :: !Timestamp
  , gcseq       :: !Int
  , gcpar       :: !Int  -- TODO: We probably don't have enough data for that.
  , gcelapsed   :: !Timestamp
  , gcmaxPause  :: !Timestamp
  }

data RTSState = RTSState
  { rtsGC     :: !(IM.IntMap RTSGCCounters)
  , rtsSparks :: !(IM.IntMap (RTSSparkCounters, RTSSparkCounters))
  }

summaryViewProcessEvents :: Maybe Interval -> Maybe (Array Int CapEvent)
                         -> (String, Maybe (Array Int CapEvent))
summaryViewProcessEvents _ Nothing = ("", Nothing)
summaryViewProcessEvents minterval (Just events) =
  let start = RTSState
        { rtsGC    = IM.empty
        , rtsSparks = IM.empty
        }
      RTSState{rtsGC, rtsSparks = rtsSparksRaw} =
        L.foldl' step start $ elems $ events
      diffSparks (RTSSparkCounters crt1 dud1 ovf1 cnv1 fiz1 gcd1,
                  RTSSparkCounters crt2 dud2 ovf2 cnv2 fiz2 gcd2) =
        RTSSparkCounters (crt2 - crt1) (dud2 - dud1) (ovf2 - ovf1)
                         (cnv2 - cnv1) (fiz2 - fiz1) (gcd2 - gcd1)
      rtsSparks = IM.map diffSparks rtsSparksRaw
      totalElapsedS = timeToSecondsDbl $ iend - istart
      gcLine :: Int -> RTSGCCounters -> String
      gcLine k = displayGCCounter (printf "GC HEC %d" k)
      gcSum = sumGCCounters $ IM.elems rtsGC
      gcLines =
        [ (-300,        "                                            Tot elapsed time   Avg pause  Max pause")] ++
        (map (\ (k, gc) -> (-200 + k, gc)) $
           IM.assocs (IM.mapWithKey gcLine rtsGC)) ++
        [(-100, displayGCCounter "GC TOTAL" gcSum)] ++
        [(-1, "")]
      sparkLine :: Int -> RTSSparkCounters -> String
      sparkLine k = displaySparkCounter (printf "SPARKS HEC %d" k)
      sparkSum = sumSparkCounters $ IM.elems rtsSparks
      sparkLines =
        IM.assocs (IM.mapWithKey sparkLine rtsSparks) ++
        [(100, displaySparkCounter "SPARKS TOTAL" sparkSum)] ++
        [(200, "")]
      timeLines =
        [ (201, printf "  GC      time  %6.2fs elapsed"
                  (timeToSecondsDbl (gcelapsed gcSum)))
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
    in RTSGCCounters
         ModePar EndGC 0 (sumPr gcseq) (sumPr gcpar) (sumPr gcelapsed)
         (L.maximum $ 0 : map gcmaxPause l)
  displayGCCounter :: String -> RTSGCCounters -> String
  displayGCCounter header RTSGCCounters{..} =
    let gccolls = gcseq + gcpar
        gcelapsedS = timeToSecondsDbl gcelapsed
        gcmaxPauseS = timeToSecondsDbl gcmaxPause
        gcavgPauseS
          | gccolls == 0 = 0
          | otherwise = gcelapsedS / fromIntegral gccolls
    in printf "  %s  Gen 0+1  %5d colls, %5d par      %5.2fs          %3.4fs    %3.4fs" header gccolls gcpar gcelapsedS gcavgPauseS gcmaxPauseS
  sumSparkCounters l =
    let sumPr proj = L.sum $ L.map proj l
    in RTSSparkCounters
         (sumPr sparkCreated) (sumPr sparkDud) (sumPr sparkOverflowed)
         (sumPr sparkConverted) (sumPr sparkFizzled) (sumPr sparkGCd)
  displaySparkCounter :: String -> RTSSparkCounters -> String
  displaySparkCounter header RTSSparkCounters{..} =
    printf "  %s: %7d (%7d converted, %7d overflowed, %7d dud, %7d GC'd, %7d fizzled)" header (sparkCreated + sparkDud + sparkOverflowed) sparkConverted sparkOverflowed sparkDud sparkGCd sparkFizzled
  step !state (CapEvent _ ev) | time ev < istart || time ev > iend = state
  step !state (CapEvent mcap ev) =
    let defaultGC time = RTSGCCounters
          { gcMode = ModeInitial
          , gclastEvent = EndGC
          , gclastStart = time
          , gcseq = 0
          , gcpar = 0
          , gcelapsed = 0
          , gcmaxPause = 0
          }
        -- We ignore GCWork, GCIdle and GCDone. Too detailed for the summary.
        stateNew cap !rtsstate@RTSState{rtsGC, rtsSparks} (Event time spec) =
         let defGC@RTSGCCounters{..} =
               IM.findWithDefault (defaultGC time) cap rtsGC
             _lastGcEnded RTSGCCounters{gclastEvent} = case gclastEvent of
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
            let newGC = defGC { gclastEvent = StartGC
                              , gclastStart = time }
            in rtsstate { rtsGC = IM.insert cap newGC rtsGC
                        }
          EndGC ->
-- TODO: not true for intervals; check in ghc-events verify:
--          assert (case gclastEvent of
--                    StartGC -> True
--                    _       -> False) $
            let duration = time - gclastStart
                endedGC = defGC { gcMode = ModeEnded
                                , gclastEvent = EndGC
                                }
                timeGC = endedGC { gcelapsed = gcelapsed + duration
                                 , gcmaxPause = max gcmaxPause duration
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
                  ModeSeq -> timeGC { gcseq = gcseq + 1 }
                  ModePar -> timeGC { gcpar = gcpar + 1 }
                  -- We don't know if this GC requested before the selected
                  -- interval is par or seq --- skip it.
                  ModeInitial -> endedGC
-- TODO: investigate, fix and reenable the error
--                  ModeEnded   -> error "EndGC found before Request*GC"
                  ModeEnded   -> endedGC
            in case gclastEvent of
              StartGC -> rtsstate { rtsGC = IM.insert cap collsGC rtsGC
                                  }
              -- We don't know the exact timing of this GC started before
              -- the selected interval --- skip it. We don't know if StartGC
              -- or RequestParGC/RequestSeqGC gets issued first,
              -- so we have to check both gcMode above and gclastEvent here.
              _       -> rtsstate { rtsGC = IM.insert cap endedGC rtsGC
                                  }
          SparkCounters crt dud ovf cnv fiz gcd _rem -> -- TODO
            let current = RTSSparkCounters crt dud ovf cnv fiz gcd
                alter Nothing = Just (current, current)
                alter (Just (first, _previous)) = Just (first, current)
            in rtsstate { rtsSparks = IM.alter alter cap rtsSparks }
          _ -> rtsstate
    in stateNew (fromJust mcap) state ev

summaryViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> IO ()
summaryViewSetEvents = infoViewSetEvents (summaryViewProcessEvents Nothing)

-------------------------------------------------------------------------------

drawInfo :: Layout -> String -> Maybe (Array Int CapEvent)
         -> Maybe Interval -> IO ()
drawInfo gtkLayout defaultInfo mevents minterval = do
  let info = case minterval of
        Nothing -> defaultInfo
        _       -> fst (summaryViewProcessEvents minterval mevents)  -- HACK
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx info
  layoutSetAttributes layout [AttrFamily minBound maxBound "monospace"]
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 10)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout

-------------------------------------------------------------------------------

summaryViewSetInterval :: InfoView -> Maybe Interval -> IO ()
summaryViewSetInterval InfoView{gtkLayout, mintervalIORef} minterval = do
  writeIORef mintervalIORef minterval
  widgetQueueDraw gtkLayout
