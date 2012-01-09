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

data RTSSparkCounters = RTSSparkCounters
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Timestamp
 }

data RTSGCCounters = RTSGCCounters
  { gclastEvent :: !EventInfo
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
         EndGC 0 (sumPr gcseq) (sumPr gcpar) (sumPr gcelapsed)
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
          { gclastEvent = EndGC
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
             _gclastEnded RTSGCCounters{gclastEvent} = case gclastEvent of
                                                         EndGC -> True
                                                         _     -> False
         in case spec of
          -- TODO: check EventBlock elsewhere, define {map,fold}EventBlock, etc.
          EventBlock {cap = bcap, block_events} ->
            L.foldl' (stateNew bcap) rtsstate block_events
          RequestSeqGC ->
            assert (_gclastEnded defGC) $
            rtsstate { rtsGC = IM.insert cap
                                (defGC { gclastEvent = RequestSeqGC }) rtsGC
                     }
          RequestParGC ->
            -- JaffaCake says that RequestParGC is enough to distinguish
            -- between seq and par GC; only one cap issues a RequestParGC,
            -- the others will all StartGC some time later.
-- TODO: not true, probably the request can be issued before old GCs finish:
--          assert (L.all _gclastEnded $ IM.elems rtsGC) $
-- But this means we have to queue the RequestParGC requests (or at least one)
-- and get back to them after the previous GC is finished.
-- Or perhaps caps busy with old GCs don't take part in par GCs? The current
-- code behaves as if this was the case.
-- TODO: investigate in the parallel-gc.pdf paper and/or GC or +RTS -s code
            rtsstate { rtsGC = IM.map (\ dGC@RTSGCCounters{gclastEvent} ->
                                        case gclastEvent of
                                          EndGC ->
                                            dGC { gclastEvent = RequestParGC }
                                          _ -> dGC)
                                 rtsGC
                     }
          StartGC ->
-- TODO: not true for intervals; check in ghc-events verify:
--           assert (case gclastEvent of
--                     RequestSeqGC -> True
--                     RequestParGC -> True
--                     _            -> False) $
            let timeGC = defGC { gclastEvent = StartGC
                               , gclastStart = time }
                collsGC = case gclastEvent of
                  RequestSeqGC -> timeGC { gcseq = gcseq + 1 }
                  RequestParGC -> timeGC { gcpar = gcpar + 1 }
                  EndGC        -> timeGC  -- start of an interval
                  _            -> error "Wrong event before StartGC"
            in rtsstate { rtsGC = IM.insert cap collsGC rtsGC
                        }
          EndGC ->
-- TODO: not true for intervals; check in ghc-events verify:
--          assert (case gclastEvent of
--                    StartGC -> True
--                    _       -> False) $
            rtsstate { rtsGC = IM.insert cap
                                 (defGC { gclastEvent = EndGC
                                        , gcelapsed = gcelapsed + duration
                                        , gcmaxPause =
                                            max gcmaxPause duration }) rtsGC
                     }
           where
            duration = time - gclastStart
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
