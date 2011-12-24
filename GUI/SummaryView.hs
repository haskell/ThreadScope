module GUI.SummaryView (
    InfoView,
    summaryViewNew,
    summaryViewSetEvents,
  ) where

import GHC.RTS.Events

import Events.HECs
import GUI.Timeline.Render.Constants

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
     { gtkLayout :: !Layout
     , stateRef :: !(IORef InfoState)
     }

data InfoState
   = InfoEmpty
   | InfoLoaded
     { infoState :: String
     }

-------------------------------------------------------------------------------

infoViewNew :: String -> Builder -> IO InfoView
infoViewNew widgetName builder = do

  stateRef <- newIORef undefined
  let getWidget cast = builderGetObject builder cast
  gtkLayout  <- getWidget castToLayout widgetName
  writeIORef stateRef InfoEmpty
  let infoView = InfoView{..}

  -- Drawing
  on gtkLayout exposeEvent $ liftIO $ do
    drawInfo infoView =<< readIORef stateRef
    return True

  return infoView

summaryViewNew :: Builder -> IO InfoView
summaryViewNew = infoViewNew "eventsLayoutSummary"

-------------------------------------------------------------------------------

infoViewSetEvents :: (Array Int CapEvent -> HECs -> InfoState)
                  -> InfoView -> Maybe (Array Int CapEvent) -> HECs -> IO ()
infoViewSetEvents f InfoView{gtkLayout, stateRef} mevents hecs = do
  let infoState = case mevents of
        Nothing     -> InfoEmpty
        Just events -> f events hecs
  writeIORef stateRef infoState
  widgetQueueDraw gtkLayout

runViewProcessEvents :: Array Int CapEvent -> HECs -> InfoState
runViewProcessEvents events _hecs =
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
  in InfoLoaded (showInfo events)

runViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> HECs -> IO ()
runViewSetEvents = infoViewSetEvents runViewProcessEvents

data RTSSparkCounters = RTSSparkCounters
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Timestamp
 }

data RTSGCCounters = RTSGCCounters
  { gclastEvent :: !EventInfo
  , gclastStart :: !Timestamp
  , gccolls     :: !Int
  , gcpar       :: !Int  -- TODO: We probably don't have enough data for that.
  , gcelapsed   :: !Timestamp
  , gcmaxPause  :: !Timestamp
  }

data RTSState = RTSState
  { rtsGC     :: !(IM.IntMap RTSGCCounters)
  , rtsSparks :: !(IM.IntMap RTSSparkCounters)
  }

summaryViewProcessEvents :: Array Int CapEvent -> HECs -> InfoState
summaryViewProcessEvents events hecs =
  let start = RTSState
        { rtsGC    = IM.empty
        , rtsSparks = IM.empty
        }
      RTSState{rtsGC, rtsSparks} = L.foldl' step start $ elems $ events
      lastTxS = timeToSecondsDbl $ hecLastEventTime hecs
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
        , (202, printf "  Total   time  %6.2fs elapsed" lastTxS)
        ]
      infoLines = gcLines ++ sparkLines ++ timeLines
      info = unlines $ map snd $ L.sort infoLines
  in InfoLoaded info
 where
  tIME_RESOLUTION = 1000000
  timeToSecondsDbl :: Integral a => a -> Double
  timeToSecondsDbl t = fromIntegral t / tIME_RESOLUTION
  sumGCCounters l =
    let sumPr proj = L.sum $ L.map proj l
    in RTSGCCounters
         EndGC 0 (sumPr gccolls) (sumPr gcpar) (sumPr gcelapsed)
         (L.maximum $ 0 : map gcmaxPause l)
  displayGCCounter :: String -> RTSGCCounters -> String
  displayGCCounter header RTSGCCounters{..} =
    let gcelapsedS = timeToSecondsDbl gcelapsed
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
  step !state (CapEvent mcap ev) =
    let defaultGC = RTSGCCounters
          { gclastEvent = EndGC
          , gclastStart = 0
          , gccolls = 0
          , gcpar = 0
          , gcelapsed = 0
          , gcmaxPause = 0
          }
        -- We ignore GCWork, GCIdle and GCDone. Too detailed for the summary.
        gcstateNew cap !gcstate@RTSState{rtsGC, rtsSparks} (Event time spec) =
         let defGC@RTSGCCounters{..} = IM.findWithDefault defaultGC cap rtsGC
         in case spec of
          -- TODO: check EventBlock elsewhere, define {map,fold}EventBlock, etc.
          EventBlock {cap = bcap, block_events} ->
            L.foldl' (gcstateNew bcap) gcstate block_events
          RequestSeqGC ->
            assert (case gclastEvent of
                      EndGC -> True
                      _     -> False) $
            gcstate { rtsGC = IM.insert cap
                                (defGC { gclastEvent = RequestSeqGC }) rtsGC
                    }
          RequestParGC ->
            assert (case gclastEvent of
                      EndGC -> True
                      _     -> False) $
            gcstate { rtsGC = IM.insert cap
                                (defGC { gclastEvent = RequestParGC
                      -- Probably inaccurate, but that's the best we can do.
                                       , gcpar = gcpar + 1 }) rtsGC
                    }
          StartGC ->
-- TODO: apparently does not hold.
--            assert (case gclastEvent of
--                      RequestSeqGC -> True
--                      RequestParGC -> True
--                      _            -> False) $
-- TODO: Probably GC does not have to be requested.
-- Consequently, we move Incrementing gccolls from Request* to EndGC.
-- We can't move gcpar, so let's hope parallel GC requires requests,
-- or else gcpar is too low.
            gcstate { rtsGC = IM.insert cap
                                (defGC { gclastEvent = StartGC
                                       , gclastStart = time }) rtsGC
                    }
          EndGC ->
            assert (case gclastEvent of
                      StartGC -> True
                      _       -> False) $
            gcstate { rtsGC = IM.insert cap
                                (defGC { gclastEvent = EndGC
                                       , gccolls = gccolls + 1
                                       , gcelapsed = gcelapsed + duration
                                       , gcmaxPause =
                                           max gcmaxPause duration }) rtsGC
                    }
           where
            duration = time - gclastStart
          SparkCounters crt dud ovf cnv fiz gcd _rem ->
            let cnt = RTSSparkCounters crt dud ovf cnv fiz gcd
            in gcstate { rtsSparks = IM.insert cap cnt rtsSparks }
          _ -> gcstate
    in gcstateNew (fromJust mcap) state ev

summaryViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> HECs -> IO ()
summaryViewSetEvents = infoViewSetEvents summaryViewProcessEvents

-------------------------------------------------------------------------------

drawInfo :: InfoView -> InfoState -> IO ()
drawInfo _ InfoEmpty = return ()
drawInfo InfoView{gtkLayout} InfoLoaded{..} = do
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx infoState
  layoutSetAttributes layout [AttrFamily minBound maxBound "monospace"]
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 30)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout
