module GUI.SummaryView (
    InfoView,
    runViewNew,
    summaryViewNew,
    runViewSetEvents,
    summaryViewSetEvents,
  ) where

import GHC.RTS.Events

import GUI.Timeline.Render.Constants

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Control.Monad.Reader
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

runViewNew :: Builder -> IO InfoView
runViewNew = infoViewNew "eventsLayoutRun"

summaryViewNew :: Builder -> IO InfoView
summaryViewNew = infoViewNew "eventsLayoutSummary"

-------------------------------------------------------------------------------

infoViewSetEvents :: (Array Int CapEvent -> InfoState)
                  -> InfoView -> Maybe (Array Int CapEvent) -> IO ()
infoViewSetEvents f InfoView{gtkLayout, stateRef} mevents = do
  let infoState = case mevents of
        Nothing     -> InfoEmpty
        Just events -> f events
  writeIORef stateRef infoState
  widgetQueueDraw gtkLayout

runViewProcessEvents :: Array Int CapEvent -> InfoState
runViewProcessEvents events =
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
      start = [(1, "Program start time:  how to get it?")]
      showInfo = unlines . map snd . L.sort . foldr showEvent start . elems
  in InfoLoaded (showInfo events)

runViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> IO ()
runViewSetEvents = infoViewSetEvents runViewProcessEvents

data GCState = GCState
  { gclastEvent :: !(IM.IntMap EventInfo)
  , gclastStart :: !(IM.IntMap Timestamp)
  , gccolls     :: !Int
  , gcpar       :: !Int  -- TODO: we probably don't have enough data for that.
  , gcelapsed   :: !Timestamp
  , gcmaxPause  :: !Timestamp
  }

summaryViewProcessEvents :: Array Int CapEvent -> InfoState
summaryViewProcessEvents events =
  let start = GCState
        { gclastEvent = IM.empty
        , gclastStart = IM.empty
        , gccolls = 0
        , gcpar = 0
        , gcelapsed = 0
        , gcmaxPause = 0
        }
      GCState{..} = L.foldl' step start $ elems $ events
      tIME_RESOLUTION = 1000000
      timeToSecondsDbl :: Integral a => a -> Double
      timeToSecondsDbl t = fromIntegral t / tIME_RESOLUTION
      gcelapsedS = timeToSecondsDbl gcelapsed
      gcmaxPauseS = timeToSecondsDbl gcmaxPause
      avgPauseS
        | gccolls == 0 = 0
        | otherwise = gcelapsedS / fromIntegral gccolls
      infoLines = (1,        "                                     Tot elapsed time   Avg pause  Max pause") :
                  (2, printf "  Gen  all    %5d colls, %5d par      %5.2fs          %3.4fs    %3.4fs" gccolls gcpar gcelapsedS avgPauseS gcmaxPauseS) : []
      info = unlines $ map snd $ L.sort infoLines
  in InfoLoaded info
 where
  step !gcstate@GCState{..} (CapEvent mcap (Event time spec)) =
    let cap = fromJust mcap
        -- We ignore GCWork, GCIdle and GCDone. Too detailed for the summary.
        gcstateNew = case spec of
          RequestSeqGC ->
            assert (case IM.findWithDefault EndGC cap gclastEvent of
                      EndGC -> True
                      _     -> False) $
            gcstate { gclastEvent = IM.insert cap RequestSeqGC gclastEvent
                    }
          RequestParGC ->
            assert (case IM.findWithDefault EndGC cap gclastEvent of
                      EndGC -> True
                      _     -> False) $
            gcstate { gclastEvent = IM.insert cap RequestParGC gclastEvent
                    , -- Probably inaccurate, but that's the best we can do.
                      gcpar = gcpar + 1
                    }
          StartGC ->
-- TODO: apparently does not hold.
--            assert (case IM.findWithDefault EndGC cap gclastEvent of
--                      RequestSeqGC -> True
--                      RequestParGC -> True
--                      _            -> False) $
-- TODO: Probably GC does not have to be requested.
-- Consequently, we move Incrementing gccolls from Request* to EndGC.
-- We can't move gcpar, so let's hope parallel GC requires requests,
-- or else gcpar is too low.
            gcstate { gclastEvent = IM.insert cap StartGC gclastEvent
                    , gclastStart = IM.insert cap time gclastStart
                    }
          EndGC ->
            assert (case IM.findWithDefault EndGC cap gclastEvent of
                      StartGC -> True
                      _       -> False) $
            gcstate { gclastEvent = IM.insert cap EndGC gclastEvent
                    , gccolls = gccolls + 1
                    , gcelapsed = gcelapsed + duration
                    , gcmaxPause = max gcmaxPause duration
                    }
           where
            duration = time - gclastStart IM.! cap
          _ -> gcstate
    in gcstateNew

summaryViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> IO ()
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
