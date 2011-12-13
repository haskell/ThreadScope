module GUI.SummaryView (
    RunView,
    runViewNew,
    runViewSetEvents,
  ) where

import GHC.RTS.Events

import Graphics.UI.Gtk

import Control.Monad.Reader
import Data.Array
import Data.IORef
import qualified Data.List as L

-------------------------------------------------------------------------------

data RunView = RunView
     { drawArea :: !Widget
     , stateRef :: !(IORef RunState)
     }

data RunState
   = RunEmpty
   | RunLoaded
     { runState :: String
     }

-------------------------------------------------------------------------------

runViewNew :: Builder -> IO RunView
runViewNew builder = do

  stateRef <- newIORef undefined
  let getWidget cast = builderGetObject builder cast
  drawArea  <- getWidget castToWidget "eventsDrawingArea2"
  writeIORef stateRef RunEmpty
  let runView = RunView{..}

  -- Drawing
  on drawArea exposeEvent $ liftIO $ do
    drawRun runView =<< readIORef stateRef
    return True

  return runView

-------------------------------------------------------------------------------

runViewSetEvents :: RunView -> Maybe (Array Int CapEvent) -> IO ()
runViewSetEvents RunView{drawArea, stateRef} mevents = do
  let runState = case mevents of
        Nothing     -> RunEmpty
        Just events -> RunLoaded (showRun events)
      showEvent (CapEvent _cap (Event _time spec)) acc =
        case spec of
          RtsIdentifier{} -> (2, showEventInfo spec) : acc
          ProgramArgs{}   -> (3, showEventInfo spec) : acc
          ProgramEnv{}    -> (4, showEventInfo spec) : acc
          _               -> acc
      showRun = unlines . map snd . L.sort . foldr showEvent [(1, "Start time: how to get it?")] . elems
  writeIORef stateRef runState
  widgetQueueDraw drawArea

-------------------------------------------------------------------------------

drawRun :: RunView -> RunState -> IO ()
drawRun _ RunEmpty = return ()
drawRun RunView{drawArea} RunLoaded{..} = do
  win <- widgetGetDrawWindow drawArea
  (width, _) <- widgetGetSize drawArea
  pangoCtx <- widgetGetPangoContext drawArea
  layout <- layoutText pangoCtx runState
  layoutSetWidth layout (Just $ fromIntegral width)
  renderWithDrawable win $ showLayout layout
