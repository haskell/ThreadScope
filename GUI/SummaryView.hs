module GUI.SummaryView (
    RunView,
    runViewNew,
    runViewSetEvents,
  ) where

import GHC.RTS.Events

import GUI.Timeline.Render.Constants

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Control.Monad.Reader
import Data.Array
import Data.IORef
import qualified Data.List as L

-------------------------------------------------------------------------------

data RunView = RunView
     { gtkLayout :: !Layout
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
  gtkLayout  <- getWidget castToLayout "eventsLayoutRun"
  writeIORef stateRef RunEmpty
  let runView = RunView{..}

  -- Drawing
  on gtkLayout exposeEvent $ liftIO $ do
    drawRun runView =<< readIORef stateRef
    return True

  return runView

-------------------------------------------------------------------------------

runViewSetEvents :: RunView -> Maybe (Array Int CapEvent) -> IO ()
runViewSetEvents RunView{gtkLayout, stateRef} mevents = do
  let runState = case mevents of
        Nothing     -> RunEmpty
        Just events -> RunLoaded (showRun events)
      showEnv env = (5, "Program environment:") : zip [6..] (map ("   " ++) env)

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
      showRun = unlines . map snd . L.sort . foldr showEvent start . elems
  writeIORef stateRef runState
  widgetQueueDraw gtkLayout

-------------------------------------------------------------------------------

drawRun :: RunView -> RunState -> IO ()
drawRun _ RunEmpty = return ()
drawRun RunView{gtkLayout} RunLoaded{..} = do
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx runState
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 30)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout
