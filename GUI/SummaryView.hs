module GUI.SummaryView (
    InfoView,
    summaryViewNew,
    summaryViewSetEvents,
  ) where

import GHC.RTS.Events

import GUI.Timeline.Render.Constants

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.Array
import Data.IORef

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

infoViewSetEvents :: (Array Int CapEvent -> InfoState)
                  -> InfoView -> Maybe (Array Int CapEvent) -> IO ()
infoViewSetEvents f InfoView{gtkLayout, stateRef} mevents = do
  let infoState = case mevents of
        Nothing     -> InfoEmpty
        Just events -> f events
  writeIORef stateRef infoState
  widgetQueueDraw gtkLayout

summaryViewProcessEvents :: Array Int CapEvent -> InfoState
summaryViewProcessEvents _events = InfoLoaded "TODO"

summaryViewSetEvents :: InfoView -> Maybe (Array Int CapEvent) -> IO ()
summaryViewSetEvents = infoViewSetEvents summaryViewProcessEvents

-------------------------------------------------------------------------------

drawInfo :: InfoView -> InfoState -> IO ()
drawInfo _ InfoEmpty = return ()
drawInfo InfoView{gtkLayout} InfoLoaded{..} = do
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx infoState
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 30)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout
