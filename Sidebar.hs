module Sidebar (
    setupSideBar,
    sidebarBookmarks,
    sidebarTraces,
  ) where

import State
import Timeline

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Data.IORef
import Control.Monad
import Control.Monad.Trans

-- XXX: we should be using a Model here, but not sure how to do that
-- with Glade.
sidebarTraces, sidebarBookmarks :: Int
sidebarTraces    = 0
sidebarBookmarks = 1

setupSideBar :: ViewerState -> IO ()
setupSideBar state@ViewerState{..} = do
  on sidebarCloseButton buttonPressEvent $ tryEvent $ liftIO $ do
     checkMenuItemSetActive sidebarToggle False
     containerRemove hpaned sidebarVBox
     
  onToggle sidebarToggle $ do -- no new-style event for menu toggles?
     b <- checkMenuItemGetActive sidebarToggle
     if b 
        then panedAdd1 hpaned sidebarVBox
        else containerRemove hpaned sidebarVBox

  on sidebarCombo changed $ do
     sidebarChangeView state

  writeIORef sidebarComboState 1
  comboBoxSetActive sidebarCombo 0
  sidebarChangeView state

  traceColumn <- treeViewColumnNew
--  treeViewColumnSetTitle traceColumn "Trace"

  textcell <- cellRendererTextNew
  togglecell <- cellRendererToggleNew

  treeViewColumnPackStart traceColumn textcell True
  treeViewColumnPackEnd   traceColumn togglecell False

  cellLayoutSetAttributes traceColumn textcell tracesStore $
          \(t,bool) -> case t of
                         TraceGroup str -> [cellText := str]
                         TraceHEC   n   -> [cellText := show n]
                         TraceThread n  -> [cellText := show n]
                         TraceActivity  -> [cellText := "Activity Profile"]

  cellLayoutSetAttributes traceColumn togglecell tracesStore $
          \(str,bool) -> [cellToggleActive := bool]

  on togglecell cellToggled $ \str ->  do
    let p = stringToTreePath str
    (str,bool) <- treeStoreGetValue tracesStore p
    treeStoreSetValue tracesStore p (str, not bool)
    timelineParamsChanged state

  treeViewAppendColumn tracesTreeView traceColumn

  return ()

sidebarChangeView :: ViewerState -> IO ()
sidebarChangeView state@ViewerState{..} = do
  r <- readIORef sidebarComboState
  v <- comboBoxGetActive sidebarCombo
  when (v /= r) $ do
   writeIORef sidebarComboState v
   case v of
    _ | v == sidebarTraces -> do
        containerRemove sidebarVBox bookmarkVBox
        boxPackEnd sidebarVBox tracesVBox PackGrow 0
        widgetShowAll tracesVBox

      | v == sidebarBookmarks -> do
        containerRemove sidebarVBox tracesVBox
        boxPackEnd sidebarVBox bookmarkVBox PackGrow 0
        widgetShowAll bookmarkVBox

    _ ->
      return ()
