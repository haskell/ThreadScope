module Sidebar (
    setupSideBar,
    sidebarBookmarks,
    sidebarTraces,
  ) where

import State

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Data.Tree
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
  comboBoxSetActive sidebarCombo 1

  tracesStore <- treeStoreNew [ Node { rootLabel = ("HECs",True),
                                       subForest = [
                                          Node { rootLabel = ("1",True),
                                                 subForest = [] },
                                          Node { rootLabel = ("2",True),
                                                 subForest = [] } ]
                                     } ]
  treeViewSetModel tracesTreeView tracesStore

  traceColumn <- treeViewColumnNew
  treeViewColumnSetTitle traceColumn "Trace"
  cell <- cellRendererTextNew
  treeViewColumnPackStart traceColumn cell True
  cellLayoutSetAttributes traceColumn cell tracesStore $
          \(str,bool) -> [cellText := str]
  treeViewAppendColumn tracesTreeView traceColumn

  showColumn <- treeViewColumnNew
  treeViewColumnSetTitle showColumn "Show"
  cell <- cellRendererToggleNew
  treeViewColumnPackStart showColumn cell True
  cellLayoutSetAttributes showColumn cell tracesStore $
          \(str,bool) -> [cellToggleActive := bool]
  treeViewAppendColumn tracesTreeView showColumn

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
