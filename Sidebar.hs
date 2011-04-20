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
  on sidebarToggle checkMenuItemToggled $ do
     showSidebar <- checkMenuItemGetActive sidebarToggle
     set sidebarBox [ widgetVisible := showSidebar ]

  traceColumn <- treeViewColumnNew

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

