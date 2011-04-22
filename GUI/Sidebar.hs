module GUI.Sidebar (
    setupSideBar,
  ) where

import GUI.State
import GUI.Timeline

import Graphics.UI.Gtk



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

