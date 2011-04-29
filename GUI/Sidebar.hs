module GUI.Sidebar (
    Sidebar,
    sidebarNew,
    SidebarActions(..),

    sidebarSetVisibility
  ) where

import GUI.Types

import Graphics.UI.Gtk


-- | Abstract sidebar object.
--
data Sidebar = Sidebar {
       sidebarBox :: Widget
     }

-- | The actions to take in response to sidebar events.
--
data SidebarActions = SidebarActions {
       sidebarTraceToggled :: IO ()
     }


sidebarSetVisibility :: Sidebar -> Bool -> IO ()
sidebarSetVisibility sidebar visible =
  set (sidebarBox sidebar) [ widgetVisible := visible ]


sidebarNew :: TreeStore (Trace, Bool) --TODO: eliminate this param
           -> Builder -> SidebarActions -> IO Sidebar
sidebarNew tracesStore builder actions = do

    let getWidget cast = builderGetObject builder cast

    sidebarBox      <- getWidget castToWidget   "sidebar"
    tracesTreeView  <- getWidget castToTreeView "traces_tree"

    traceColumn <- treeViewColumnNew
    textcell    <- cellRendererTextNew
    togglecell  <- cellRendererToggleNew

    treeViewColumnPackStart traceColumn textcell   True
    treeViewColumnPackStart traceColumn togglecell False
    treeViewAppendColumn tracesTreeView traceColumn

    treeViewSetModel tracesTreeView tracesStore

    cellLayoutSetAttributes traceColumn textcell tracesStore $ \(tr, _) ->
      [ cellText := renderTrace tr ]

    cellLayoutSetAttributes traceColumn togglecell tracesStore $ \(_, bool) ->
      [ cellToggleActive := bool ]

    on togglecell cellToggled $ \str ->  do
      let tp = stringToTreePath str
      (str, bool) <- treeStoreGetValue tracesStore tp
      treeStoreSetValue tracesStore tp (str, not bool)
      sidebarTraceToggled actions

    return Sidebar {..}

  where
    renderTrace (TraceGroup str) = str
    renderTrace (TraceHEC    n)  = show n
    renderTrace (TraceThread n)  = show n
    renderTrace (TraceActivity)  = "Activity Profile"
