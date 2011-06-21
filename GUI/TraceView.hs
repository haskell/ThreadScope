module GUI.TraceView (
    TraceView,
    traceViewNew,
    TraceViewActions(..),
    traceViewSetHECs,
    traceViewGetTraces,
  ) where

import Events.HECs
import GUI.Types

import Graphics.UI.Gtk
import Data.Tree


-- | Abstract trace view object.
--
data TraceView = TraceView {
       tracesStore :: TreeStore (Trace, Bool)
     }

-- | The actions to take in response to TraceView events.
--
data TraceViewActions = TraceViewActions {
       traceViewTracesChanged :: [Trace] -> IO ()
     }

traceViewNew :: Builder -> TraceViewActions -> IO TraceView
traceViewNew builder actions = do

    tracesTreeView <- builderGetObject builder  castToTreeView "traces_tree"

    tracesStore <- treeStoreNew []
    traceColumn <- treeViewColumnNew
    textcell    <- cellRendererTextNew
    togglecell  <- cellRendererToggleNew

    let traceview = TraceView {..}

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
      traceViewTracesChanged actions =<< traceViewGetTraces traceview

    return traceview

  where
    renderTrace (TraceHEC    n)  = show n
    renderTrace (SparksHEC   n)  = show n
    renderTrace (TraceThread n)  = show n
    renderTrace (TraceGroup str) = str
    renderTrace (TraceActivity)  = "Activity Profile"

-- Find the HEC traces in the treeStore and replace them
traceViewSetHECs :: TraceView -> HECs -> IO ()
traceViewSetHECs TraceView{tracesStore} hecs = do
    treeStoreClear tracesStore
    go 0
    treeStoreInsert tracesStore [] 0 (TraceActivity, True)
  where
    newt = Node { rootLabel = (TraceGroup "HEC Traces", True),
                  subForest = [ Node { rootLabel = (TraceHEC k, True),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    news = Node { rootLabel = (TraceGroup "HEC Sparks", True),
                  subForest = [ Node { rootLabel = (SparksHEC k, True),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    go n = do
      m <- treeStoreLookup tracesStore [n]
      case m of
        Nothing -> do
          treeStoreInsertTree tracesStore [] 0 news
          treeStoreInsertTree tracesStore [] 0 newt
        Just t  ->
          case t of
             Node { rootLabel = (TraceGroup "HEC Traces", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n newt
               go (n+1)
             Node { rootLabel = (TraceGroup "HEC Sparks", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n news
               go (n+1)
             Node { rootLabel = (TraceActivity, _) } -> do
               treeStoreRemove tracesStore [n]
               go (n+1)
             _ ->
               go (n+1)

traceViewGetTraces :: TraceView -> IO [Trace]
traceViewGetTraces TraceView{tracesStore} = do
  f <- getTracesStoreContents tracesStore
  return [ t | (t, True) <- concatMap flatten f, notGroup t ]
 where
  notGroup (TraceGroup _) = False
  notGroup _              = True

getTracesStoreContents :: TreeStore (Trace,Bool) -> IO (Forest (Trace,Bool))
getTracesStoreContents tracesStore = go 0
  where
  go !n = do
    m <- treeStoreLookup tracesStore [n]
    case m of
      Nothing -> return []
      Just t  -> do
        ts <- go (n+1)
        return (t:ts)
