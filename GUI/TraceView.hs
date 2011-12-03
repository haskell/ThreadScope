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
       tracesStore :: TreeStore (Trace, Visibility)
     }

data Visibility = Visible | Hidden | MixedVisibility
  deriving Eq

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

    cellLayoutSetAttributes traceColumn togglecell tracesStore $ \(_, vis) ->
      [ cellToggleActive       := vis == Visible
      , cellToggleInconsistent := vis == MixedVisibility ]

    on togglecell cellToggled $ \str ->  do
      let path = stringToTreePath str
      Node (trace, visibility) subtrees <- treeStoreGetTree tracesStore path
      let visibility' = invertVisibility visibility
      treeStoreSetValue tracesStore path (trace, visibility')
      updateChildren tracesStore path subtrees visibility'
      updateParents tracesStore (init path)

      traceViewTracesChanged actions =<< traceViewGetTraces traceview

    return traceview

  where
    renderTrace (TraceHEC           hec) = "HEC " ++ show hec
    renderTrace (TraceInstantHEC    hec) = "HEC " ++ show hec
    renderTrace (TraceCreationHEC   hec) = "HEC " ++ show hec
    renderTrace (TraceConversionHEC hec) = "HEC " ++ show hec
    renderTrace (TracePoolHEC       hec) = "HEC " ++ show hec
    renderTrace (TraceHistogram)         = "Spark Histogram"
    renderTrace (TraceGroup       label) = label
    renderTrace (TraceActivity)          = "Activity Profile"

    updateChildren tracesStore path subtrees visibility' =
      sequence_
        [ do treeStoreSetValue tracesStore path' (trace, visibility')
             updateChildren tracesStore path' subtrees' visibility'
        | (Node (trace, _) subtrees', n) <- zip subtrees [0..]
        , let path' = path ++ [n] ]

    updateParents :: TreeStore (Trace, Visibility) -> TreePath -> IO ()
    updateParents _           []   = return ()
    updateParents tracesStore path = do
      Node (trace, _) subtrees <- treeStoreGetTree tracesStore path
      let visibility = accumVisibility  [ vis | subtree  <- subtrees
                                              , (_, vis) <- flatten subtree ]
      treeStoreSetValue tracesStore path (trace, visibility)
      updateParents tracesStore (init path)

    invertVisibility Hidden = Visible
    invertVisibility _      = Hidden

    accumVisibility = foldr1 (\a b -> if a == b then a else MixedVisibility)

-- Find the HEC traces in the treeStore and replace them
traceViewSetHECs :: TraceView -> HECs -> IO ()
traceViewSetHECs TraceView{tracesStore} hecs = do
    treeStoreClear tracesStore
    -- for testing only (e.g., to compare with histogram of data from interval
    -- or to compare visually with other traces):
    -- treeStoreInsert tracesStore [] 0 (TraceHistogram, Visible)
    go 0
    treeStoreInsert tracesStore [] 0 (TraceActivity, Visible)
  where
    newT = Node { rootLabel = (TraceGroup "HEC Traces", Visible),
                  subForest = [ Node { rootLabel = (TraceHEC k, Visible),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    newI = Node { rootLabel = (TraceGroup "Instant Events", Hidden),
                  subForest = [ Node { rootLabel = (TraceInstantHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nCre = Node { rootLabel = (TraceGroup "Spark Creation", Hidden),
                  subForest = [ Node { rootLabel = (TraceCreationHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nCon = Node { rootLabel = (TraceGroup "Spark Conversion", Hidden),
                  subForest = [ Node { rootLabel = (TraceConversionHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    nPoo = Node { rootLabel = (TraceGroup "Spark Pool", Hidden),
                  subForest = [ Node { rootLabel = (TracePoolHEC k, Hidden),
                                       subForest = [] }
                              | k <- [ 0 .. hecCount hecs - 1 ] ] }
    go n = do
      m <- treeStoreLookup tracesStore [n]
      case m of
        Nothing -> do
          treeStoreInsertTree tracesStore [] 0 nPoo
          treeStoreInsertTree tracesStore [] 0 nCon
          treeStoreInsertTree tracesStore [] 0 nCre
          treeStoreInsertTree tracesStore [] 0 newI
          treeStoreInsertTree tracesStore [] 0 newT
        Just t  ->
          case t of
             Node { rootLabel = (TraceGroup "HEC Traces", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n newT
               go (n+1)
             Node { rootLabel = (TraceGroup "HEC Instant Events", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n newI
               go (n+1)
             Node { rootLabel = (TraceGroup "Spark Creation", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n nCre
               go (n+1)
             Node { rootLabel = (TraceGroup "Spark Conversion", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n nCon
               go (n+1)
             Node { rootLabel = (TraceGroup "Spark Pool", _) } -> do
               treeStoreRemove tracesStore [n]
               treeStoreInsertTree tracesStore [] n nPoo
               go (n+1)
             Node { rootLabel = (TraceActivity, _) } -> do
               treeStoreRemove tracesStore [n]
               go (n+1)
             _ ->
               go (n+1)

traceViewGetTraces :: TraceView -> IO [Trace]
traceViewGetTraces TraceView{tracesStore} = do
  f <- getTracesStoreContents tracesStore
  return [ t | (t, Visible) <- concatMap flatten f, notGroup t ]
 where
  notGroup (TraceGroup _) = False
  notGroup _              = True

getTracesStoreContents :: TreeStore a -> IO (Forest a)
getTracesStoreContents tracesStore = go 0
  where
  go !n = do
    m <- treeStoreLookup tracesStore [n]
    case m of
      Nothing -> return []
      Just t  -> do
        ts <- go (n+1)
        return (t:ts)
