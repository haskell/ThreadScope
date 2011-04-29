{-# LANGUAGE NamedFieldPuns #-}
module GUI.Traces (
    newHECs,
    getViewTraces
 ) where

import GUI.State (Trace(..), HECs(..))

import Graphics.UI.Gtk
import Data.Tree

-- Find the HEC traces in the treeStore and replace them
newHECs :: TreeStore (Trace, Bool) -> HECs -> IO ()
newHECs tracesStore hecs = do
  go 0
  treeStoreInsert tracesStore [] 0 (TraceActivity, True)
 where
  newt = Node { rootLabel = (TraceGroup "HECs", True),
                subForest = [ Node { rootLabel = (TraceHEC n, True),
                                     subForest = [] }
                            | n <- [ 0 .. hecCount hecs - 1 ] ] }

  go n = do
    m <- treeStoreLookup tracesStore [n]
    case m of
      Nothing -> treeStoreInsertTree tracesStore [] 0 newt
      Just t  ->
        case t of
           Node { rootLabel = (TraceGroup "HECs", _) } -> do
             treeStoreRemove tracesStore [n]
             treeStoreInsertTree tracesStore [] n newt
           Node { rootLabel = (TraceActivity, _) } -> do
             treeStoreRemove tracesStore [n]
             go (n+1)
           _ ->
             go (n+1)

getViewTraces :: TreeStore (Trace, Bool) -> IO [Trace]
getViewTraces tracesStore = do
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
