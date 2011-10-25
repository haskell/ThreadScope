module GUI.BookmarkView (
    BookmarkView,
    bookmarkViewNew,
    BookmarkViewActions(..),

    bookmarkViewGet,
    bookmarkViewAdd,
    bookmarkViewRemove,
    bookmarkViewClear,
    bookmarkViewSetLabel,
  ) where

import GHC.RTS.Events (Timestamp)

import Graphics.UI.Gtk
import Numeric

---------------------------------------------------------------------------

-- | Abstract bookmark view object.
--
data BookmarkView = BookmarkView {
       bookmarkStore :: ListStore (Timestamp, String)
     }

-- | The actions to take in response to TraceView events.
--
data BookmarkViewActions = BookmarkViewActions {
       bookmarkViewAddBookmark    :: IO (),
       bookmarkViewRemoveBookmark :: Int -> IO (),
       bookmarkViewGotoBookmark   :: Timestamp -> IO (),
       bookmarkViewEditLabel      :: Int -> String -> IO ()
     }

---------------------------------------------------------------------------

bookmarkViewAdd :: BookmarkView -> Timestamp -> String -> IO ()
bookmarkViewAdd BookmarkView{bookmarkStore} ts label = do
  listStoreAppend bookmarkStore (ts, label)
  return ()

bookmarkViewRemove :: BookmarkView -> Int -> IO ()
bookmarkViewRemove BookmarkView{bookmarkStore} n = do
  listStoreRemove bookmarkStore n
  return ()

bookmarkViewClear :: BookmarkView -> IO ()
bookmarkViewClear BookmarkView{bookmarkStore} =
  listStoreClear bookmarkStore

bookmarkViewGet :: BookmarkView -> IO [(Timestamp, String)]
bookmarkViewGet BookmarkView{bookmarkStore} =
  listStoreToList bookmarkStore

bookmarkViewSetLabel :: BookmarkView -> Int -> String -> IO ()
bookmarkViewSetLabel BookmarkView{bookmarkStore} n label = do
  (ts,_) <- listStoreGetValue bookmarkStore n
  listStoreSetValue bookmarkStore n (ts, label)

---------------------------------------------------------------------------

bookmarkViewNew :: Builder -> BookmarkViewActions -> IO BookmarkView
bookmarkViewNew builder BookmarkViewActions{..} = do

    let getWidget cast name = builderGetObject builder cast name

    ---------------------------------------------------------------------------

    bookmarkTreeView <- getWidget castToTreeView "bookmark_list"
    bookmarkStore    <- listStoreNew []
    columnTs         <- treeViewColumnNew
    cellTs           <- cellRendererTextNew
    columnLabel      <- treeViewColumnNew
    cellLabel        <- cellRendererTextNew
    selection        <- treeViewGetSelection bookmarkTreeView

    treeViewColumnSetTitle columnTs    "Time"
    treeViewColumnSetTitle columnLabel "Label"
    treeViewColumnPackStart columnTs    cellTs    False
    treeViewColumnPackStart columnLabel cellLabel True
    treeViewAppendColumn bookmarkTreeView columnTs
    treeViewAppendColumn bookmarkTreeView columnLabel

    treeViewSetModel bookmarkTreeView bookmarkStore

    cellLayoutSetAttributes columnTs cellTs bookmarkStore $ \(ts,_) ->
      [ cellText := showFFloat (Just 6) (fromIntegral ts / 1000000) "s" ]

    cellLayoutSetAttributes columnLabel cellLabel bookmarkStore $ \(_,label) ->
      [ cellText := label ]

    ---------------------------------------------------------------------------

    addBookmarkButton    <- getWidget castToToolButton "add_bookmark_button"
    deleteBookmarkButton <- getWidget castToToolButton "delete_bookmark"
    gotoBookmarkButton   <- getWidget castToToolButton "goto_bookmark_button"

    onToolButtonClicked addBookmarkButton $
      bookmarkViewAddBookmark

    onToolButtonClicked deleteBookmarkButton $ do
      selected <- treeSelectionGetSelected selection
      case selected of
        Nothing   -> return ()
        Just iter ->
          let pos = listStoreIterToIndex iter
           in bookmarkViewRemoveBookmark pos

    onToolButtonClicked gotoBookmarkButton $ do
      selected <- treeSelectionGetSelected selection
      case selected of
        Nothing   -> return ()
        Just iter -> do
          let pos = listStoreIterToIndex iter
          (ts,_) <- listStoreGetValue bookmarkStore pos
          bookmarkViewGotoBookmark ts

    onRowActivated bookmarkTreeView $ \[pos] _ -> do
      (ts, _) <- listStoreGetValue bookmarkStore pos
      bookmarkViewGotoBookmark ts

    set cellLabel [ cellTextEditable := True ]
    on cellLabel edited $ \[pos] val -> do
      bookmarkViewEditLabel pos val

    ---------------------------------------------------------------------------

    return BookmarkView{..}
