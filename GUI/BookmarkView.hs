module GUI.BookmarkView (
    BookmarkView,
    bookmarkViewNew,
    BookmarkViewActions(..),

    bookmarkViewGet,
    bookmarkViewAdd,
    bookmarkViewRemove,
    bookmarkViewClear,
  ) where

import GHC.RTS.Events (Timestamp)

import Graphics.UI.Gtk
import Numeric

---------------------------------------------------------------------------

-- | Abstract bookmark view object.
--
data BookmarkView = BookmarkView {
       bookmarkStore :: ListStore Timestamp
     }

-- | The actions to take in response to TraceView events.
--
data BookmarkViewActions = BookmarkViewActions {
       bookmarkViewAddBookmark    :: IO (),
       bookmarkViewRemoveBookmark :: Int -> IO (),
       bookmarkViewGotoBookmark   :: Timestamp -> IO ()
     }

---------------------------------------------------------------------------

bookmarkViewAdd :: BookmarkView -> Timestamp -> IO ()
bookmarkViewAdd BookmarkView{bookmarkStore} ts = do
  listStoreAppend bookmarkStore ts
  return ()

bookmarkViewRemove :: BookmarkView -> Int -> IO ()
bookmarkViewRemove BookmarkView{bookmarkStore} n = do
  listStoreRemove bookmarkStore n
  return ()

bookmarkViewClear :: BookmarkView -> IO ()
bookmarkViewClear BookmarkView{bookmarkStore} =
  listStoreClear bookmarkStore

bookmarkViewGet :: BookmarkView -> IO [Timestamp]
bookmarkViewGet BookmarkView{bookmarkStore} =
  listStoreToList bookmarkStore

---------------------------------------------------------------------------

bookmarkViewNew :: Builder -> BookmarkViewActions -> IO BookmarkView
bookmarkViewNew builder BookmarkViewActions{..} = do

    let getWidget cast name = builderGetObject builder cast name

    ---------------------------------------------------------------------------

    bookmarkTreeView <- getWidget castToTreeView "bookmark_list"
    bookmarkStore    <- listStoreNew []
    bookmarkColumn   <- treeViewColumnNew
    cell             <- cellRendererTextNew
    selection        <- treeViewGetSelection bookmarkTreeView

    treeViewColumnSetTitle bookmarkColumn "Time"
    treeViewColumnPackStart bookmarkColumn cell True
    treeViewAppendColumn bookmarkTreeView bookmarkColumn

    treeViewSetModel bookmarkTreeView bookmarkStore

    cellLayoutSetAttributes bookmarkColumn cell bookmarkStore $ \time ->
      [ cellText := showFFloat (Just 6) (fromIntegral time / 1000000000) "s" ]

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
          ts <- listStoreGetValue bookmarkStore pos
          bookmarkViewGotoBookmark ts

    onRowActivated bookmarkTreeView $ \[pos] _ -> do
      ts <- listStoreGetValue bookmarkStore pos
      bookmarkViewGotoBookmark ts

    ---------------------------------------------------------------------------

    return BookmarkView{..}
