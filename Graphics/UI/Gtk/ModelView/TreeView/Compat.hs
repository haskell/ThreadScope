{-# LANGUAGE CPP #-}
module Graphics.UI.Gtk.ModelView.TreeView.Compat
    ( treeViewSetModel
    ) where
import Graphics.UI.Gtk hiding (treeViewSetModel)
import qualified Graphics.UI.Gtk.ModelView.TreeView as Gtk
#if !MIN_VERSION_gtk(0, 14, 9)
import qualified System.Glib.FFI as Glib
import qualified Graphics.UI.GtkInternals as Gtk
#endif

treeViewSetModel
    :: (TreeViewClass self, TreeModelClass model)
    => self
    -> Maybe model
    -> IO ()
#if MIN_VERSION_gtk(0, 14, 9)
treeViewSetModel = Gtk.treeViewSetModel
#else
treeViewSetModel self model = Gtk.treeViewSetModel self
    (maybe (Gtk.TreeModel Glib.nullForeignPtr) toTreeModel model)
#endif
