{-# LANGUAGE CPP #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module GUI.MainWindow (
    MainWindow,
    mainWindowNew,
    MainWindowActions(..),

    setFileLoaded,
    setStatusMessage,
    sidebarSetVisibility,
    eventsSetVisibility,

  ) where

import Paths_threadscope

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Gdk.Events as Old hiding (eventModifier)
import System.Glib.GObject as Glib


-------------------------------------------------------------------------------

data MainWindow = MainWindow {
       mainWindow         :: Window,

       sidebarBox,
       eventsBox          :: Widget,

       statusBar          :: Statusbar,
       statusBarCxt       :: ContextId
     }

instance Glib.GObjectClass  MainWindow where
  toGObject = toGObject . mainWindow
  unsafeCastGObject = error "cannot downcast to MainView type"

instance Gtk.ObjectClass    MainWindow
instance Gtk.WidgetClass    MainWindow
instance Gtk.ContainerClass MainWindow
instance Gtk.BinClass       MainWindow
instance Gtk.WindowClass    MainWindow

data MainWindowActions = MainWindowActions {

       -- Menu actions
       mainWinOpen          :: IO (),
       mainWinExport        :: IO (),
       mainWinQuit          :: IO (),
       mainWinViewSidebar   :: Bool -> IO (),
       mainWinViewEvents    :: Bool -> IO (),
       mainWinViewBW        :: Bool -> IO (),
       mainWinViewReload    :: IO (),
       mainWinAbout         :: IO (),

       -- Toolbar actions
       --TODO: all toolbar actions should also be available from the menu
       mainWinJumpStart     :: IO (),
       mainWinJumpEnd       :: IO (),
       mainWinJumpCursor    :: IO (),
       mainWinJumpZoomIn    :: IO (),
       mainWinJumpZoomOut   :: IO (),
       mainWinJumpZoomFit   :: IO (),
       mainWinScrollLeft    :: IO (),
       mainWinScrollRight   :: IO (),
       mainWinDisplayLabels :: Bool -> IO ()
     }

-------------------------------------------------------------------------------

setFileLoaded :: MainWindow -> Maybe FilePath -> IO ()
setFileLoaded mainWin Nothing =
  set (mainWindow mainWin) [
      windowTitle := "ThreadScope"
    ]
setFileLoaded mainWin (Just file) =
  set (mainWindow mainWin) [
      windowTitle := file ++ " - ThreadScope"
    ]

setStatusMessage :: MainWindow -> String -> IO ()
setStatusMessage mainWin msg = do
  statusbarPop  (statusBar mainWin) (statusBarCxt mainWin)
  statusbarPush (statusBar mainWin) (statusBarCxt mainWin) (' ':msg)
  return ()

sidebarSetVisibility :: MainWindow -> Bool -> IO ()
sidebarSetVisibility mainWin visible =
  set (sidebarBox mainWin) [ widgetVisible := visible ]

eventsSetVisibility :: MainWindow -> Bool -> IO ()
eventsSetVisibility mainWin visible =
  set (eventsBox mainWin) [ widgetVisible := visible ]

-------------------------------------------------------------------------------

mainWindowNew :: Builder -> MainWindowActions -> IO MainWindow
mainWindowNew builder actions = do

  let getWidget cast name = builderGetObject builder cast name


  mainWindow         <- getWidget castToWindow "main_window"
  statusBar          <- getWidget castToStatusbar "statusbar"

  sidebarBox         <- getWidget castToWidget "sidebar"
  eventsBox          <- getWidget castToWidget "eventsbox"

  bwToggle           <- getWidget castToCheckMenuItem "black_and_white"
  sidebarToggle      <- getWidget castToCheckMenuItem "view_sidebar"
  eventsToggle       <- getWidget castToCheckMenuItem "view_events"
  openMenuItem       <- getWidget castToMenuItem "openMenuItem"
  exportMenuItem     <- getWidget castToMenuItem "exportMenuItem"
  reloadMenuItem     <- getWidget castToMenuItem "view_reload"
  quitMenuItem       <- getWidget castToMenuItem "quitMenuItem"
  aboutMenuItem      <- getWidget castToMenuItem "aboutMenuItem"

  timelineViewport   <- getWidget castToWidget "timeline_viewport"

  zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
  zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
  zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

  showLabelsToggle   <- getWidget castToToggleToolButton "cpus_showlabels"
  firstButton        <- getWidget castToToolButton "cpus_first"
  lastButton         <- getWidget castToToolButton "cpus_last"
  centreButton       <- getWidget castToToolButton "cpus_centre"

  --TODO: this is currently not used, but it'be nice if it were!
  eventsTextEntry    <- getWidget castToEntry      "events_entry"

  ------------------------------------------------------------------------
  -- Show everything
  widgetShowAll mainWindow

  widgetHide eventsTextEntry  -- for now we hide it, see above.

#ifndef USE_SPARK_HISTOGRAM
  -- If we're not enabling the histogram feature then hide the tab.
  histogram_drawingarea <- getWidget castToWidget "histogram_drawingarea"
  eventsbox             <- getWidget castToNotebook "eventsbox"
  Just pgnum <- notebookPageNum eventsbox histogram_drawingarea
  notebookRemovePage eventsbox  pgnum
#endif

  ------------------------------------------------------------------------

  logoPath <- getDataFileName "threadscope.png"
  windowSetIconFromFile mainWindow logoPath

  ------------------------------------------------------------------------
  -- Status bar functionality

  statusBarCxt <- statusbarGetContextId statusBar "file"
  statusbarPush statusBar statusBarCxt "No eventlog loaded."

  ------------------------------------------------------------------------
  -- Bind all the events
  
  -- Menus
  on openMenuItem      menuItemActivate $ mainWinOpen actions
  on exportMenuItem    menuItemActivate $ mainWinExport actions

  on quitMenuItem menuItemActivate $ mainWinQuit actions
  on mainWindow   objectDestroy    $ mainWinQuit actions

  on sidebarToggle  checkMenuItemToggled $ checkMenuItemGetActive sidebarToggle
                                       >>= mainWinViewSidebar actions
  on eventsToggle   checkMenuItemToggled $ checkMenuItemGetActive eventsToggle
                                       >>= mainWinViewEvents  actions
  on bwToggle       checkMenuItemToggled $ checkMenuItemGetActive bwToggle
                                       >>= mainWinViewBW      actions
  on reloadMenuItem menuItemActivate     $ mainWinViewReload actions

  on aboutMenuItem  menuItemActivate     $ mainWinAbout actions

  -- Toolbar  
  onToolButtonClicked firstButton  $ mainWinJumpStart  actions
  onToolButtonClicked lastButton   $ mainWinJumpEnd    actions
  onToolButtonClicked centreButton $ mainWinJumpCursor actions

  onToolButtonClicked zoomInButton  $ mainWinJumpZoomIn  actions
  onToolButtonClicked zoomOutButton $ mainWinJumpZoomOut actions
  onToolButtonClicked zoomFitButton $ mainWinJumpZoomFit actions
   
  onToolButtonToggled showLabelsToggle $
    toggleToolButtonGetActive showLabelsToggle >>= mainWinDisplayLabels actions

  -- Key bindings
  --TODO: move these to the timeline module
  onKeyPress timelineViewport $ \Key { Old.eventKeyName = key, eventKeyChar = mch } ->
    case (key, mch) of
      ("Right", _)   -> mainWinScrollRight actions >> return True
      ("Left",  _)   -> mainWinScrollLeft  actions >> return True
      (_ , Just '+') -> mainWinJumpZoomIn  actions >> return True
      (_ , Just '-') -> mainWinJumpZoomOut actions >> return True
      _              -> return False

  return MainWindow {..}
