{-# LANGUAGE TemplateHaskell #-}
module GUI.MainWindow (
    MainWindow,
    mainWindowNew,
    MainWindowActions(..),

    setFileLoaded,
    setStatusMessage,
    sidebarSetVisibility,
    eventsSetVisibility,

  ) where

import Graphics.UI.Gtk as Gtk
import qualified System.Glib.GObject as Glib

import GUI.App (appTitle)
import GUI.DataFiles (loadLogo)

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
       mainWinWebsite       :: IO (),
       mainWinTutorial      :: IO (),
       mainWinAbout         :: IO (),

       -- Toolbar actions
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
      windowTitle := appTitle
    ]
setFileLoaded mainWin (Just file) =
  set (mainWindow mainWin) [
      windowTitle := file ++ " - " ++ appTitle
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
  labModeToggle      <- getWidget castToCheckMenuItem "view_labels_mode"
  sidebarToggle      <- getWidget castToCheckMenuItem "view_sidebar"
  eventsToggle       <- getWidget castToCheckMenuItem "view_events"
  openMenuItem       <- getWidget castToMenuItem "openMenuItem"
  exportMenuItem     <- getWidget castToMenuItem "exportMenuItem"
  reloadMenuItem     <- getWidget castToMenuItem "view_reload"
  quitMenuItem       <- getWidget castToMenuItem "quitMenuItem"
  websiteMenuItem    <- getWidget castToMenuItem "websiteMenuItem"
  tutorialMenuItem   <- getWidget castToMenuItem "tutorialMenuItem"
  aboutMenuItem      <- getWidget castToMenuItem "aboutMenuItem"

  firstMenuItem      <- getWidget castToMenuItem "move_first"
  centreMenuItem     <- getWidget castToMenuItem "move_centre"
  lastMenuItem       <- getWidget castToMenuItem "move_last"

  zoomInMenuItem     <- getWidget castToMenuItem "move_zoomin"
  zoomOutMenuItem    <- getWidget castToMenuItem "move_zoomout"
  zoomFitMenuItem    <- getWidget castToMenuItem "move_zoomfit"

  openButton         <- getWidget castToToolButton "cpus_open"

  firstButton        <- getWidget castToToolButton "cpus_first"
  centreButton       <- getWidget castToToolButton "cpus_centre"
  lastButton         <- getWidget castToToolButton "cpus_last"

  zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
  zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
  zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

  --TODO: this is currently not used, but it'be nice if it were!
  eventsTextEntry    <- getWidget castToEntry      "events_entry"

  ------------------------------------------------------------------------
  -- Show everything
  widgetShowAll mainWindow

  widgetHide eventsTextEntry  -- for now we hide it, see above.

  ------------------------------------------------------------------------

  logo <- $loadLogo
  set mainWindow [ windowIcon := Just logo ]

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
                                       >>= mainWinViewSidebar   actions
  on eventsToggle   checkMenuItemToggled $ checkMenuItemGetActive eventsToggle
                                       >>= mainWinViewEvents    actions
  on bwToggle       checkMenuItemToggled $ checkMenuItemGetActive bwToggle
                                       >>= mainWinViewBW        actions
  on labModeToggle  checkMenuItemToggled $ checkMenuItemGetActive labModeToggle
                                       >>= mainWinDisplayLabels actions
  on reloadMenuItem menuItemActivate     $ mainWinViewReload actions

  on websiteMenuItem  menuItemActivate    $ mainWinWebsite actions
  on tutorialMenuItem menuItemActivate    $ mainWinTutorial actions
  on aboutMenuItem    menuItemActivate    $ mainWinAbout actions

  on firstMenuItem   menuItemActivate     $ mainWinJumpStart  actions
  on centreMenuItem  menuItemActivate     $ mainWinJumpCursor actions
  on lastMenuItem    menuItemActivate     $ mainWinJumpEnd    actions

  on zoomInMenuItem  menuItemActivate     $ mainWinJumpZoomIn  actions
  on zoomOutMenuItem menuItemActivate     $ mainWinJumpZoomOut actions
  on zoomFitMenuItem menuItemActivate     $ mainWinJumpZoomFit actions

  -- Toolbar
  onToolButtonClicked openButton $ mainWinOpen       actions

  onToolButtonClicked firstButton  $ mainWinJumpStart  actions
  onToolButtonClicked centreButton $ mainWinJumpCursor actions
  onToolButtonClicked lastButton   $ mainWinJumpEnd    actions

  onToolButtonClicked zoomInButton  $ mainWinJumpZoomIn  actions
  onToolButtonClicked zoomOutButton $ mainWinJumpZoomOut actions
  onToolButtonClicked zoomFitButton $ mainWinJumpZoomFit actions

  return MainWindow {..}
