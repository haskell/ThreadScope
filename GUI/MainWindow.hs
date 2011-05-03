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
       mainWinSavePDF       :: IO (),
       mainWinSavePNG       :: IO (),
       mainWinQuit          :: IO (),
       mainWinViewSidebar   :: Bool -> IO (),
       mainWinViewEvents    :: Bool -> IO (),
       mainWinViewBW        :: Bool -> IO (),
       mainWinViewRefresh   :: IO (),
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
  saveAsPDFMenuItem  <- getWidget castToMenuItem "saveAsPDFMenuItem"
  saveAsPNGMenuItem  <- getWidget castToMenuItem "saveAsPNGMenuItem"
  reloadMenuItem     <- getWidget castToMenuItem "view_reload"
  quitMenuItem       <- getWidget castToMenuItem "quitMenuItem"
  aboutMenuItem      <- getWidget castToMenuItem "aboutMenuItem"

--  timelineDrawingArea      <- getWidget castToDrawingArea "timeline_drawingarea"
--  timelineLabelDrawingArea <- getWidget castToDrawingArea "timeline_labels_drawingarea"
--  timelineHScrollbar  <- getWidget castToHScrollbar "timeline_hscroll"
--  timelineVScrollbar  <- getWidget castToVScrollbar "timeline_vscroll"
--  timelineAdj         <- rangeGetAdjustment timelineHScrollbar
--  timelineVAdj        <- rangeGetAdjustment timelineVScrollbar

  zoomInButton       <- getWidget castToToolButton "cpus_zoomin"
  zoomOutButton      <- getWidget castToToolButton "cpus_zoomout"
  zoomFitButton      <- getWidget castToToolButton "cpus_zoomfit"

  showLabelsToggle   <- getWidget castToToggleToolButton "cpus_showlabels"
  firstButton        <- getWidget castToToolButton "cpus_first"
  lastButton         <- getWidget castToToolButton "cpus_last"
  centreButton       <- getWidget castToToolButton "cpus_centre"

  --TODO: these two are currently unbound, but they should be!
  --  eventsTextEntry    <- getWidget castToEntry      "events_entry"
  --  eventsFindButton   <- getWidget castToToolButton "events_find"

  ------------------------------------------------------------------------

  widgetSetAppPaintable mainWindow True --TODO: Really?

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
  on saveAsPDFMenuItem menuItemActivate $ mainWinSavePDF actions
  on saveAsPNGMenuItem menuItemActivate $ mainWinSavePNG actions

  on quitMenuItem menuItemActivate $ mainWinQuit actions
  on mainWindow   objectDestroy    $ mainWinQuit actions

  on sidebarToggle  checkMenuItemToggled $ checkMenuItemGetActive sidebarToggle
                                       >>= mainWinViewSidebar actions
  on eventsToggle   checkMenuItemToggled $ checkMenuItemGetActive eventsToggle
                                       >>= mainWinViewEvents  actions
  on bwToggle       checkMenuItemToggled $ checkMenuItemGetActive bwToggle
                                       >>= mainWinViewBW      actions
  on reloadMenuItem menuItemActivate     $ mainWinViewRefresh actions

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
  onKeyPress mainWindow $ \Key { Old.eventKeyName = key, eventKeyChar = mch } -> do
    case (key, mch) of
      ("Right", _)   -> mainWinScrollLeft  actions
      ("Left",  _)   -> mainWinScrollRight actions
      (_ , Just '+') -> mainWinJumpZoomIn  actions
      (_ , Just '-') -> mainWinJumpZoomOut actions
      _              -> return ()
    return True

  ------------------------------------------------------------------------
  -- Show all windows
  widgetShowAll mainWindow

  return MainWindow {..}
