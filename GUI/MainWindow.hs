{-# LANGUAGE CPP #-}
-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module GUI.MainWindow (
    MainWindow,
    mainWindowNew,
    MainWindowActions(..),

  ) where

import Paths_threadscope

-- Imports for GTK
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Gdk.Events as Old hiding (eventModifier)


-------------------------------------------------------------------------------

data MainWindow = MainWindow {
       mainWindow         :: Window,
       statusBar          :: Statusbar
     }

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
       mainWinDisplayLabels :: IO (),
       
       --TODO: decide if these should stay here or move to sidebar,
       --      or alternatiely, merge sidebar here
       mainWinAddBookmark    :: IO (),
       mainWinRemoveBookmark :: IO (),
       mainWinGotoBookmark   :: IO ()
     }

-------------------------------------------------------------------------------

mainWindowNew :: Builder -> MainWindowActions -> IO MainWindow
mainWindowNew builder actions = do

  let getWidget cast name = builderGetObject builder cast name


  mainWindow         <- getWidget castToWindow "main_window"
  statusBar          <- getWidget castToStatusbar "statusbar"

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

--  bookmarkTreeView   <- getWidget castToTreeView "bookmark_list"

  -- Bookmarks
  addBookmarkButton    <- getWidget castToToolButton "add_bookmark_button"
  deleteBookmarkButton <- getWidget castToToolButton "delete_bookmark"
  gotoBookmarkButton   <- getWidget castToToolButton "goto_bookmark_button"

  ------------------------------------------------------------------------

  widgetSetAppPaintable mainWindow True --TODO: Really?

  logoPath <- getDataFileName "threadscope.png"
  windowSetIconFromFile mainWindow logoPath

  ------------------------------------------------------------------------
  -- Status bar functionality

  ctx <- statusbarGetContextId statusBar "file"
  statusbarPush statusBar ctx "No eventlog loaded."

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
   
  onToolButtonToggled showLabelsToggle $ mainWinDisplayLabels actions

  -- Key bindings
  onKeyPress mainWindow $ \Key { Old.eventKeyName = key, eventKeyChar = mch } -> do
    case (key, mch) of
      ("Right", _)   -> mainWinScrollLeft  actions
      ("Left",  _)   -> mainWinScrollRight actions
      (_ , Just '+') -> mainWinJumpZoomIn  actions
      (_ , Just '-') -> mainWinJumpZoomOut actions
      _              -> return ()
    return True

  -- Sidebar buttons
  -- TODO: probably should move the bodies of these handlers elsewhere
  onToolButtonClicked addBookmarkButton    $ mainWinAddBookmark    actions
  onToolButtonClicked deleteBookmarkButton $ mainWinRemoveBookmark actions
  onToolButtonClicked gotoBookmarkButton   $ mainWinGotoBookmark   actions

  ------------------------------------------------------------------------
  -- Show all windows
  widgetShowAll mainWindow

  return MainWindow {..}
