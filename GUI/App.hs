{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------
-- | Module : GUI.App
--
-- Platform-specific application functionality
-------------------------------------------------------------------------------

module GUI.App (appTitle, initApp) where

-- Mac OS X-specific GTK imports
#if defined(darwin_HOST_OS)
import Control.Monad (void)
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OSX as OSX
import GUI.DataFiles (loadLogo)
#else
import Control.Monad (void)
import qualified Graphics.UI.Gtk as Gtk
import System.Glib.Utils (setProgramName)
#endif

-------------------------------------------------------------------------------

appTitle :: String
appTitle = "ThreadScope"

#if defined(darwin_HOST_OS)

-- | Initialize application
-- Perform Mac OS X-specific application initialization
initApp :: IO ()
initApp = do
  void Gtk.initGUI
  app <- OSX.applicationNew
  menuBar <- Gtk.menuBarNew
  OSX.applicationSetMenuBar app menuBar
  logo <- $loadLogo
  OSX.applicationSetDockIconPixbuf app (Just logo)
  OSX.applicationReady app

#else

-- | Initialize application
-- Perform application initialization for non-Mac OS X platforms
initApp :: IO ()
initApp = do
  setProgramName appTitle
  void Gtk.initGUI

#endif
