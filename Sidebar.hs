module Sidebar (
    setupSideBar
  ) where

import State

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Control.Monad
import Control.Monad.Trans

setupSideBar :: ViewerState -> IO ()
setupSideBar state@ViewerState{..} = do
  on sidebarCloseButton buttonPressEvent $ tryEvent $ liftIO $ do
     checkMenuItemSetActive sidebarToggle False
     containerRemove hpaned sidebarVBox
     
  onToggle sidebarToggle $ do -- no new-style event for menu toggles?
     b <- checkMenuItemGetActive sidebarToggle
     if b 
        then panedAdd1 hpaned sidebarVBox
        else containerRemove hpaned sidebarVBox

  return ()
