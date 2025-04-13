{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module GUI.GtkExtras where

-- This is all stuff that should be bound in the gtk package but is not yet
-- (as of gtk-0.12.0)

import Graphics.UI.GtkInternals
import Graphics.UI.Gtk (Rectangle)
import System.Glib.MainLoop
import Graphics.Rendering.Pango.Types
import Graphics.Rendering.Pango.BasicTypes
import Graphics.UI.Gtk.General.Enums (StateType, ShadowType)

import Foreign
import Foreign.C
import Control.Concurrent.MVar

#if mingw32_HOST_OS || mingw32_TARGET_OS
#include "windows_cconv.h"
#else
import System.Glib.GError
import Control.Monad
#endif

waitGUI :: IO ()
waitGUI = do
  resultVar <- newEmptyMVar
  idleAdd (putMVar resultVar () >> return False) priorityDefaultIdle
  takeMVar resultVar

-------------------------------------------------------------------------------

launchProgramForURI :: String -> IO Bool
#if mingw32_HOST_OS || mingw32_TARGET_OS
launchProgramForURI uri = do
    withCString "open" $ \verbPtr ->
      withCString uri $ \filePtr ->
        c_ShellExecuteA
            nullPtr
            verbPtr
            filePtr
            nullPtr
            nullPtr
            1       -- SW_SHOWNORMAL
    return True

foreign import WINDOWS_CCONV unsafe "shlobj.h ShellExecuteA"
    c_ShellExecuteA :: Ptr ()  -- HWND hwnd
                    -> CString -- LPCTSTR lpOperation
                    -> CString -- LPCTSTR lpFile
                    -> CString -- LPCTSTR lpParameters
                    -> CString -- LPCTSTR lpDirectory
                    -> CInt    -- INT nShowCmd
                    -> IO CInt -- HINSTANCE return

#else
launchProgramForURI uri =
  propagateGError $ \errPtrPtr ->
    withCString uri $ \uriStrPtr -> do
      timestamp <- gtk_get_current_event_time
      liftM toBool $ gtk_show_uri nullPtr uriStrPtr timestamp errPtrPtr
#endif

-------------------------------------------------------------------------------

foreign import ccall safe "gtk_show_uri"
  gtk_show_uri :: Ptr Screen -> Ptr CChar -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "gtk_get_current_event_time"
  gtk_get_current_event_time :: IO CUInt
