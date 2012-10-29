{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module GUI.GtkExtras where

-- This is all stuff that should be bound in the gtk package but is not yet
-- (as of gtk-0.12.0)

import Graphics.UI.GtkInternals
import Graphics.UI.Gtk (Rectangle)
import System.Glib.GError
import System.Glib.MainLoop
import Graphics.Rendering.Pango.Types
import Graphics.Rendering.Pango.BasicTypes
import Graphics.UI.Gtk.General.Enums (StateType, ShadowType)

import Foreign
import Foreign.C
import Control.Monad
import Control.Concurrent.MVar

waitGUI :: IO ()
waitGUI = do
  resultVar <- newEmptyMVar
  idleAdd (putMVar resultVar () >> return False) priorityDefaultIdle
  takeMVar resultVar

-------------------------------------------------------------------------------

stylePaintFlatBox :: WidgetClass widget
                  => Style
                  -> DrawWindow
                  -> StateType
                  -> ShadowType
                  -> Rectangle
                  -> widget
                  -> String
                  -> Int -> Int -> Int -> Int
                  -> IO ()
stylePaintFlatBox style window stateType shadowType
                  clipRect widget detail x y width height =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  (\(Style arg1) (DrawWindow arg2) arg3 arg4 arg5 (Widget arg6) arg7 arg8 arg9 arg10 arg11 -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg6 $ \argPtr6 -> gtk_paint_flat_box argPtr1 argPtr2 arg3 arg4 arg5 argPtr6 arg7 arg8 arg9 arg10 arg11)
    style
    window
    ((fromIntegral.fromEnum) stateType)
    ((fromIntegral.fromEnum) shadowType)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    (fromIntegral width) (fromIntegral height)

stylePaintLayout :: WidgetClass widget
                 => Style
                 -> DrawWindow
                 -> StateType
                 -> Bool
                 -> Rectangle
                 -> widget
                 -> String
                 -> Int -> Int
                 -> PangoLayout
                 -> IO ()
stylePaintLayout style window stateType useText
                  clipRect widget detail x y (PangoLayout _ layout) =
  with clipRect $ \rectPtr ->
  withCString detail $ \detailPtr ->
  (\(Style arg1) (DrawWindow arg2) arg3 arg4 arg5 (Widget arg6) arg7 arg8 arg9 (PangoLayoutRaw arg10) -> withForeignPtr arg1 $ \argPtr1 ->withForeignPtr arg2 $ \argPtr2 ->withForeignPtr arg6 $ \argPtr6 ->withForeignPtr arg10 $ \argPtr10 -> gtk_paint_layout argPtr1 argPtr2 arg3 arg4 arg5 argPtr6 arg7 arg8 arg9 argPtr10)
    style
    window
    ((fromIntegral.fromEnum) stateType)
    (fromBool useText)
    (castPtr rectPtr)
    (toWidget widget)
    detailPtr
    (fromIntegral x) (fromIntegral y)
    layout


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

foreign import stdcall unsafe "shlobj.h ShellExecuteA"
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

foreign import ccall safe "gtk_paint_flat_box"
  gtk_paint_flat_box :: Ptr Style -> Ptr DrawWindow -> CInt -> CInt -> Ptr () -> Ptr Widget -> Ptr CChar -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "gtk_paint_layout"
  gtk_paint_layout :: Ptr Style -> Ptr DrawWindow -> CInt -> CInt -> Ptr () -> Ptr Widget -> Ptr CChar -> CInt -> CInt -> Ptr PangoLayoutRaw -> IO ()

foreign import ccall safe "gtk_show_uri"
  gtk_show_uri :: Ptr Screen -> Ptr CChar -> CUInt -> Ptr (Ptr ()) -> IO CInt

foreign import ccall unsafe "gtk_get_current_event_time"
  gtk_get_current_event_time :: IO CUInt
