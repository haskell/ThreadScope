{-# LANGUAGE ForeignFunctionInterface #-}
module GUI.GtkExtras where

-- This is all stuff that should be bound in the gtk package but is not yet
-- (as of gtk-0.12.0)

import Graphics.UI.GtkInternals
import Graphics.UI.Gtk (Rectangle)
import System.Glib.Attributes
import Graphics.Rendering.Pango.Types
import Graphics.Rendering.Pango.BasicTypes
import Graphics.UI.Gtk.General.Enums (StateType, ShadowType)
import Foreign
import Foreign.C
import Control.Monad


widgetState :: WidgetClass self => Attr self StateType
widgetState = newAttr
  widgetGetState
  widgetSetState

widgetGetState :: WidgetClass self => self -> IO StateType
widgetGetState widget =
  liftM (toEnum . fromIntegral) $
  (\(Widget arg1) -> withForeignPtr arg1 $ \argPtr1 -> gtk_widget_get_state argPtr1)
    (toWidget widget)

widgetSetState :: WidgetClass self => self -> StateType -> IO ()
widgetSetState widget state =
  (\(Widget arg1) arg2 -> withForeignPtr arg1 $ \argPtr1 -> gtk_widget_set_state argPtr1 arg2)
    (toWidget widget)
    ((fromIntegral . fromEnum) state)


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

-------------------------------------------------------------------------------

foreign import ccall safe "gtk_widget_get_state"
  gtk_widget_get_state :: Ptr Widget -> IO CInt

foreign import ccall safe "gtk_widget_set_state"
  gtk_widget_set_state :: Ptr Widget -> CInt -> IO ()

foreign import ccall safe "gtk_paint_flat_box"
  gtk_paint_flat_box :: Ptr Style -> Ptr DrawWindow -> CInt -> CInt -> Ptr () -> Ptr Widget -> Ptr CChar -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "gtk_paint_layout"
  gtk_paint_layout :: Ptr Style -> Ptr DrawWindow -> CInt -> CInt -> Ptr () -> Ptr Widget -> Ptr CChar -> CInt -> CInt -> Ptr PangoLayoutRaw -> IO ()
