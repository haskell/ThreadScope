module GUI.KeyView (
    KeyView,
    keyViewNew,
  ) where

import GUI.ViewerColours
import GUI.Timeline.Render.Constants

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo as C


---------------------------------------------------------------------------

-- | Abstract key view object.
--
data KeyView = KeyView

---------------------------------------------------------------------------

keyViewNew :: Builder -> IO KeyView
keyViewNew builder = do

    keyTreeView <- builderGetObject builder castToTreeView "key_list"

    dw <- widgetGetDrawWindow keyTreeView
    keyEntries  <- createKeyEntries dw keyData

    keyStore    <- listStoreNew keyEntries
    keyColumn   <- treeViewColumnNew
    imageCell   <- cellRendererPixbufNew
    labelCell   <- cellRendererTextNew

    treeViewColumnPackStart keyColumn imageCell False
    treeViewColumnPackStart keyColumn labelCell True
    treeViewAppendColumn keyTreeView keyColumn

    selection <- treeViewGetSelection keyTreeView
    treeSelectionSetMode selection SelectionNone

    treeViewSetModel keyTreeView keyStore

    cellLayoutSetAttributes keyColumn imageCell keyStore $ \(_,img) ->
      [ cellPixbuf := img ]
    cellLayoutSetAttributes keyColumn labelCell keyStore $ \(label,_) ->
      [ cellText := label ]

    ---------------------------------------------------------------------------

    return KeyView

-------------------------------------------------------------------------------

data KeyStyle = Box | Vertical

keyData :: [(String, KeyStyle, Color)]
keyData = [ ("running",         Box,      runningColour)
          , ("GC",              Box,      gcColour)
          , ("create thread",   Vertical, createThreadColour)
          , ("run spark",       Vertical, createdConvertedColour)
          , ("thread runnable", Vertical, threadRunnableColour)
          , ("seq GC req",      Vertical, seqGCReqColour)
          , ("par GC req",      Vertical, parGCReqColour)
          , ("migrate thread",  Vertical, migrateThreadColour)
          , ("thread wakeup",   Vertical, threadWakeupColour)
          , ("shutdown",        Vertical, shutdownColour)
          ]


createKeyEntries :: DrawableClass dw => dw
                 -> [(String, KeyStyle, Color)] -> IO [(String, Pixbuf)]
createKeyEntries similar entries =
  sequence
    [ do pixbuf <- renderToPixbuf similar (50, hecBarHeight) $ do
                     setSourceRGB 1 1 1
                     paint
                     renderKeyIcon style colour
         return (label, pixbuf)

    | (label, style, colour) <- entries ]

renderKeyIcon :: KeyStyle -> Color -> Render ()
renderKeyIcon Box keyColour = do
  setSourceRGBAhex keyColour 1.0
  rectangle 0 0 50 (fromIntegral (hecBarHeight `div` 2))
  C.fill
renderKeyIcon Vertical keyColour = do
  setSourceRGBAhex keyColour 1.0
  setLineWidth 3.0
  moveTo 10 0
  relLineTo 0 25
  C.stroke

renderToPixbuf :: DrawableClass dw => dw -> (Int, Int) -> Render () -> IO Pixbuf
renderToPixbuf similar (w, h) draw = do
  pixmap <- pixmapNew (Just similar) w h Nothing
  renderWithDrawable pixmap draw
  Just pixbuf <- pixbufGetFromDrawable pixmap (Rectangle 0 0 w h)
  return pixbuf

-------------------------------------------------------------------------------
