module GUI.KeyView (
    KeyView,
    keyViewNew,
  ) where

import GUI.ViewerColours
import GUI.Timeline.Render.Constants

import Graphics.UI.Gtk
import qualified Graphics.Rendering.Cairo as C


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
          , ("thread runnable", Vertical, threadRunnableColour)
          , ("seq GC req",      Vertical, seqGCReqColour)
          , ("par GC req",      Vertical, parGCReqColour)
          , ("migrate thread",  Vertical, migrateThreadColour)
          , ("thread wakeup",   Vertical, threadWakeupColour)
          , ("shutdown",        Vertical, shutdownColour)

          , ("create spark",    Vertical, createdConvertedColour)
          , ("dud spark",       Vertical, fizzledDudsColour)
          , ("overflowed spark", Vertical, overflowedColour)
          , ("run spark",       Vertical, createdConvertedColour)
          , ("fizzled spark",   Vertical, fizzledDudsColour)
          , ("GCed spark",      Vertical, gcColour)

          , ("user message",    Vertical, userMessageColour)
          ]


createKeyEntries :: DrawableClass dw => dw
                 -> [(String, KeyStyle, Color)] -> IO [(String, Pixbuf)]
createKeyEntries similar entries =
  sequence
    [ do pixbuf <- renderToPixbuf similar (50, hecBarHeight) $ do
                     C.setSourceRGB 1 1 1
                     C.paint
                     renderKeyIcon style colour
         return (label, pixbuf)

    | (label, style, colour) <- entries ]

renderKeyIcon :: KeyStyle -> Color -> C.Render ()
renderKeyIcon Box keyColour = do
  setSourceRGBAhex keyColour 1.0
  C.rectangle 0 0 50 (fromIntegral (hecBarHeight `div` 2))
  C.fill
renderKeyIcon Vertical keyColour = do
  setSourceRGBAhex keyColour 1.0
  C.setLineWidth 3.0
  let x = fromIntegral ox
  C.moveTo x 0
  C.relLineTo 0 25
  C.stroke

renderToPixbuf :: DrawableClass dw => dw -> (Int, Int) -> C.Render ()
                  -> IO Pixbuf
renderToPixbuf similar (w, h) draw = do
  pixmap <- pixmapNew (Just similar) w h Nothing
  renderWithDrawable pixmap draw
  Just pixbuf <- pixbufGetFromDrawable pixmap (Rectangle 0 0 w h)
  return pixbuf

-------------------------------------------------------------------------------
