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

data KeyStyle = KDuration | KEvent | KEventAndGraph

keyData :: [(String, KeyStyle, Color)]
keyData = [ ("running",         KDuration, runningColour)
          , ("GC",              KDuration, gcColour)

          , ("create thread",   KEvent, createThreadColour)
          , ("thread runnable", KEvent, threadRunnableColour)
          , ("seq GC req",      KEvent, seqGCReqColour)
          , ("par GC req",      KEvent, parGCReqColour)
          , ("migrate thread",  KEvent, migrateThreadColour)
          , ("thread wakeup",   KEvent, threadWakeupColour)
          , ("shutdown",        KEvent, shutdownColour)
          , ("user message",    KEvent, userMessageColour)

          , ("create spark",    KEventAndGraph, createdConvertedColour)
          , ("dud spark",       KEventAndGraph, fizzledDudsColour)
          , ("ovfled spark",    KEventAndGraph, overflowedColour)
          , ("run spark",       KEventAndGraph, createdConvertedColour)
          , ("fizzled spark",   KEventAndGraph, fizzledDudsColour)
          , ("GCed spark",      KEventAndGraph, gcColour)
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
renderKeyIcon KDuration keyColour = do
  setSourceRGBAhex keyColour 1.0
  let x = fromIntegral ox
  C.rectangle (x - 2) 5 38 (fromIntegral (hecBarHeight `div` 2))
  C.fill
renderKeyIcon KEvent keyColour = renderKEvent keyColour
renderKeyIcon KEventAndGraph keyColour = do
  renderKEvent keyColour
  let x = fromIntegral ox
  C.arc (3.1 * x) 11 5 0 (2 * pi)
  C.fill

renderKEvent :: Color -> C.Render ()
renderKEvent keyColour = do
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
