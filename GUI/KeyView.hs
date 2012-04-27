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

    let tooltipColumn = makeColumnIdString 0
    customStoreSetColumn keyStore tooltipColumn (\(_,tooltip,_) -> tooltip)
    treeViewSetModel keyTreeView keyStore

    set keyTreeView [ treeViewTooltipColumn := tooltipColumn ]

    cellLayoutSetAttributes keyColumn imageCell keyStore $ \(_,_,img) ->
      [ cellPixbuf := img ]
    cellLayoutSetAttributes keyColumn labelCell keyStore $ \(label,_,_) ->
      [ cellText := label ]

    ---------------------------------------------------------------------------

    return KeyView

-------------------------------------------------------------------------------

data KeyStyle = KDuration | KEvent | KEventAndGraph

keyData :: [(String, KeyStyle, Color, String)]
keyData =
  [ ("running",         KDuration, runningColour,
     "Indicates a period of time spent running Haskell code (not GC, not blocked/idle)")
  , ("GC",              KDuration, gcColour,
     "Indicates a period of time spent by the RTS performing garbage collection (GC)")
  , ("create thread",   KEvent, createThreadColour,
     "Indicates a new Haskell thread has been created")
  , ("seq GC req",      KEvent, seqGCReqColour,
     "Indicates a HEC has requested to start a sequential GC")
  , ("par GC req",      KEvent, parGCReqColour,
     "Indicates a HEC has requested to start a parallel GC")
  , ("migrate thread",  KEvent, migrateThreadColour,
     "Indicates a Haskell thread has been moved from one HEC to another")
  , ("thread wakeup",   KEvent, threadWakeupColour,
     "Indicates that a thread that was previously blocked (e.g. I/O, MVar etc) is now ready to run")
  , ("shutdown",        KEvent, shutdownColour,
     "Indicates a HEC is terminating")
  , ("user message",    KEvent, userMessageColour,
     "Indicates a message generated from Haskell code (via traceEvent)")
  , ("perf counter",    KEvent, createdConvertedColour,
     "Indicates an update of a perf counter")
  , ("perf tracepoint",    KEvent, shutdownColour,
     "Indicates that a perf tracepoint was reached")
  , ("create spark",    KEventAndGraph, createdConvertedColour,
     "As an event it indicates a use of `par` resulted in a spark being " ++
     "created (and added to the spark pool). In the spark creation " ++
     "graph the coloured area represents the number of sparks created.")
  , ("dud spark",       KEventAndGraph, fizzledDudsColour,
     "As an event it indicates a use of `par` resulted in the spark being " ++
     "discarded because it was a 'dud' (already evaluated). In the spark " ++
     "creation graph the coloured area represents the number of dud sparks.")
  , ("overflowed spark",KEventAndGraph, overflowedColour,
     "As an event it indicates a use of `par` resulted in the spark being " ++
     "discarded because the spark pool was full. In the spark creation " ++
     "graph the coloured area represents the number of overflowed sparks.")
  , ("run spark",       KEventAndGraph, createdConvertedColour,
     "As an event it indicates a spark has started to be run/evaluated. " ++
     "In the spark conversion graph the coloured area represents the number " ++
     "of sparks run.")
  , ("fizzled spark",   KEventAndGraph, fizzledDudsColour,
     "As an event it indicates a spark has 'fizzled', meaning it has been " ++
     "discovered that the spark's thunk was evaluated by some other thread. " ++
     "In the spark conversion  graph the coloured area represents the number " ++
     "of sparks that have fizzled.")
  , ("GCed spark",      KEventAndGraph, gcColour,
     "As an event it indicates a spark has been GC'd, meaning it has been " ++
     "discovered that the spark's thunk was no longer needed anywhere. " ++
     "In the spark conversion graph the coloured area represents the number " ++
     "of sparks that were GC'd.")
  ]


createKeyEntries :: DrawableClass dw
                 => dw
                 -> [(String, KeyStyle, Color,String)]
                 -> IO [(String, String, Pixbuf)]
createKeyEntries similar entries =
  sequence
    [ do pixbuf <- renderToPixbuf similar (50, hecBarHeight) $ do
                     C.setSourceRGB 1 1 1
                     C.paint
                     renderKeyIcon style colour
         return (label, tooltip, pixbuf)

    | (label, style, colour, tooltip) <- entries ]

renderKeyIcon :: KeyStyle -> Color -> C.Render ()
renderKeyIcon KDuration keyColour = do
  setSourceRGBAhex keyColour 1.0
  let x = fromIntegral ox
  C.rectangle (x - 2) 5 38 (fromIntegral (hecBarHeight `div` 2))
  C.fill
renderKeyIcon KEvent keyColour = renderKEvent keyColour
renderKeyIcon KEventAndGraph keyColour = do
  renderKEvent keyColour
  -- An icon roughly repreenting a jagedy graph.
  let x = fromIntegral ox
      y = fromIntegral hecBarHeight
  C.moveTo    (2*x)    (y - 2)
  C.relLineTo 3        (-6)
  C.relLineTo 3        0
  C.relLineTo 3        3
  C.relLineTo 5        1
  C.relLineTo 1        (-(y - 4))
  C.relLineTo 2        (y - 4)
  C.relLineTo 1        (-(y - 4))
  C.relLineTo 2        (y - 4)
  C.lineTo    (2*x+20) (y - 2)
  C.fill
  setSourceRGBAhex black 1.0
  C.setLineWidth 1.0
  C.moveTo    (2*x-4)  (y - 2.5)
  C.lineTo    (2*x+24) (y - 2.5)
  C.stroke

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
