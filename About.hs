-------------------------------------------------------------------------------
--- $Id: About.hs#5 2009/03/10 17:30:04 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/About.hs $
-------------------------------------------------------------------------------

module About where

-- Imports for GTK/Glade
import Graphics.UI.Gtk

-------------------------------------------------------------------------------

showAboutDialog :: Window -> IO ()
showAboutDialog parent 
 = do aboutDialog <- aboutDialogNew
      set aboutDialog [
         aboutDialogName      := "ThreadScope",
         aboutDialogVersion   := "0.1",
         aboutDialogCopyright := "Released under the GHC license.",
         aboutDialogComments  := "A GHC eventlog profile viewer",
         aboutDialogAuthors   := ["Donnie Jones (donnie@darthik.com)",
                                  "Simon Marlow (simonm@microsoft.com)",
                                  "Satnam Singh (s.singh@ieee.org)"],
         aboutDialogWebsite   := "http://raintown.org/threadscope"
         ]
      windowSetTransientFor aboutDialog parent
      afterResponse aboutDialog $ \_ -> widgetDestroy aboutDialog
      widgetShow aboutDialog

-------------------------------------------------------------------------------
