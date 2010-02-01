-------------------------------------------------------------------------------
--- $Id: About.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/About.hs $
-------------------------------------------------------------------------------

module About where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Paths_threadscope

-------------------------------------------------------------------------------

showAboutDialog :: Window -> IO ()
showAboutDialog parent 
 = do aboutDialog <- aboutDialogNew
      logoPath <- getDataFileName "threadscope.png"
      logo <- pixbufNewFromFile logoPath
      set aboutDialog [
         aboutDialogName      := "ThreadScope",
         aboutDialogVersion   := "0.1.1",
         aboutDialogCopyright := "Released under the GHC license as part of the Glasgow Haskell Compiler.",
         aboutDialogComments  := "A GHC eventlog profile viewer",
         aboutDialogAuthors   := ["Donnie Jones (donnie@darthik.com)",
                                  "Simon Marlow (simonm@microsoft.com)",
                                  "Satnam Singh (s.singh@ieee.org)"],
         aboutDialogLogo := Just logo,
         aboutDialogWebsite   := "http://research.microsoft.com/threadscope"
         ]
      windowSetTransientFor aboutDialog parent
      afterResponse aboutDialog $ \_ -> widgetDestroy aboutDialog
      widgetShow aboutDialog

-------------------------------------------------------------------------------
