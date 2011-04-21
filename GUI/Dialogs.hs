-------------------------------------------------------------------------------
--- $Id: About.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/About.hs $
-------------------------------------------------------------------------------

module GUI.Dialogs where

import Paths_threadscope (getDataFileName, version)

import Graphics.UI.Gtk
import Data.Version (showVersion)


-------------------------------------------------------------------------------

aboutDialog :: Window -> IO ()
aboutDialog parent
 = do dialog <- aboutDialogNew
      logoPath <- getDataFileName "threadscope.png"
      logo <- pixbufNewFromFile logoPath
      set dialog [
         aboutDialogName      := "ThreadScope",
         aboutDialogVersion   := showVersion version,
         aboutDialogCopyright := "Released under the GHC license as part of the Glasgow Haskell Compiler.",
         aboutDialogComments  := "A GHC eventlog profile viewer",
         aboutDialogAuthors   := ["Donnie Jones (donnie@darthik.com)",
                                  "Simon Marlow (simonm@microsoft.com)",
                                  "Satnam Singh (s.singh@ieee.org)"],
         aboutDialogLogo      := Just logo,
         aboutDialogWebsite   := "http://research.microsoft.com/threadscope",
         windowTransientFor   := parent
        ]
      onResponse dialog $ \_ -> widgetDestroy dialog
      widgetShow dialog

-------------------------------------------------------------------------------

openFileDialog :: Window -> (FilePath -> IO ()) -> IO ()
openFileDialog parent open
  = do dialog <- fileChooserDialogNew
                   (Just "Open Profile... ")
                   (Just parent)
                   FileChooserActionOpen
                   [("gtk-cancel", ResponseCancel)
                   ,("gtk-open", ResponseAccept)]
       set dialog [
           windowModal := True
         ]
       onResponse dialog $ \response -> do
         case response of
           ResponseAccept -> do
             mfile <- fileChooserGetFilename dialog
             case mfile of
               Just file -> open file
               Nothing   -> return ()
           _             -> return ()
         widgetDestroy dialog

       widgetShowAll dialog

-------------------------------------------------------------------------------
