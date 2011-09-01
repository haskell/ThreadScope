-------------------------------------------------------------------------------
--- $Id: About.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/About.hs $
-------------------------------------------------------------------------------

module GUI.Dialogs where

import Paths_threadscope (getDataFileName, version)

import Graphics.UI.Gtk
import Data.Version (showVersion)


-------------------------------------------------------------------------------

aboutDialog :: WindowClass window => window -> IO ()
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
         windowTransientFor   := toWindow parent
        ]
      onResponse dialog $ \_ -> widgetDestroy dialog
      widgetShow dialog

-------------------------------------------------------------------------------

openFileDialog :: WindowClass window => window -> (FilePath -> IO ()) -> IO ()
openFileDialog parent open
  = do dialog <- fileChooserDialogNew
                   (Just "Open Profile...")
                   (Just (toWindow parent))
                   FileChooserActionOpen
                   [("gtk-cancel", ResponseCancel)
                   ,("gtk-open", ResponseAccept)]
       set dialog [
           windowModal := True
         ]

       eventlogfiles <- fileFilterNew
       fileFilterSetName eventlogfiles "GHC eventlog files (*.eventlog)"
       fileFilterAddPattern eventlogfiles "*.eventlog"
       fileChooserAddFilter dialog eventlogfiles

       allfiles <- fileFilterNew
       fileFilterSetName allfiles "All files"
       fileFilterAddPattern allfiles "*"
       fileChooserAddFilter dialog allfiles

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

errorMessageDialog :: WindowClass window => window -> String -> String -> IO ()
errorMessageDialog parent headline explanation = do

  dialog <- messageDialogNew (Just (toWindow parent))
              [] MessageError ButtonsNone ""

  set dialog
    [ windowModal := True
    , windowTransientFor := toWindow parent
    , messageDialogText  := Just headline
    , messageDialogSecondaryText := Just explanation
    , windowResizable := True
    ]

  dialogAddButton dialog "Close" ResponseClose
  dialogSetDefaultResponse dialog ResponseClose

  onResponse dialog $ \_-> widgetDestroy dialog
  widgetShowAll dialog
