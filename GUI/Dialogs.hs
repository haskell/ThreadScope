{-# LANGUAGE TemplateHaskell #-}
module GUI.Dialogs where

import GUI.App (appTitle)
import GUI.DataFiles (loadLogo)
import Paths_threadscope (version)

import Graphics.UI.Gtk

import Data.Version (showVersion)
import System.FilePath


-------------------------------------------------------------------------------

aboutDialog :: WindowClass window => window -> IO ()
aboutDialog parent
 = do dialog <- aboutDialogNew
      logo <- $loadLogo
      set dialog [
         aboutDialogName      := appTitle,
         aboutDialogVersion   := showVersion version,
         aboutDialogCopyright := "Released under the GHC license as part of the Glasgow Haskell Compiler.",
         aboutDialogComments  := "A GHC eventlog profile viewer",
         aboutDialogAuthors   := ["Donnie Jones <donnie@darthik.com>",
                                  "Simon Marlow <simonm@microsoft.com>",
                                  "Satnam Singh <s.singh@ieee.org>",
                                  "Duncan Coutts <duncan@well-typed.com>",
                                  "Mikolaj Konarski <mikolaj@well-typed.com>",
                                  "Nicolas Wu <nick@well-typed.com>",
                                  "Eric Kow <eric@well-typed.com>"],
         aboutDialogLogo      := Just logo,
         aboutDialogWebsite   := "http://www.haskell.org/haskellwiki/ThreadScope",
         windowTransientFor   := toWindow parent
        ]
      onResponse dialog $ \_ -> widgetDestroy dialog
      widgetShow dialog

-------------------------------------------------------------------------------

openFileDialog :: WindowClass window => window -> (FilePath -> IO ()) -> IO ()
openFileDialog parent  open
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

data FileExportFormat = FormatPDF | FormatPNG

exportFileDialog :: WindowClass window => window
                 -> FilePath
                 -> (FilePath -> FileExportFormat -> IO ())
                 -> IO ()
exportFileDialog parent oldfile save = do
    dialog <- fileChooserDialogNew
                (Just "Save timeline image...")
                (Just (toWindow parent))
                FileChooserActionSave
                [("gtk-cancel", ResponseCancel)
                ,("gtk-save", ResponseAccept)]
    set dialog [
       fileChooserDoOverwriteConfirmation := True,
       windowModal := True
     ]

    let (olddir, oldfilename) = splitFileName oldfile
    fileChooserSetCurrentName   dialog (replaceExtension oldfilename "png")
    fileChooserSetCurrentFolder dialog olddir

    pngFiles <- fileFilterNew
    fileFilterSetName pngFiles "PNG bitmap files"
    fileFilterAddPattern pngFiles "*.png"
    fileChooserAddFilter dialog pngFiles

    pdfFiles <- fileFilterNew
    fileFilterSetName pdfFiles "PDF files"
    fileFilterAddPattern pdfFiles "*.pdf"
    fileChooserAddFilter dialog pdfFiles

    onResponse dialog $ \response ->
      case response of
        ResponseAccept -> do
          mfile <- fileChooserGetFilename dialog
          case mfile of
            Just file
              | takeExtension file == ".pdf" -> do
                  save file FormatPDF
                  widgetDestroy dialog
              | takeExtension file == ".png" -> do
                  save file FormatPNG
                  widgetDestroy dialog
              | otherwise ->
                  formatError dialog
            Nothing  -> widgetDestroy dialog
        _            -> widgetDestroy dialog

    widgetShowAll dialog
  where
    formatError dialog = do
      msg <- messageDialogNew (Just (toWindow dialog))
               [DialogModal, DialogDestroyWithParent]
               MessageError ButtonsClose
               "The file format is unknown or unsupported"
      set msg [
        messageDialogSecondaryText := Just $
             "The PNG and PDF formats are supported. "
          ++ "Please use a file extension of '.png' or '.pdf'."
        ]
      dialogRun msg
      widgetDestroy msg



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
