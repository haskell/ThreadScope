-------------------------------------------------------------------------------
--- $Id: FileDialog.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/FileDialog.hs $
-------------------------------------------------------------------------------

module GUI.FileDialog
where

import Graphics.UI.Gtk

-------------------------------------------------------------------------------

openFileDialog :: Window -> IO (Maybe String)
openFileDialog parentWindow
  = do dialog <- fileChooserDialogNew
                   (Just "Open Profile... ")
                   (Just parentWindow)
	           FileChooserActionOpen
	           [("gtk-cancel", ResponseCancel)
	           ,("gtk-open", ResponseAccept)]
       widgetShow dialog
       response <- dialogRun dialog
       widgetHide dialog
       case response of
         ResponseAccept -> fileChooserGetFilename dialog
         _ -> return Nothing

-------------------------------------------------------------------------------

