-------------------------------------------------------------------------------
--- $Id: FileDialog.hs#1 2009/02/25 13:18:33 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/FileDialog.hs $
-------------------------------------------------------------------------------

module FileDialog
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

