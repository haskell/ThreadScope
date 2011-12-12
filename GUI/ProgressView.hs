{-# LANGUAGE DeriveDataTypeable #-}

module GUI.ProgressView (
    ProgressView,
    withProgress,
    setText,
    setTitle,
    setProgress,
    startPulse,
  ) where

import Graphics.UI.Gtk as Gtk
import GUI.GtkExtras
import Graphics.Rendering.Cairo

import qualified Control.Concurrent as Concurrent
import Control.Exception
import Data.Typeable

import Prelude hiding (catch)


data ProgressView = ProgressView {
    progressWindow :: Gtk.Window,
    progressLabel  :: Gtk.Label,
    progressBar    :: Gtk.ProgressBar
  }

-- | Perform a long-running operation and display a progress window. The
-- operation has access to the progress window and it is expected to update it
-- using 'setText' and 'setProgress'
--
-- The user may cancel the operation at any time.
--
withProgress :: WindowClass win => win -> (ProgressView -> IO a) -> IO (Maybe a)
withProgress parent action = do
  self <- Concurrent.myThreadId
  let cancel = throwTo self OperationInterrupted
  bracket (new parent cancel) close $ \progress ->
    fmap Just (action progress)
      `catch` \OperationInterrupted -> return Nothing

data OperationInterrupted = OperationInterrupted
  deriving (Typeable, Show)
instance Exception OperationInterrupted

setText :: ProgressView -> String -> IO ()
setText view msg =
  set (progressBar view) [
    progressBarText := msg
  ]

setTitle :: ProgressView -> String -> IO ()
setTitle view msg = do
  set (progressWindow view) [ windowTitle := msg ]
  set (progressLabel view)  [ labelLabel  := "<b>" ++ msg ++ "</b>" ]

startPulse :: ProgressView -> IO (IO ())
startPulse view = do
  let pulse = do
        progressBarPulse (progressBar view)
        Concurrent.threadDelay 200000
        pulse
  thread <- Concurrent.forkIO $
              pulse `catch` \OperationInterrupted -> return ()
  let stop = throwTo thread OperationInterrupted
  waitGUI
  return stop

setProgress :: ProgressView -> Int -> Int -> IO ()
setProgress view total current = do
  let frac = fromIntegral current / fromIntegral total
  set (progressBar view) [ progressBarFraction := frac ]
  waitGUI

close :: ProgressView -> IO ()
close view = widgetDestroy (progressWindow view)

new :: WindowClass win => win -> IO () -> IO ProgressView
new parent cancelAction = do
  win <- windowNew
  set win [
      containerBorderWidth := 10,
      windowTitle := "",
      windowTransientFor := toWindow parent,
      windowModal := True,
      windowWindowPosition := WinPosCenterOnParent,
      windowDefaultWidth := 400,
      windowSkipTaskbarHint := True
    ]

  progText <- labelNew Nothing
  set progText [
      miscXalign := 0,
      labelUseMarkup := True
    ]

  progress <- progressBarNew

  cancel <- buttonNewFromStock stockCancel
  onClicked cancel (widgetDestroy win >> cancelAction)
  onDelete win (\_ -> cancelAction >> return True)
  on win keyPressEvent $ do
    keyVal <- eventKeyVal
    case keyVal of
      0xff1b -> liftIO $ cancelAction >> return True
      _      -> return False

  vbox <- vBoxNew False 20
  hbox <- hBoxNew False 0
  boxPackStart vbox progText PackRepel 10
  boxPackStart vbox progress PackGrow   5
  boxPackStart vbox hbox     PackNatural 5
  boxPackEnd   hbox cancel   PackNatural 0
  containerAdd win vbox

  widgetShowAll win

  return ProgressView {
    progressWindow = win,
    progressLabel  = progText,
    progressBar    = progress
  }
