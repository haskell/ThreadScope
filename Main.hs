-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

import Options
import GUI.Main

import Graphics.UI.Gtk

import System.Environment


-------------------------------------------------------------------------------

main :: IO ()
main
  = do -- Deal with command line argument processing.
       -- This application accepts one optional argument specifying
       -- the event log.
       args <- getArgs
       let options = parseOptions args

       initGUI
       state <- buildInitialState options

       startup options state
