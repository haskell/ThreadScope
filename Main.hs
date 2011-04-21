-- ThreadScope: a graphical viewer for Haskell event log information.
-- Maintainer: satnams@microsoft.com, s.singh@ieee.org

module Main where

import GUI.Main (runGUI)

import System.Environment
import Control.Monad

-------------------------------------------------------------------------------

main :: IO ()
main
  = do -- Deal with command line argument processing.
       -- This application accepts one optional argument specifying
       -- the event log.
       args <- getArgs
       let options = parseOptions args

           debug      = Debug `elem` options
           filenames  = [filename | Filename filename <- options]
           tracenames = [name | TestTrace name <- options]

       when (length filenames > 1)
         (putStrLn "usage: threadscope [eventlog_filename]")

       let filename = if filenames == [] then
                       ""
                      else
                        head filenames
           traceName = if tracenames == [] then
                         ""
                       else
                         head tracenames

       runGUI filename traceName debug

-------------------------------------------------------------------------------

data Option
  = Debug
  | Filename String
  | TestTrace String
    deriving Eq

-------------------------------------------------------------------------------

parseOptions :: [String] -> [Option]
parseOptions [] = []
parseOptions ("--debug":rest)
  = Debug : parseOptions rest
parseOptions ("--test":rest)
  = if rest == [] then
      error ("--test needs an argument")
    else
      TestTrace (head rest) : parseOptions (tail rest)
parseOptions (filename:rest)
  = Filename filename : parseOptions rest

-----------------------------------------------------------------------------
