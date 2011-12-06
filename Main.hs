module Main where

import GUI.Main (runGUI)

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Version (showVersion)
import Paths_threadscope (version)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    (flags, args') <- parseArgs args
    handleArgs flags args'

handleArgs :: Flags -> [String] -> IO ()
handleArgs flags args
  | flagHelp    flags = printHelp
  | flagVersion flags = printVersion
  | otherwise         = do

    initialTrace <- case (args, flagTest flags) of
      ([filename], Nothing) -> return (Just (Left filename))
      ([], Just tracename)  -> return (Just (Right tracename))
      ([], Nothing)         -> return Nothing
      _                     -> printUsage >> exitFailure

    runGUI initialTrace

  where
    printVersion = putStrLn ("ThreadScope version " ++ showVersion version)
    printUsage   = putStrLn usageHeader
    usageHeader  = "Usage: threadscope [eventlog]\n" ++
                   "   or: threadscope [FLAGS]"
    helpHeader   = usageHeader ++ "\n\nFlags: "
    printHelp    = putStrLn (usageInfo helpHeader flagDescrs
                             ++ "\nFor more details see http://www.haskell.org/haskellwiki/ThreadScope_Tour\n")


-------------------------------------------------------------------------------

data Flags = Flags {
     flagTest    :: Maybe FilePath,
     flagVersion :: Bool,
     flagHelp    :: Bool
  }

defaultFlags :: Flags
defaultFlags = Flags Nothing False False

flagDescrs :: [OptDescr (Flags -> Flags)]
flagDescrs =
  [ Option ['h'] ["help"]
      (NoArg (\flags -> flags { flagHelp = True }))
      "Show this help text"

  , Option ['v'] ["version"]
      (NoArg (\flags -> flags { flagVersion = True }))
      "Program version"

  , Option ['t'] ["test"]
      (ReqArg (\name flags -> flags { flagTest = Just name }) "NAME")
      "Load a named internal test (see Events/TestEvents.hs)"
  ]

parseArgs :: [String] -> IO (Flags, [String])
parseArgs args
  | flagHelp flags  = return (flags, args')
  | not (null errs) = printErrors errs
  | otherwise       = return (flags, args')

  where
    (flags0, args', errs) = getOpt Permute flagDescrs args
    flags = foldr (flip (.)) id flags0 defaultFlags

    printErrors errs = do
      putStrLn $ concat errs ++ "Try --help."
      exitFailure
