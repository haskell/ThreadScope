{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events as Log
import System.Environment
import Text.Printf
import Data.List
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe
import System.IO
import System.Exit

main = do
  [file] <- getArgs

  log <- do e <- readEventLogFromFile file
            case e of
               Left  s   -> die ("Failed to parse " ++ file ++ ": " ++ s)
               Right log -> return log
    
  printf "Event Types:\n"
  let etypes = Log.eventTypes (Log.header log)
  putStrLn (unlines (map ppEventType etypes))

  let imap = M.fromList [ (fromIntegral (Log.num t),t) | t <- etypes ]

  let pes = Log.events (Log.dat log)
      sorted = sortEvents pes
              -- the events come out reversed, and we want a stable sort

  printf "Events:\n"
  putStrLn $ unlines $ map (ppEvent imap) $ sorted
--  putStrLn $ show $ length $ sorted

unsortEvents :: [Event] -> [(Int,Event)]
unsortEvents es = ess
 where 
   blocks = [ b | b@EventBlock{} <- map spec es ]
   ess    = [ (cap block, e) | block <- blocks, e <- block_events block ]

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

ppEventType :: EventType -> String
ppEventType et = printf "%4d: %s (size %s)" (Log.num et) (Log.desc et) 
   (case Log.size et of Nothing -> "variable"; Just x -> show x)

ppEvent :: IntMap EventType -> CapEvent -> String
ppEvent imap (CapEvent cap (Event ref time spec)) =
  printf "%9d: " time ++
  (case cap of
    Nothing -> ""
    Just c  -> printf "cap %d: " c) ++
  case spec of
    UnknownEvent ->
      printf (Log.desc (fromJust (M.lookup (fromIntegral ref) imap)))

    Message msg -> msg

    other -> showEventTypeSpecificInfo spec
