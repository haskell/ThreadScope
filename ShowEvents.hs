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
      sorted = sortBy (compare `on` Log.time) (reverse pes)
              -- the events come out reversed, and we want a stable sort

  printf "Events:\n"
  putStrLn $ unlines $ map (ppEvent imap) $ sorted


die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

--   putStrLn (show $ getFirstPE dat)
--   let len = length $ phaseEvents dat
--   putStrLn (show $ phaseEvents dat !! (len - 3))
--   putStrLn (show $ phaseEvents dat !! (len - 2))
--   putStrLn (show $ phaseEvents dat !! (len - 1))
-- 
-- getFirstPE dat = head $ phaseEvents dat

ppEventType :: EventType -> String
ppEventType et = printf "%4d: %s (size %s)" (Log.num et) (Log.desc et) 
   (case Log.size et of Nothing -> "variable"; Just x -> show x)

ppEvent :: IntMap EventType -> Event -> String
ppEvent imap Event{..} =
  printf "%9d: " time ++
  case spec of
    UnknownEvent ->
      printf (Log.desc (fromJust (M.lookup (fromIntegral ref) imap)))

    other ->
      printf "cap %d: " (cap spec) ++
      case spec of
        CreateThread{..}   -> printf "creating thread %d" thread
        RunThread{..}      -> printf "running thread %d" thread
        StopThread{..}     -> printf "stopping thread %d (%s)" thread (showThreadStopStatus status)
        ThreadRunnable{..} -> printf "thread %d is runnable" thread
        MigrateThread{..}  -> printf "migrating thread %d to cap %d" thread newCap
        RunSpark{..}       -> printf "running a local spark (thread %d)" thread
        StealSpark{..}     -> printf "thread %d stealing a spark from cap %d" thread origCap
        Shutdown{..}       -> printf "shutting down"
        WakeupThread{..}   -> printf "waking up thread %d on cap %d" thread otherCap
        RequestSeqGC{..}   -> printf "requesting sequential GC"
        RequestParGC{..}   -> printf "requesting parallel GC"
        StartGC{..}        -> printf "starting GC"
        EndGC{..}          -> printf "finished GC"

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
