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

ppEventType :: EventType -> String
ppEventType et = printf "%4d: %s (size %s)" (Log.num et) (Log.desc et) 
   (case Log.size et of Nothing -> "variable"; Just x -> show x)

ppEvent :: IntMap EventType -> Event -> String
ppEvent imap (Event ref time spec) =
  printf "%9d: " time ++
  case spec of
    UnknownEvent ->
      printf (Log.desc (fromJust (M.lookup (fromIntegral ref) imap)))

    other ->
      printf "cap %d: " (cap spec) ++
      case spec of
        CreateThread cap thread          -> 
          printf "creating thread %d" thread
        RunThread cap thread             -> 
          printf "running thread %d" thread
        StopThread cap thread status     -> 
          printf "stopping thread %d (%s)" thread (showThreadStopStatus status)
        ThreadRunnable cap thread        -> 
          printf "thread %d is runnable" thread
        MigrateThread cap thread newCap  -> 
          printf "migrating thread %d to cap %d" thread newCap
        CreateSpark cap thread              -> 
          printf "create spark (thread %d)" thread
        RunSpark cap thread              -> 
          printf "running a local spark (thread %d)" thread
        StealSpark cap thread origCap    -> 
          printf "thread %d stealing a spark from cap %d" thread origCap
        SparkToThread cap thread spark_thread -> 
          printf "thread %d from cap %d turning spark into thread %d" thread cap spark_thread
        Shutdown cap                     -> 
          printf "shutting down"
        WakeupThread cap thread otherCap -> 
          printf "waking up thread %d on cap %d" thread otherCap
        RequestSeqGC cap                 -> 
          printf "requesting sequential GC"
        RequestParGC cap   -> 
          printf "requesting parallel GC"
        StartGC cap        -> 
          printf "starting GC"
        EndGC cap          -> 
          printf "finished GC"

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
