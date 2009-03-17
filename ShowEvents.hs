{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events
import System.Environment
import Text.Printf
import Data.List
import Data.Function
import Data.Maybe

main = do
  [file] <- getArgs

  eventLog <- readEventLogFromFile file

  let ets = eventTypes $ header eventLog
      es = events $ dat eventLog
      sorted_es = sortBy (compare `on` time) es

  printf "Event Types:\n"
  putStrLn (unlines (map ppEventType ets))

  printf "Events:\n"
  putStrLn $ unlines $ map ppEvent $ sorted_es

ppEventType :: EventType -> String
ppEventType et = 
  printf "%4d: %s (size %d)" (num et) (desc et) (varSize et)
  where
    varSize et = case (size et) of 
                   Just sz -> sz
                   Nothing -> 0

ppEvent :: Event -> String
ppEvent Event{..} =
  printf "%9d: cap %d: " time (cap spec) ++
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

{- EOF. -}
