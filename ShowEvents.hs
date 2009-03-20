{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.RTS.Events
import System.Environment
import Text.Printf
import Data.List
import Data.Function

main = do
  [file] <- getArgs

  fmt <- buildFormat file

  printf "Event Types:\n"
  case fmtHeader fmt of
    Header (EventTypes ets) -> putStrLn (unlines (map ppEventType ets))

  let pes = events (fmtData fmt)
      sorted = sortBy (compare `on` ts) (reverse pes)
              -- the events come out reversed, and we want a stable sort

  printf "Events:\n"
  putStrLn $ unlines $ map ppEvent $ sorted

--   putStrLn (show $ getFirstPE dat)
--   let len = length $ phaseEvents dat
--   putStrLn (show $ phaseEvents dat !! (len - 3))
--   putStrLn (show $ phaseEvents dat !! (len - 2))
--   putStrLn (show $ phaseEvents dat !! (len - 1))
-- 
-- getFirstPE dat = head $ phaseEvents dat

{- EOF. -}

ppEventType :: EventType -> String
ppEventType et = printf "%4d: %s (size %d)" (etNum et) (etDesc et) (etSize et)

ppEvent :: Event -> String
ppEvent Event{..} =
  printf "%9d: cap %d: " ts (cap spec) ++
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
