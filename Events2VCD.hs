{-# LANGUAGE RecordWildCards #-}
--- $Id: Events2VCD.hs#1 2009/03/20 13:27:50 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/Events2VCD.hs $

module Main
where
import GHC.RTS.Events
import System.Environment
import Text.Printf
import Data.List
import Data.Function


main = do
  [file] <- getArgs

  fmt <- buildFormat file

  let pes = events (fmtData fmt)
      sorted = sortBy (compare `on` ts) pes
  putStrLn "$timescale 1ns $end" -- Time units are represented as microseconds
  showCapabilities (ennumerateCapabilities pes)
  putStrLn $ unlines $ map ppEvent $ sorted

ennumerateCapabilities :: [Event] -> [Int]
ennumerateCapabilities events
  = sort (nub (map (cap.spec) events))

showCapabilities :: [Int] -> IO ()
showCapabilities caps
  = do putStrLn (unlines (map showCap caps))
       putStrLn "$enddefinitions $end"

showCap :: Int -> String
showCap n = "$var integer 32 <"++ show n ++ " cap" ++ show n ++ " $end"


ppEventType :: EventType -> String
ppEventType et = printf "%4d: %s (size %d)" (etNum et) (etDesc et) (etSize et)

ppEvent :: Event -> String
ppEvent Event{..} =
  case spec of
    RunThread{..}      -> unlines ["#" ++ show ts, showInt thread ++ " <" ++ show cap]
    StopThread{..}     -> unlines ["#" ++ show ts, "bZ <" ++ show cap]
    _ -> ""
   
showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"

showInt v = 'b' : reverse (showInt' v)

showInt' 0 = "0"
showInt' v
  = (if v `mod` 2 == 0 then '0' else '1') : showInt' (v `div` 2) 
