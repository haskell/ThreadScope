-------------------------------------------------------------------------------
--- $Id: SumEulerPar1.hs#1 2008/05/06 16:25:08 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where
import System.Time
import System.Random
import Control.Parallel
import System.Mem
import Control.Parallel.Strategies

-------------------------------------------------------------------------------

mkList :: Int -> [Int]
mkList n = [1..n-1]

-------------------------------------------------------------------------------

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

-------------------------------------------------------------------------------

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

-------------------------------------------------------------------------------

sumEulerPar1 n = sum ((map euler (mkList n)) `using` parList rnf)

-------------------------------------------------------------------------------

input :: Int
input = 1000

-------------------------------------------------------------------------------

result :: Int
result = sumEulerPar1 input

-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn ("SumEulerPar1 parList input = " ++ show input)
       performGC
       t0 <- getClockTime
       pseq result (return ())
       t1 <- getClockTime
       putStrLn ("sumeuler = " ++ show result)
       putStrLn ("Time: " ++ show (secDiff t0 t1))

-------------------------------------------------------------------------------
