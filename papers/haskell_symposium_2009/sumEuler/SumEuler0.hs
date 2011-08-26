-------------------------------------------------------------------------------
-- This program runs fib and sumEuler separately and sequentially
-- to allow us to compute how long each individual function takes
-- to execute.

module Main
where
import System.Time
import Control.Parallel
import System.Mem

-------------------------------------------------------------------------------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

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

sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

-------------------------------------------------------------------------------

sumFibEuler :: Int -> Int -> Int
sumFibEuler a b = fib a + sumEuler b

-------------------------------------------------------------------------------

result1 :: Int
result1 = fib 38 

result2 :: Int
result2 = sumEuler 5300

-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn "SumEuler0 (sequential)"
       performGC
       t0 <- getClockTime
       pseq result1 (return ())
       t1 <- getClockTime
       putStrLn ("fib time: " ++ show (secDiff t0 t1))
       t2 <- getClockTime
       pseq result2 (return ())
       t3 <- getClockTime
       putStrLn ("sumEuler time: " ++ show (secDiff t2 t3))

-------------------------------------------------------------------------------
