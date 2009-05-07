-------------------------------------------------------------------------------
-- This demonstrates that f `par` (f + e) does not result in parallelsim.

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

parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b
  = f `par` (f + e)
    where
    f = fib a
    e = sumEuler b

-------------------------------------------------------------------------------

result :: Int
result = parSumFibEuler 38 5300

-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn "SumEuler1"
       performGC
       t0 <- getClockTime
       pseq result (return ())
       t1 <- getClockTime
       putStrLn ("sumeuler1 = " ++ show result)
       putStrLn ("Time: " ++ show (secDiff t0 t1))

-------------------------------------------------------------------------------
