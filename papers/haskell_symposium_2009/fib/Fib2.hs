-------------------------------------------------------------------------------

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

par2Fib:: Int -> Int -> Int
par2Fib a b
  = f `par` (e `pseq` f + e)
    where
    f = fib a
    e = fib b

-------------------------------------------------------------------------------

result :: Int
result = par2Fib 36 36

-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn "Fib2"
       performGC
       t0 <- getClockTime
       pseq result (return ())
       t1 <- getClockTime
       putStrLn ("fib2 = " ++ show result)
       putStrLn ("Time: " ++ show (secDiff t0 t1))

-------------------------------------------------------------------------------
