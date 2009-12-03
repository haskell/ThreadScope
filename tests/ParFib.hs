-------------------------------------------------------------------------------
-- A parallel implementation of fib in Haskell using semi-explicit
-- parallelism expressed with `par` and `pseq`

module Main
where
import System.Time
import Control.Parallel
import System.Mem

-------------------------------------------------------------------------------
-- A purely sequential implementaiton of fib.

seqFib :: Int -> Integer
seqFib 0 = 1
seqFib 1 = 1
seqFib n = seqFib (n-1) + seqFib (n-2)

-------------------------------------------------------------------------------
-- A thresh-hold value below which the parallel implementation of fib
-- reverts to sequential implementation.

threshHold :: Int
threshHold = 25

-------------------------------------------------------------------------------
-- A parallel implementation of fib.

parFib :: Int -> Integer
parFib n
  = if n < threshHold then
      seqFib n
    else
      r `par` (l `pseq` l + r)
    where
    l  = parFib (n-1)
    r  = parFib (n-2)

-------------------------------------------------------------------------------

result :: Integer
result = parFib 46

-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

main :: IO ()
main
  = do putStrLn "ParFib"
       t0 <- getClockTime
       pseq result (return ())
       t1 <- getClockTime
       putStrLn ("fib = " ++ show result)
       putStrLn ("Time: " ++ show (secDiff t0 t1))

-------------------------------------------------------------------------------
