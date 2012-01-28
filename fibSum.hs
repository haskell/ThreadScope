import System.Time
import Control.Parallel

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mkList :: Int -> [Int]
mkList n = [1..n-1]

relprime :: Int -> Int -> Bool
relprime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relprime n) (mkList n))

sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b = f `par` (e `pseq`(e + f))
                     where f = sumEuler b
                           e = fib a

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

r1 :: Int
r1 = parSumFibEuler 40 5300

main :: IO ()
main = do

    t4 <- getClockTime
    pseq r1 (return ())
    t5 <- getClockTime
    putStrLn ("sum: " ++ show r1)
    putStrLn ("time: " ++ show (secDiff t4 t5) ++ " seconds")
