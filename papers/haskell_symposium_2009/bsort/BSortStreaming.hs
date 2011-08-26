-------------------------------------------------------------------------------
--- $Id: BSortStreaming.hs#1 2009/03/06 10:53:15 REDMOND\\satnams $
-------------------------------------------------------------------------------

module Main
where

import Data.List
import System.Mem
import System.Random
import System.Time

-------------------------------------------------------------------------------

infixr 5 >->

-------------------------------------------------------------------------------

(>->) :: (a-> b) -> (b-> c) -> (a-> c)
(>->) circuit1 circuit2 input1 
  = circuit2 (circuit1 input1)

-------------------------------------------------------------------------------

halve :: [a] -> ([a], [a])
halve l
  = (take n l, drop n l)
    where
    n = length l `div` 2

-------------------------------------------------------------------------------

unhalve :: ([a], [a]) -> [a]
unhalve (a, b) = a ++ b

-------------------------------------------------------------------------------

pair :: [a] -> [[a]]
pair [] = []
pair lst | odd (length lst) 
  = error ("pair given odd length list of size " ++ show (length lst))
pair (a:b:cs) 
  = [a,b]:rest
    where
    rest = pair cs

-------------------------------------------------------------------------------

unpair :: [[a]] -> [a]
unpair list = concat list

-------------------------------------------------------------------------------

par2 :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
par2 circuit1 circuit2 (input1, input2)
  = (output1, output2)
    where
    output1 = circuit1 input1
    output2 = circuit2 input2

-------------------------------------------------------------------------------

halveList :: [a] -> [[a]]
halveList l
  = [take n l, drop n l]
    where
    n = length l `div` 2
 
-------------------------------------------------------------------------------

unhalveList :: [[a]] -> [a]
unhalveList [a, b] = a ++ b

-------------------------------------------------------------------------------

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n l 
  = (take n l) : chop n (drop n l)

-------------------------------------------------------------------------------

zipList :: [[a]] -> [[a]]
zipList [[], _] = []
zipList [_, []] = []
zipList [a:as, b:bs] 
  = [a,b] : zipList [as, bs]

-------------------------------------------------------------------------------

unzipList :: [[a]] -> [[a]]
unzipList list = [map fstListPair list, map sndListPair list]

-------------------------------------------------------------------------------

fsT :: (a -> b) -> (a, c) -> (b, c)
fsT f (a, b) 
  = (f a, b)

-------------------------------------------------------------------------------

snD :: (b -> c) -> (a, b) -> (a, c)
snD f (a, b) 
  = (a, f b)

-------------------------------------------------------------------------------

sndList :: ([a] -> [a]) -> [a] -> [a]
sndList f = halve >-> snD f >-> unhalve

-------------------------------------------------------------------------------

fstListPair :: [a] -> a
fstListPair [a, _] = a 

-------------------------------------------------------------------------------

sndListPair :: [a] -> a
sndListPair [_, b] = b 

-------------------------------------------------------------------------------

two :: ([a] -> [b]) -> [a] -> [b]
two r = halve >-> par2 r r >-> unhalve
 
-------------------------------------------------------------------------------
-- Many twos.

twoN :: Int -> ([a] -> [b]) -> [a] -> [b]
twoN 0 r = r
twoN n r = two (twoN (n-1) r)

-------------------------------------------------------------------------------

riffle :: [a] -> [a]
riffle = halveList >-> zipList >-> unpair

-------------------------------------------------------------------------------

unriffle :: [a] -> [a]
unriffle = pair >-> unzipList >-> unhalveList

-------------------------------------------------------------------------------
 
ilv :: ([a] -> [b]) -> [a] -> [b]
ilv r = unriffle >-> two r >-> riffle

-------------------------------------------------------------------------------

ilvN :: Int -> ([a] -> [b]) -> [a] -> [b]
ilvN 0 r = r
ilvN n r = ilv (ilvN (n-1) r)

-------------------------------------------------------------------------------

evens :: ([a] -> [b]) -> [a] -> [b]
evens f = chop 2 >-> map f >-> concat

-------------------------------------------------------------------------------

type ButterflyElement a = [a] -> [a]
type Butterfly a = [a] -> [a]

-------------------------------------------------------------------------------

butterfly :: ButterflyElement a -> Butterfly a 
butterfly circuit [x,y] = circuit [x,y]
butterfly circuit input
  = (ilv (butterfly circuit) >-> evens circuit) input

-------------------------------------------------------------------------------

sortB cmp [x, y] = cmp [x, y]
sortB cmp input
  = (two (sortB cmp) >-> sndList reverse >-> butterfly cmp) input

-------------------------------------------------------------------------------

twoSorter :: [Int] -> [Int]
twoSorter [a, b] 
  = if a <= b then
      [a, b]
    else
      [b, a]

-------------------------------------------------------------------------------

streamingTwoSorter :: [[Int]] -> [[Int]]
streamingTwoSorter [as, bs]
  = transpose [twoSorter [a, b] | (a, b) <- zip as bs]

-------------------------------------------------------------------------------

bsort :: [[Int]] -> [[Int]]
bsort = sortB streamingTwoSorter

-------------------------------------------------------------------------------

produceRandomNumbers :: Int -> IO [Int]
produceRandomNumbers n
  = sequence (replicate n (getStdRandom (randomR (1,25))))

-------------------------------------------------------------------------------

main :: IO ()
main 
  = do putStrLn "Streaming bsort"
       -- The argument to replicate specifies the input of inputs
       -- to the sorter e.g. 2^3 means this is an 8 input sorter.
       -- The argument to produceRandomNumbers specified how many
       -- numbers flow along each input stream.
       nums <- sequence (replicate (2^5) (produceRandomNumbers 10000))
       --putStrLn (show nums)
       performGC
       tStart <- getClockTime
       let r = concat (bsort nums)
       seq r (return ())
       tEnd <- getClockTime
       --putStrLn (show r)
       putStrLn (show (sum r))
       putStrLn ("Time: " ++ show (secDiff tStart tEnd) ++ " seconds.")
 
-------------------------------------------------------------------------------

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

-------------------------------------------------------------------------------

