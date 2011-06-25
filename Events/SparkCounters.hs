module Events.SparkCounters (
     SparkCounters(..),
     zero, add, sub, rescale,
  ) where

data SparkCounters =
  SparkCounters { sparksCreated, sparksDud, sparksOverflowed,
                  sparksConverted, sparksFizzled, sparksGCd,
                  sparksRemaining :: {-# UNPACK #-}! Double }
  deriving (Show, Eq)

zero :: SparkCounters
zero = SparkCounters 0 0 0 0 0 0 0

map2 :: (Double -> Double -> Double) ->
       SparkCounters -> SparkCounters -> SparkCounters
map2 f
  (SparkCounters sparksCreated1 sparksDud1 sparksOverflowed1
                 sparksConverted1 sparksFizzled1 sparksGCd1
                 sparksRemaining1)
  (SparkCounters sparksCreated2 sparksDud2 sparksOverflowed2
                 sparksConverted2 sparksFizzled2 sparksGCd2
                 sparksRemaining2)
  = SparkCounters
      (f sparksCreated1 sparksCreated2)
      (f sparksDud1 sparksDud2)
      (f sparksOverflowed1 sparksOverflowed2)
      (f sparksConverted1 sparksConverted2)
      (f sparksFizzled1 sparksFizzled2)
      (f sparksGCd1 sparksGCd2)
      (f sparksRemaining1 sparksRemaining2)

add :: SparkCounters -> SparkCounters -> SparkCounters
add = map2 (+)

-- The values in the second counter have to be greater or equal
-- to the values int he first counter.
sub :: SparkCounters -> SparkCounters -> SparkCounters
sub = map2 (-)

-- Scale has to be positive.
rescale :: Double -> SparkCounters -> SparkCounters
rescale scale c =
  let f w _ = w * scale
  in map2 f c zero
