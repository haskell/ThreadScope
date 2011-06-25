module Events.SparkStats (
  SparkStats(..),
  zero, add, sub, rescale,
  ) where

data SparkStats =
  SparkStats { sparksCreated, sparksDud, sparksOverflowed,
               sparksConverted, sparksFizzled, sparksGCd,
               sparksRemaining :: {-# UNPACK #-}! Double }
  deriving (Show, Eq)

zero :: SparkStats
zero = SparkStats 0 0 0 0 0 0 0

map2 :: (Double -> Double -> Double) ->
        SparkStats -> SparkStats -> SparkStats
map2 f
  (SparkStats sparksCreated1 sparksDud1 sparksOverflowed1
              sparksConverted1 sparksFizzled1 sparksGCd1
              sparksRemaining1)
  (SparkStats sparksCreated2 sparksDud2 sparksOverflowed2
              sparksConverted2 sparksFizzled2 sparksGCd2
              sparksRemaining2)
  = SparkStats
      (f sparksCreated1 sparksCreated2)
      (f sparksDud1 sparksDud2)
      (f sparksOverflowed1 sparksOverflowed2)
      (f sparksConverted1 sparksConverted2)
      (f sparksFizzled1 sparksFizzled2)
      (f sparksGCd1 sparksGCd2)
      (f sparksRemaining1 sparksRemaining2)

add :: SparkStats -> SparkStats -> SparkStats
add = map2 (+)

-- The values in the second counter have to be greater or equal
-- to the values int he first counter.
sub :: SparkStats -> SparkStats -> SparkStats
sub = map2 (-)

-- Scale has to be positive.
rescale :: Double -> SparkStats -> SparkStats
rescale scale c =
  let f w _ = w * scale
  in map2 f c zero
