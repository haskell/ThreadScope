module Events.SparkStats (
  SparkStats(..),
  zero, aggregate, create, rescale,
  ) where

data SparkStats =
  SparkStats { rateCreated, rateDud, rateOverflowed,
               rateConverted, rateFizzled, rateGCd,
               sparksRemaining :: {-# UNPACK #-}! Double }
  deriving (Show, Eq)

zero :: SparkStats
zero = SparkStats 0 0 0 0 0 0 0

map2 :: (Double -> Double -> Double) ->
        SparkStats -> SparkStats -> SparkStats
map2 f
  (SparkStats rateCreated1 rateDud1 rateOverflowed1
              rateConverted1 rateFizzled1 rateGCd1
              sparksRemaining1)
  (SparkStats rateCreated2 rateDud2 rateOverflowed2
              rateConverted2 rateFizzled2 rateGCd2
              sparksRemaining2)
  = SparkStats
      (f rateCreated1 rateCreated2)
      (f rateDud1 rateDud2)
      (f rateOverflowed1 rateOverflowed2)
      (f rateConverted1 rateConverted2)
      (f rateFizzled1 rateFizzled2)
      (f rateGCd1 rateGCd2)
      (f sparksRemaining1 sparksRemaining2)

aggregate :: SparkStats -> SparkStats -> SparkStats
aggregate = map2 (+)

-- The values in the second counter have to be greater or equal
-- to the values int he first counter.
create :: SparkStats -> SparkStats -> SparkStats
create = map2 (-)

-- Scale has to be positive.
rescale :: Double -> SparkStats -> SparkStats
rescale scale c =
  let f w _ = w * scale
  in map2 f c zero
