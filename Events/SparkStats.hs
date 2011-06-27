module Events.SparkStats (
  SparkStats(..),
  zero, aggregate, aggrMaybe, create, rescale,
  ) where

data SparkStats =
  SparkStats { rateCreated, rateDud, rateOverflowed,
               rateConverted, rateFizzled, rateGCd,
               meanPool, maxPool, minPool :: {-# UNPACK #-}! Double }
  deriving (Show, Eq)

zero :: SparkStats
zero = SparkStats 0 0 0 0 0 0 0 0 0

map2 :: (Double -> Double -> Double) ->
        SparkStats -> SparkStats -> SparkStats
map2 f
  (SparkStats rateCreated1 rateDud1 rateOverflowed1
              rateConverted1 rateFizzled1 rateGCd1
              meanPool1 maxPool1 minPool1)
  (SparkStats rateCreated2 rateDud2 rateOverflowed2
              rateConverted2 rateFizzled2 rateGCd2
              _ _ _)
  = SparkStats
      (f rateCreated1 rateCreated2)
      (f rateDud1 rateDud2)
      (f rateOverflowed1 rateOverflowed2)
      (f rateConverted1 rateConverted2)
      (f rateFizzled1 rateFizzled2)
      (f rateGCd1 rateGCd2)
      meanPool1 maxPool1 minPool1  -- TODO: semi-hack

aggregate :: SparkStats -> SparkStats -> SparkStats
aggregate s1 s2 =
  let m = map2 (+) s1 s2
  in m {meanPool = (meanPool s1 + meanPool s2) / 2,  -- TODO: not accurate
        maxPool  = max (maxPool s1) (maxPool s2),
        minPool  = min (minPool s1) (minPool s2)}

aggrMaybe :: Maybe SparkStats -> SparkStats -> SparkStats
aggrMaybe Nothing s2 = s2
aggrMaybe (Just s1) s2 = aggregate s1 s2

-- The values in the second counter have to be greater or equal
-- to the values in the first counter.
create :: SparkStats -> SparkStats -> SparkStats
create = map2 (-)

-- Scale has to be positive.
rescale :: Double -> SparkStats -> SparkStats
rescale scale s =
  let f w _ = scale * w
  in map2 f s zero
