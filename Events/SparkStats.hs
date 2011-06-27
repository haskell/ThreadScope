module Events.SparkStats (
  SparkStats(..),
  zero, create, aggrMaybe, extrMaybe,rescale,
  ) where

import Data.Word (Word64)

data SparkStats =
  SparkStats { rateCreated, rateDud, rateOverflowed,
               rateConverted, rateFizzled, rateGCd,
               meanPool, maxPool, minPool :: {-# UNPACK #-}! Double }
  deriving (Show, Eq)

zero :: SparkStats
zero = SparkStats 0 0 0 0 0 0 0 0 0

-- The values in the second counter have to be greater or equal
-- to the values in the first counter, except for the spark pool size.
create :: (Word64, Word64, Word64, Word64, Word64, Word64, Word64) ->
          (Word64, Word64, Word64, Word64, Word64, Word64, Word64) ->
          SparkStats
create (crt1, dud1, ovf1, cnv1, fiz1, gcd1, rem1)
       (crt2, dud2, ovf2, cnv2, fiz2, gcd2, _rem2) =
  let (crt, dud, ovf, cnv, fiz, gcd) =
        (fromIntegral $ crt2 - crt1,
         fromIntegral $ dud2 - dud1,
         fromIntegral $ ovf2 - ovf1,
         fromIntegral $ cnv2 - cnv1,
         fromIntegral $ fiz2 - fiz1,
         fromIntegral $ gcd2 - gcd1)
      pool1 = fromIntegral rem1
  in SparkStats crt dud ovf cnv fiz gcd pool1 pool1 pool1

map2 :: (Double -> Double -> Double) -> Double -> Double -> Double
        -> SparkStats -> SparkStats -> SparkStats
map2 f meanP maxP minP
  (SparkStats rateCreated1 rateDud1 rateOverflowed1
              rateConverted1 rateFizzled1 rateGCd1
              _ _ _)
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
      meanP maxP minP

aggregate :: SparkStats -> SparkStats -> SparkStats
aggregate s1 s2 =
  let meanP = (meanPool s1 + meanPool s2) / 2  -- TODO: not accurate
      maxP  = max (maxPool s1) (maxPool s2)
      minP  = min (minPool s1) (minPool s2)
  in map2 (+) meanP maxP minP s1 s2

aggrMaybe :: Maybe SparkStats -> SparkStats -> SparkStats
aggrMaybe Nothing s2 = s2
aggrMaybe (Just s1) s2 = aggregate s1 s2

-- Scale has to be positive.
rescale :: Double -> SparkStats -> SparkStats
rescale scale s =
  let f w _ = scale * w
  in map2 f (meanPool s) (maxPool s) (minPool s) s zero

extrapolate :: SparkStats -> SparkStats
extrapolate s =
  let f w _ = 0 * w
  in map2 f (meanPool s) (maxPool s) (minPool s) s zero

extrMaybe :: Maybe SparkStats -> SparkStats
extrMaybe Nothing  = zero
extrMaybe (Just s) = extrapolate s
