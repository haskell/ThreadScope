module Events.SparkStats (
  SparkStats(rateCreated, rateDud, rateOverflowed,
             rateConverted, rateFizzled, rateGCd,
             meanPool, maxPool, minPool),
  initial, create, aggregate, agEx, rescale,
  ) where

import Data.Word (Word64)

data SparkStats =
  SparkStats { rateCreated, rateDud, rateOverflowed,
               rateConverted, rateFizzled, rateGCd,
               meanPool, maxPool, minPool :: {-# UNPACK #-}!Double }
  deriving (Show, Eq)

-- | Initial, default value of spark stats, at the start of runtime,
-- before any spark activity is recorded.
initial :: SparkStats
initial = SparkStats 0 0 0 0 0 0 0 0 0

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

foldStats :: (Double -> Double -> Double) -> Double -> Double -> Double
        -> [SparkStats] -> SparkStats
foldStats f meanP maxP minP l
  = SparkStats
      (foldr f 0 (map rateCreated l))
      (foldr f 0 (map rateDud l))
      (foldr f 0 (map rateOverflowed l))
      (foldr f 0 (map rateConverted l))
      (foldr f 0 (map rateFizzled l))
      (foldr f 0 (map rateGCd l))
      meanP maxP minP

aggregate :: [SparkStats] -> SparkStats
aggregate [s] = s  -- optimization
aggregate l =
  let meanP = sum (map meanPool l) / fromIntegral (length l) -- TODO: inaccurate
      maxP  = maximum (map maxPool l)
      minP  = minimum (map minPool l)
  in foldStats (+) meanP maxP minP l

-- Scale has to be positive.
rescale :: Double -> SparkStats -> SparkStats
rescale scale s =
  let f w _ = scale * w
  in foldStats f (meanPool s) (maxPool s) (minPool s) [s]

-- Rates of change extrapolate by dropping to 0, absolute pools size values
-- by staying constant.
extrapolate :: SparkStats -> SparkStats
extrapolate s =
  let f w _ = 0 * w
  in foldStats f (meanPool s) (maxPool s) (minPool s) [s]

agEx :: [SparkStats] -> SparkStats -> (SparkStats, SparkStats)
agEx [] s = (extrapolate s, s)
agEx l@(s:_) _ = (aggregate l, s)
