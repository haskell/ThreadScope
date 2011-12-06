module Events.SparkStats
  ( SparkStats(..)
  , initial, create, rescale, aggregate, agEx
  ) where

import Data.Word (Word64)

-- | Sparks change state. Each state transition process has a duration.
-- Spark statistics, for a given duration, record the spark transition rate
-- (the number of sparks that enter a given state within the interval)
-- and the absolute mean, maximal and minimal number of sparks
-- in the spark pool within the duration.
data SparkStats =
  SparkStats { rateCreated, rateDud, rateOverflowed,
               rateConverted, rateFizzled, rateGCd,
               meanPool, maxPool, minPool :: {-# UNPACK #-}!Double }
  deriving (Show, Eq)

-- | Initial, default value of spark stats, at the start of runtime,
-- before any spark activity is recorded.
initial :: SparkStats
initial = SparkStats 0 0 0 0 0 0 0 0 0

-- | Create spark stats for a duration, given absolute
-- numbers of sparks in all categories at the start and end of the duration.
-- The units for spark transitions (first 6 counters) is [spark/duration]:
-- the fact that intervals may have different lenghts is ignored here.
-- The units for the pool stats are just [spark].
-- The values in the second counter have to be greater or equal
-- to the values in the first counter, except for the spark pool size.
-- For pool size, we take into account only the first sample,
-- to visualize more detail at high zoom levels, at the cost
-- of a slight shift of the graph. Mathematically, this corresponds
-- to taking the initial durations as centered around samples,
-- but to have the same tree for rates and pool sizes, we then have
-- to shift the durations by half interval size to the right
-- (which would be neglectable if the interval was small and even).
create :: (Word64, Word64, Word64, Word64, Word64, Word64, Word64)
       -> (Word64, Word64, Word64, Word64, Word64, Word64, Word64)
       -> SparkStats
create (crt1, dud1, ovf1, cnv1, fiz1, gcd1, remaining1)
       (crt2, dud2, ovf2, cnv2, fiz2, gcd2, _remaining2) =
  let (crt, dud, ovf, cnv, fiz, gcd) =
        (fromIntegral $ crt2 - crt1,
         fromIntegral $ dud2 - dud1,
         fromIntegral $ ovf2 - ovf1,
         fromIntegral $ cnv2 - cnv1,
         fromIntegral $ fiz2 - fiz1,
         fromIntegral $ gcd2 - gcd1)
      p = fromIntegral remaining1
  in SparkStats crt dud ovf cnv fiz gcd p p p

-- | Reduce a list of spark stats; spark pool stats are overwritten.
foldStats :: (Double -> Double -> Double)
          -> Double -> Double -> Double
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

-- | Rescale the spark transition stats, e.g., to change their units.
rescale :: Double -> SparkStats -> SparkStats
rescale scale s =
  let f w _ = scale * w
  in foldStats f (meanPool s) (maxPool s) (minPool s) [s]

-- | Derive spark stats for an interval from a list of spark stats,
-- in reverse chronological order, of consecutive subintervals
-- that sum up to the original interval.
aggregate :: [SparkStats] -> SparkStats
aggregate [] = error "aggregate"
aggregate [s] = s  -- optimization
aggregate l =
  let meanP = sum (map meanPool l) / fromIntegral (length l) -- TODO: inaccurate
      maxP  = maximum (map maxPool l)
      minP  = minimum (map minPool l)
  in foldStats (+) meanP maxP minP l

-- | Extrapolate spark stats from previous data.
-- Absolute pools size values extrapolate by staying constant,
-- rates of change of spark status extrapolate by dropping to 0
-- (which corresponds to absolute numbers of sparks staying constant).
extrapolate :: SparkStats -> SparkStats
extrapolate s =
  let f w _ = 0 * w
  in foldStats f (meanPool s) (maxPool s) (minPool s) [s]

-- | Aggregate, if any data provided. Extrapolate from previous data, otherwise.
-- In both cases, the second component is the new choice of "previous data".
-- The list of stats is expected in reverse chronological order,
-- as for aggregate.
agEx :: [SparkStats] -> SparkStats -> (SparkStats, SparkStats)
agEx [] s = (extrapolate s, s)
agEx l@(s:_) _ = (aggregate l, s)
