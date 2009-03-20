-------------------------------------------------------------------------------
--- $Id: EventlogViewerCommon.hs#5 2009/03/10 14:37:37 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ghc-profiling/Events/EventlogViewerCommon.hs $
-------------------------------------------------------------------------------

module EventlogViewerCommon
where
import Data.Array
import Data.IORef
import Data.List

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-------------------------------------------------------------------------------

-- An EventArray stores events for a single HEC
type EventArray = Array Int GHCEvents.Event

-- HECs is a list of events for each HEC
type HECs = [(Int, EventArray)]

type MaybeHECsIORef = IORef  (Maybe HECs)

-------------------------------------------------------------------------------

ennumerateCapabilities :: [GHCEvents.Event] -> [Int]
ennumerateCapabilities events
  = sort (nub (map (cap . spec) events))

-------------------------------------------------------------------------------

-- Find the last timestamp value (in microseconds)
findLastTxValue :: HECs -> Integer
findLastTxValue hecs
  = maximum (map (event2ms . arrayLast) (map snd hecs))

-------------------------------------------------------------------------------

arrayLast a 
  = a!lastIdx
    where
    (_, lastIdx) = bounds a

-------------------------------------------------------------------------------


-- Origin for graph

ox :: Int
ox = 10

oy :: Int
oy = 30

-- Origin for capability bars

oycap :: Int
oycap = 60

-- Gap betweem capabilities

gapcap :: Int
gapcap = 55

-- Bar sizes

barHeight :: Int
barHeight = 20

-- Scaling down from ts ns 1e-9 values to microsecond 1e-6 values
scaleDown :: Integer
scaleDown = 1000

-------------------------------------------------------------------------------

eScale :: GHCEvents.Event -> Double -> Int
eScale e s = scaleIntegerBy (event2ms e) s

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a micro-second value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.

defaultScaleValue :: Double
defaultScaleValue = 0.1

-------------------------------------------------------------------------------
-- Scale a timetamp value (in microsecond) by a value s giving an Int

tsScale :: Timestamp -> Double -> Int
tsScale t s = scaleIntegerBy (ts2ms t) s

-------------------------------------------------------------------------------
-- Extact a timestamp value from an event and return as microseconds

event2ms :: GHCEvents.Event -> Integer
event2ms event = ts2ms (ts event)

-------------------------------------------------------------------------------

ts2ms :: Timestamp -> Integer
ts2ms t = (fromIntegral t) `div` scaleDown

-------------------------------------------------------------------------------

ts2msInt :: Timestamp -> Int
ts2msInt t = fromIntegral (ts2ms t)

-------------------------------------------------------------------------------

scaleIntegerBy :: Integer -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------
