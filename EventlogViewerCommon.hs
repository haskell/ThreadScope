module EventlogViewerCommon where
import Data.List

import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

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

tracePad :: Int
tracePad = 55

-- Bar height

barHeight :: Int
barHeight = 20

-- Ticks

ticksHeight :: Int
ticksHeight = 20

ticksPad :: Int
ticksPad = 20

-------------------------------------------------------------------------------

-- This scale value is used to map a micro-second value to a pixel unit.
-- To convert a timestamp value to a pixel value, multiply it by scale.
-- To convert a pixel value to a micro-second value, divide it by scale.
-- A negative value means the scale value to be computed to fit the
-- trace to the display.

defaultScaleValue :: Double
defaultScaleValue = -1.0

-------------------------------------------------------------------------------
-- Scale a timetamp value by a value s giving an Int

tsScale :: Timestamp -> Double -> Int
tsScale t s = scaleIntegerBy (toInteger t) s

-------------------------------------------------------------------------------

scaleIntegerBy :: Integral int => int -> Double -> Int
scaleIntegerBy i d
  = truncate (fromIntegral i * d)

-------------------------------------------------------------------------------

