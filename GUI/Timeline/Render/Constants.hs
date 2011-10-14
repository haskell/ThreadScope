module GUI.Timeline.Render.Constants (
    ox, firstTraceY, tracePad,
    hecTraceHeight, hecSparksHeight, hecBarOff, hecBarHeight, hecLabelExtra,
    activityGraphHeight, stdHistogramHeight,
    ticksHeight, ticksPad
  ) where

-------------------------------------------------------------------------------

-- The standard gap in various graphs

ox :: Int
ox = 10

-- Origin for traces

firstTraceY :: Int
firstTraceY = 13

-- Gap betweem traces in the timeline view

tracePad :: Int
tracePad = 20

-- HEC bar height

hecTraceHeight, hecSparksHeight, hecBarHeight, hecBarOff, hecLabelExtra :: Int

hecTraceHeight  = 40
hecBarHeight    = 20
hecBarOff       = 10

-- extra space to allow between HECs when labels are on.
-- ToDo: should be calculated somehow
hecLabelExtra  = 80

-- Activity graph

activityGraphHeight :: Int
activityGraphHeight = 100

-- Should be divisible by 10, for regular 1/10th size vertical scale ticks:
hecSparksHeight :: Int
hecSparksHeight = 10 * (activityGraphHeight `div` 10)

-- Histogram height when displayed with other traces (e.g., in PNG/PDF).
stdHistogramHeight :: Int
stdHistogramHeight = 2 * hecSparksHeight

-- Ticks

ticksHeight :: Int
ticksHeight = 20

ticksPad :: Int
ticksPad = 20
