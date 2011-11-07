module GUI.Timeline.Render.Constants (
    ox, firstTraceY, tracePad,
    hecTraceHeight, hecInstantHeight, hecSparksHeight,
    hecBarOff, hecBarHeight, hecLabelExtra,
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

hecTraceHeight, hecInstantHeight, hecBarHeight, hecBarOff, hecLabelExtra :: Int

hecTraceHeight   = 40
hecInstantHeight = 25
hecBarHeight     = 20
hecBarOff        = 10

-- extra space to allow between HECs when labels are on.
-- ToDo: should be calculated somehow
hecLabelExtra  = 80

-- Activity graph

activityGraphHeight :: Int
activityGraphHeight = 100

-- Height of the spark graphs.
hecSparksHeight :: Int
hecSparksHeight = activityGraphHeight

-- Histogram height when displayed with other traces (e.g., in PNG/PDF).
stdHistogramHeight :: Int
stdHistogramHeight = hecSparksHeight

-- Ticks

ticksHeight :: Int
ticksHeight = 20

ticksPad :: Int
ticksPad = 20
