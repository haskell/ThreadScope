module GUI.Timeline.Render.Constants (
    ox, oy, firstTraceY, tracePad,
    hecTraceHeight, hecSparksHeight, hecBarOff, hecBarHeight, hecLabelExtra,
    activityGraphHeight,
    ticksHeight, ticksPad
  ) where

-------------------------------------------------------------------------------

-- Origin for graph

ox :: Int
ox = 10

oy :: Int
oy = 30

-- Origin for capability bars

firstTraceY :: Int
firstTraceY = 60

-- Gap betweem traces in the timeline view

tracePad :: Int
tracePad = 20

-- HEC bar height

hecTraceHeight, hecSparksHeight, hecBarHeight, hecBarOff, hecLabelExtra :: Int

hecTraceHeight  = 40
hecSparksHeight = 2 * activityGraphHeight
hecBarHeight    = 20
hecBarOff       = 10

-- extra space to allow between HECs when labels are on.
-- ToDo: should be calculated somehow
hecLabelExtra  = 80

-- Activity graph

activityGraphHeight :: Int
activityGraphHeight = 100

-- Ticks

ticksHeight :: Int
ticksHeight = 20

ticksPad :: Int
ticksPad = 20
