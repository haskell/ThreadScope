-------------------------------------------------------------------------------
--- $Id: ViewerColours.hs#2 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/ViewerColours.hs $
-------------------------------------------------------------------------------

module ViewerColours where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo 

-------------------------------------------------------------------------------

-- Colours

runningColour :: Color
runningColour = green

gcColour :: Color
gcColour = orange

gcStartColour, gcWorkColour, gcIdleColour, gcEndColour :: Color
gcStartColour = orange
gcWorkColour  = green
gcIdleColour  = white
gcEndColour   = orange

createThreadColour :: Color
createThreadColour = lightBlue

runSparkColour :: Color
runSparkColour = darkBlue

stealSparkColour :: Color
stealSparkColour = magenta

threadRunnableColour :: Color
threadRunnableColour = darkGreen

seqGCReqColour :: Color
seqGCReqColour = cyan

parGCReqColour :: Color
parGCReqColour = darkBlue

migrateThreadColour :: Color
migrateThreadColour = darkRed

threadWakeupColour :: Color
threadWakeupColour = purple

shutdownColour :: Color
shutdownColour = darkBrown

labelTextColour :: Color
labelTextColour = black

-------------------------------------------------------------------------------

black :: Color
black = Color 0 0 0

grey :: Color
grey = Color 0x8000 0x8000 0x8000

green :: Color
green = Color 0 0xFFFF 0

darkGreen :: Color
darkGreen = Color 0x0000 0x6600 0x0000

blue :: Color
blue = Color 0 0 0xFFFF

cyan :: Color
cyan = Color 0 0xFFFF 0xFFFF 

magenta :: Color
magenta = Color 0xFFFF 0 0xFFFF

lightBlue :: Color
lightBlue = Color 0x6600 0x9900 0xFF00

darkBlue :: Color
darkBlue = Color 0 0 0xBB00

purple :: Color
purple = Color 0x9900 0x0000 0xcc00

darkPurple :: Color
darkPurple = Color 0x6600 0 0x6600

darkRed :: Color
darkRed = Color 0xcc00 0x0000 0x0000

orange :: Color
orange = Color 0xFFFF 0x9900 0x0000 -- orange

profileBackground :: Color
profileBackground = Color 0xFFFF 0xFFFF 0xFFFF

tickColour :: Color
tickColour = Color 0x3333 0x3333 0xFFFF

darkBrown :: Color
darkBrown = Color 0x6600 0 0

yellow :: Color
yellow = Color 0xff00 0xff00 0x3300

white :: Color
white = Color 0xffff 0xffff 0xffff

-------------------------------------------------------------------------------      
setSourceRGBAhex :: Color -> Double -> Render ()
setSourceRGBAhex (Color r g b) t
  = setSourceRGBA (fromIntegral r/0xFFFF) (fromIntegral g/0xFFFF)
                  (fromIntegral b/0xFFFF) t

-------------------------------------------------------------------------------
