-------------------------------------------------------------------------------
--- $Id: ViewerColours.hs#2 2009/07/18 22:48:30 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/ViewerColours.hs $
-------------------------------------------------------------------------------

module GUI.ViewerColours (Color, module GUI.ViewerColours) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

-------------------------------------------------------------------------------

-- Colours

runningColour :: Color
runningColour = darkGreen

gcColour :: Color
gcColour = orange

gcStartColour, gcWorkColour, gcIdleColour, gcEndColour :: Color
gcStartColour = orange
gcWorkColour  = orange
gcIdleColour  = white
gcEndColour   = orange

createThreadColour :: Color
createThreadColour = lightBlue

seqGCReqColour :: Color
seqGCReqColour = cyan

parGCReqColour :: Color
parGCReqColour = darkBlue

migrateThreadColour :: Color
migrateThreadColour = darkRed

threadWakeupColour :: Color
threadWakeupColour = green

shutdownColour :: Color
shutdownColour = darkBrown

labelTextColour :: Color
labelTextColour = white

bookmarkColour :: Color
bookmarkColour = Color 0xff00 0x0000 0xff00 -- pinkish

fizzledDudsColour, createdConvertedColour, overflowedColour :: Color
fizzledDudsColour      = grey
createdConvertedColour = darkGreen
overflowedColour       = red

userMessageColour :: Color
userMessageColour = darkRed

outerPercentilesColour :: Color
outerPercentilesColour = lightGrey

-------------------------------------------------------------------------------

black :: Color
black = Color 0 0 0

grey :: Color
grey = Color 0x8000 0x8000 0x8000

lightGrey :: Color
lightGrey = Color 0xD000 0xD000 0xD000

gtkBorderGrey :: Color
gtkBorderGrey = Color 0xF200 0xF100 0xF000

red :: Color
red = Color 0xFFFF 0 0

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
orange = Color 0xE000 0x7000 0x0000 -- orange

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
