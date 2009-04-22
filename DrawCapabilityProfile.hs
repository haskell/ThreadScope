-------------------------------------------------------------------------------
--- $Id: DrawCapabilityProfile.hs#7 2009/03/30 13:46:44 REDMOND\\satnams $
--- $Source: //depot/satnams/haskell/ThreadScope/DrawCapabilityProfile.hs $
-------------------------------------------------------------------------------

module DrawCapabilityProfile
where

-- Imports for GTK/Glade
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo 
import qualified Graphics.Rendering.Cairo as C

-- Imports for GHC Events
import qualified GHC.RTS.Events as GHCEvents
import GHC.RTS.Events hiding (Event)

-- Haskell library imports
import System.Environment
import Text.Printf
import Data.List
import qualified Data.Function

import Control.Monad
import Data.Array
import Data.IORef
import Data.Maybe

-- ThreadScope imports
import About
import CairoDrawing
import EventDuration
import EventlogViewerCommon
import StartTimes
import Ticks
import ViewerColours

-------------------------------------------------------------------------------
-- |The 'updateCanvas' function is called when an expose event
--  occurs. This function redraws the currently visible part of the
--  main trace canvas plus related canvases.

updateCanvas :: DrawingArea -> Viewport -> Statusbar -> ToggleButton ->
                ContextId ->  IORef Double ->
                IORef (Maybe [Int])  -> MaybeHECsIORef -> Event ->
                IO Bool
updateCanvas canvas viewport statusbar bw_button ctx scale 
             capabilitiesIORef eventArrayIORef
             event@(Expose _ area region count)
   = do -- putStrLn (show event)
        maybeCapabilities <- readIORef capabilitiesIORef
        maybeEventArray <- readIORef eventArrayIORef
        when (isJust maybeEventArray) $
           do let Just capabilities = maybeCapabilities
                  Just hecs = maybeEventArray
              bw_mode <- toggleButtonGetActive bw_button
              win <- widgetGetDrawWindow canvas 
              (width,height) <- widgetGetSize viewport
              -- Clear the drawing window
              drawWindowClearArea win x y width height
              -- Get the scrollbar settings
              hadj <- viewportGetHAdjustment viewport
              hadj_lower <- adjustmentGetLower hadj
              hadj_upper <- adjustmentGetUpper hadj
              hadj_value <- adjustmentGetValue hadj
              hadj_pagesize <- adjustmentGetPageSize hadj              
              let lastTx = findLastTxValue hecs
              scaleValue <- checkScaleValue scale viewport lastTx
              statusbarPush statusbar ctx ("Scale: " ++ show scaleValue ++ " width = " ++ show width ++ " height = " ++ show height ++ " hadj_value = " ++ show (truncate hadj_value) ++ " hadj_pagesize = " ++ show hadj_pagesize ++ " hadj_low = " ++ show hadj_lower ++ " hadj_upper = " ++ show hadj_upper)
              widgetSetSizeRequest canvas (truncate (scaleValue * fromIntegral lastTx) + 2*ox) ((length capabilities)*gapcap+oycap)
              renderWithDrawable win (currentView height hadj_value 
                 hadj_pagesize scaleValue maybeEventArray
                 maybeCapabilities bw_mode)
        return True
      where
      Rectangle x y _ _ = area 
updateCanvas _ _ _ _ _ _ _ _ _ = return True

-------------------------------------------------------------------------------
-- Estimate the width of the vertical scrollbar at 20 pixels

checkScaleValue :: IORef Double -> Viewport -> Integer -> IO Double
checkScaleValue scale viewport duration
  = do scaleValue <- readIORef scale
       if scaleValue < 0.0 then
         do (w, _) <- widgetGetSize viewport
            let newScale = fromIntegral (w - 2*ox - 20 - barHeight) / (fromIntegral (duration))
            writeIORef scale newScale
            return newScale 
        else
         return scaleValue

-------------------------------------------------------------------------------


currentView :: Int -> Double -> Double -> Double -> Maybe HECs -> Maybe [Int]
               -> Bool -> Render ()
currentView height hadj_value hadj_pagesize scaleValue maybeEventArray maybeCapabilities bw_mode
  = do when (isJust maybeEventArray) $ do
         let Just hecs = maybeEventArray
             Just capabilities = maybeCapabilities
             lastTx = findLastTxValue hecs
             startPos = truncate (hadj_value / scaleValue) -- startPos in us
             endPos = truncate ((hadj_value + hadj_pagesize) / scaleValue) `min` lastTx -- endPos in us
             startTick = ((startPos `div` (100*tickAdj) - 1) * 100*tickAdj) `max` 0
             endTick = (endPos `div` (100*tickAdj) + 1) * 100 * tickAdj 
             tickAdj = tickScale scaleValue
         selectFontFace "times" FontSlantNormal FontWeightNormal
         setFontSize 12
         setSourceRGBAhex blue 1.0
         setLineWidth 1.0
         draw_line (ox, oy) 
                   (ox+ scaleIntegerBy endTick scaleValue, oy)
         drawTicks height scaleValue startTick (10*tickAdj) (100*tickAdj) endTick
         mapM_ (hecView bw_mode height scaleValue hadj_value hadj_pagesize) 
               (map snd hecs)

-------------------------------------------------------------------------------

hecView bw_mode height scaleValue hadj_value hadj_pagesize eventArray
  = do sequence_ [drawEvent bw_mode scaleValue eventArray i |
                  i <- [startIndex..endIndex]]
       mapM_ (drawDuration scaleValue) durations
    where
    startFrom = findStartEvents eventArray startIndex
    endAt = findEndEvents eventArray endIndex lastIndex
    (_, lastIndex) = bounds eventArray
    startIndex = findStartIndexFromTime eventArray (truncate (hadj_value / scaleValue)) 0 lastIndex
    endIndex = findEndIndexFromTime eventArray (truncate ((hadj_value + hadj_pagesize) / scaleValue)) startIndex lastIndex
    -- Test bar representation
    durations = eventArrayToDuration eventArray

-------------------------------------------------------------------------------

drawDuration :: Double -> EventDuration -> Render ()
drawDuration scaleValue (ThreadRun id c startTime endTime)
  = do setSourceRGBA 1.0 0.0 0.0 0.8
       draw_rectangle_opt False
                      (ox + tsScale startTime scaleValue) -- x
                      (oycap+c*gapcap+barHeight)           -- y
                      (tsScale (endTime - startTime) scaleValue) -- w
                      (barHeight `div` 2)                        -- h
drawDuration _ other = return ()

-------------------------------------------------------------------------------

-- Binary search for starting position for currently visible part of
-- the drawing canvas
findStartIndexFromTime :: EventArray -> Integer -> Int -> Int -> Int
findStartIndexFromTime eventArray atOrBefore low high
  = if high - low <= 1 then
      low
    else
      if atOrBefore >= (event2ms $ eventArray!currentIndex) && atOrBefore < (event2ms $ eventArray!(currentIndex+1)) then
        currentIndex
      else
        if atOrBefore < (event2ms $ eventArray!currentIndex) then
          findStartIndexFromTime eventArray atOrBefore low (low + half_width)
        else
          findStartIndexFromTime eventArray atOrBefore (low+half_width) high
      where
      currentIndex = low + half_width
      half_width = (1 + high - low) `div` 2
                  
-------------------------------------------------------------------------------

-- Binary search for end position for currently visible part of
-- the drawing canvas

findEndIndexFromTime :: EventArray -> Integer -> Int -> Int -> Int 
findEndIndexFromTime eventArray atOrBefore low high
  = if high - low <= 1 then
      high
    else
      if atOrBefore >= (event2ms $ eventArray!currentIndex) && atOrBefore < (event2ms $ eventArray!(currentIndex+1)) then
        currentIndex
      else
        if atOrBefore < (event2ms $ eventArray!currentIndex) then
          findEndIndexFromTime eventArray atOrBefore low (low + half_width)
        else
          findEndIndexFromTime eventArray atOrBefore (low+half_width) high
      where
      currentIndex = low + half_width
      half_width = (1 + high - low) `div` 2

-------------------------------------------------------------------------------

findStartEvents eventArray 0 = 0
findStartEvents eventArray idx
  = case spec (eventArray!idx) of
      RunThread {cap=c, thread=t} -> idx
      StartGC {cap=c} -> idx
      _ -> findStartEvents eventArray (idx-1)

-------------------------------------------------------------------------------

findEndEvents eventArray idx maxidx
  = if idx == maxidx then
      idx
    else
      case spec (eventArray!idx) of
        StopThread {cap=c, thread=t} -> idx
        EndGC {cap=c} -> idx
        _ -> findEndEvents eventArray (idx+1) maxidx

-------------------------------------------------------------------------------

drawAllEvents :: Int -> Double -> EventArray -> Int -> Render ()
drawAllEvents height scaleValue  eventArray lastTx
  = drawEventRange False height scaleValue eventArray (bounds eventArray)

-------------------------------------------------------------------------------

drawEventRange :: Bool -> Int -> Double -> EventArray -> (Int, Int) -> 
                  Render ()
drawEventRange bw_mode height scaleValue eventArray (startIndex, endIndex)
  =    do -- translate (-(fromIntegral (scaleIntBy startTick scaleValue))) 0
          selectFontFace "times" FontSlantNormal FontWeightNormal
          setFontSize 12
          setSourceRGBAhex blue 1.0
          setLineWidth 1.0
          draw_line (ox, oy) 
                    (ox+ scaleIntegerBy endTick scaleValue, oy)
          drawTicks height scaleValue startTick (10*tickAdj) (100*tickAdj) endTick
          sequence_ [drawEvent bw_mode scaleValue eventArray i | i <- [startIndex..endIndex]]
    where
    startPos = if startIndex == 0 then
                 0
               else
                 event2ms (eventArray!startIndex) -- in microseconds
    endPos = event2ms (eventArray!endIndex) -- in microseconds
    startTick = ((startPos `div` (100*tickAdj) - 1) * 100*tickAdj) `max` 0
    endTick = (endPos `div` (100*tickAdj) + 1) * 100 * tickAdj
    tickAdj = tickScale scaleValue

-------------------------------------------------------------------------------

updateCapabilityCanvas :: DrawingArea -> IORef (Maybe [Int]) -> Event ->
                          IO Bool
updateCapabilityCanvas canvas capabilitiesIORef (Expose { eventArea=rect }) 
   = do maybeCapabilities <- readIORef capabilitiesIORef
        when (maybeCapabilities /= Nothing)
          (do let Just capabilities = maybeCapabilities
              win <- widgetGetDrawWindow canvas 
              gc <- gcNew win
              mapM_ (labelCapability canvas gc) capabilities
          )
        return True

-------------------------------------------------------------------------------

labelCapability :: DrawingArea -> GC -> Int -> IO ()
labelCapability canvas gc n
  = do win <- widgetGetDrawWindow canvas
       txt <- canvas `widgetCreateLayout` ("HEC " ++ show n)
       drawLayoutWithColors win gc 10 (oycap+6+gapcap*n) txt (Just black) Nothing

-------------------------------------------------------------------------------

subscriptThreashold :: Double
subscriptThreashold = 0.2  

-------------------------------------------------------------------------------
            
drawEvent :: Bool -> Double -> EventArray -> Int -> Render ()
drawEvent bw_mode scaleValue eventArray idx
  = case spec event of 
      CreateThread{cap=c, thread=t} -> 
        when (scaleValue >= 0.25) $ do
          setSourceRGBAhex lightBlue 0.8 
          setLineWidth 2.0
          draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
          when (scaleValue >= 4.0)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                textPath (show t ++ " created")
                C.fill
            )
      RunThread{cap=c, thread=t} -> return ()
      CreateSpark{cap=c, thread=t} -> 
           when (scaleValue >= 0.25) $
             do setSourceRGBAhex yellow 0.8 
                setLineWidth 2.0
                draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
      RunSpark{cap=c, thread=t} -> 
           when (scaleValue >= 0.25) $
             do setSourceRGBAhex magenta 0.8 
                setLineWidth 2.0
                draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
      StealSpark{cap=c, thread=t, victimCap=vc} -> 
           when (scaleValue >= 0.25) $
             do setSourceRGBAhex pink 0.8 
                setLineWidth 2.0
                draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
      CreateSparkThread{cap=c, sparkThread=st} -> 
           when (scaleValue >= 0.25) $
             do setSourceRGBAhex darkBrown 0.8 
                setLineWidth 2.0
                draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
      StopThread{cap=c, thread=t, GHC.RTS.Events.status=s} ->
        do let startTime = findRunThreadTime eventArray (idx-1)
           -- Draw a solid bar to show the thread duration
           if not bw_mode then
             setSourceRGBA 0.0 1.0 0.0 0.8 -- Green bar
            else
             setSourceRGBA 0.0 0.0 0.0 0.8 -- Black bar
           let rectWidth = (tsScale (time event - startTime) scaleValue)
           draw_rectangle_opt (scaleValue >= 0.25) (ox+ tsScale startTime scaleValue) (oycap+c*gapcap) rectWidth (barHeight `div` 2)
           -- Optionally label the bar with the threadID if there is room
           let tStr = show t
           tExtent <- textExtents tStr
           when (textExtentsWidth tExtent < fromIntegral rectWidth) $ do
             move_to (ox+ tsScale startTime scaleValue, oycap+c*gapcap) 
             if not bw_mode then
               setSourceRGB 0.0 0.0 0.0
              else
               setSourceRGB 1.0 1.0 1.0
             relMoveTo 4 13
             textPath tStr
             C.fill
           -- Optionally write the reason for the thread bein stopped
           -- depending on the zoom value
           when (scaleValue >= subscriptThreashold)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                textPath (show t ++ " " ++ showThreadStopStatus s)
                C.fill
            )
      ThreadRunnable{cap=c, thread=t} ->
        when (scaleValue >= 0.1) $ do
           setSourceRGBAhex darkGreen 0.8 
           setLineWidth 2.0
           draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= 0.2)
            (do setSourceRGB 0.0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath (show t ++ " runnable")
                C.fill
            )
      RequestSeqGC{cap=c} -> 
        when (scaleValue >= 0.1) $ do
           setSourceRGBAhex cyan 0.8 
           setLineWidth 2.0
           draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= subscriptThreashold)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath ("seq GC req")
                C.fill
            )
      RequestParGC{cap=c} -> 
         when (scaleValue >= 0.1) $ do
           setSourceRGBA 1.0 0.0 1.0 0.8 
           setLineWidth 2.0
           draw_line (ox+eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
           when (scaleValue >= subscriptThreashold)
            (do setSourceRGB 0 0.0 0.0
                move_to (ox+eScale event scaleValue, oycap+c*gapcap-5)
                textPath ("par GC req")
                C.fill
            )
      StartGC _ -> return ()
      EndGC c -> do let startTime = findStartGCTime eventArray c (idx-1)
                    if not bw_mode then
                      setSourceRGBAhex orange 0.8 --  1.0 0.6 0.0 0.8 -- orange box
                     else
                      setSourceRGB 0.8 0.8 0.8 -- grey box
                    setLineWidth 2.0
                    draw_rectangle_opt (scaleValue >= 0.25) (ox+ tsScale startTime scaleValue) (oycap+c*gapcap + barHeight `div` 2) (tsScale (time event - startTime) scaleValue) (barHeight `div` 2)
      MigrateThread {cap=oldc, thread=t, newCap=c}
        -> when (scaleValue >= 0.1) $ do
              setSourceRGBAhex darkRed 0.8 
              setLineWidth 2.0
              draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
              when (scaleValue >= subscriptThreashold)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                   textPath (show t ++ " migrated from " ++ show oldc)
                   C.fill
               )
      WakeupThread {cap=c, thread=t, otherCap=otherc}
        -> when (scaleValue >= 0.1) $ do 
              setSourceRGBAhex purple 0.8 
              setLineWidth 2.0
              draw_line (ox+ eScale event scaleValue, oycap+c*gapcap-4) (ox+ eScale event scaleValue, oycap+c*gapcap+barHeight+4)
              when (scaleValue >= subscriptThreashold)
               (do setSourceRGB 0.0 0.0 0.0
                   move_to (ox+eScale event scaleValue, oycap+c*gapcap+barHeight+12)
                   textPath (show t ++ " woken from " ++ show otherc)
                   C.fill
               )
      Shutdown{cap=c} ->
         do setSourceRGBA (102/256) 0.0 (105/256) 0.8
            draw_rectangle (ox+ eScale event scaleValue) (oycap+c*gapcap) barHeight barHeight
      _ -> return ()    
    where
    event = eventArray!idx

-------------------------------------------------------------------------------
-- tickScale adjusts the spacing between ticks to avoid collisions

tickScale :: Double -> Integer
tickScale scaleValue
  = if scaleValue <= 2.89e-5 then
      100000
    else if scaleValue <= 2.31e-4 then
      10000
    else if scaleValue <= 3.125e-3 then
      1000
    else if scaleValue <= 0.0625 then
      100
    else if scaleValue <= 0.25 then
      10
    else -- Mark major ticks every 10us
      1

-------------------------------------------------------------------------------

showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "yielding"
showThreadStopStatus ThreadBlocked  = "blocked"
showThreadStopStatus ThreadFinished = "finished"
showThreadStopStatus ForeignCall    = "foreign call"

-------------------------------------------------------------------------------
