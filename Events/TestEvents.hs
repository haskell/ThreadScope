module Events.TestEvents (testTrace)
where

import Data.Word
import GHC.RTS.Events

-------------------------------------------------------------------------------


testTrace :: String -> EventLog
testTrace name = eventLog (test name)

-------------------------------------------------------------------------------

eventLog :: [Event] -> EventLog
eventLog events =
  let eBy1000 ev = ev{evTime = evTime ev * 1000}
      eventsBy = map eBy1000 events
  in EventLog (Header testEventTypes) (Data eventsBy)

-------------------------------------------------------------------------------

create :: Word16
create = 0

-------------------------------------------------------------------------------

runThread :: Word16
runThread = 1

-------------------------------------------------------------------------------

stop :: Word16
stop = 2

-------------------------------------------------------------------------------

runnable :: Word16
runnable = 3

-------------------------------------------------------------------------------

migrate :: Word16
migrate = 4

-------------------------------------------------------------------------------

runSpark :: Word16
runSpark = 5

-------------------------------------------------------------------------------

stealSpark :: Word16
stealSpark = 6

-------------------------------------------------------------------------------

shutdown :: Word16
shutdown = 7

-------------------------------------------------------------------------------

wakeup :: Word16
wakeup = 8

-------------------------------------------------------------------------------

startGC :: Word16
startGC = 9

------------------------------------------------------------------------------

finishGC :: Word16
finishGC = 10

------------------------------------------------------------------------------

reqSeqGC :: Word16
reqSeqGC = 11

------------------------------------------------------------------------------

reqParGC :: Word16
reqParGC = 12

------------------------------------------------------------------------------

createSparkThread :: Word16
createSparkThread = 15

------------------------------------------------------------------------------

logMessage :: Word16
logMessage = 16

------------------------------------------------------------------------------

startup :: Word16
startup = 17

------------------------------------------------------------------------------

blockMarker :: Word16
blockMarker = 18

------------------------------------------------------------------------------

testEventTypes :: [EventType]
testEventTypes
  = [EventType create "Create thread" (Just 8),
     EventType runThread "Run thread" (Just 8),
     EventType stop "Stop thread" (Just 10),
     EventType runnable "Thread runnable" (Just 8),
     EventType migrate "Migrate thread" (Just 10),
     EventType runSpark "Run spark" (Just 8),
     EventType stealSpark "Steal spark" (Just 10),
     EventType shutdown "Shutdown" (Just 0),
     EventType wakeup "Wakeup thread" (Just 10),
     EventType startGC "Start GC" (Just 0),
     EventType finishGC "Finish GC" (Just 0),
     EventType reqSeqGC "Request sequetial GC" (Just 0),
     EventType reqParGC "Reqpargc parallel GC" (Just 0),
     EventType createSparkThread "Create spark thread" (Just 8),
     EventType logMessage "Log message" Nothing,
     EventType startup "Startup" (Just 0),
     EventType blockMarker "Block marker" (Just 14)
    ]

-------------------------------------------------------------------------------
test :: String -> [Event]
-------------------------------------------------------------------------------

test "empty0"
  = [
     Event 0 (Startup 1) (Just 0)
    ]

-------------------------------------------------------------------------------


test "empty1"
  = [
     Event 0 (Startup 1) (Just 0)
    ]

-------------------------------------------------------------------------------

test "test0"
  = [
     Event 0 (Startup 1) (Just 0),
     Event 4000000 Shutdown (Just 0)
    ]
-------------------------------------------------------------------------------

test "small"
  = [
     Event 0 (Startup 1) (Just 0),
     Event 1000000 (CreateThread 1) (Just 0),
     Event 2000000 (RunThread 1) (Just 0),
     Event 3000000 (StopThread 1 ThreadFinished) (Just 0),
     Event 4000000 (Shutdown) (Just 0)
    ]

-------------------------------------------------------------------------------

test "tick"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 3) (Just 0),
     Event 1000000000 (CreateThread 1) (Just 0),
     Event 2000000000 (RunThread 1) (Just 0),
     Event 3000000000 (StopThread 1 ThreadFinished) (Just 0),
     Event 4000000000 (Shutdown) (Just 0),
     -- A thread from 0.2ms to 0.3ms
     Event 1000000 (CreateThread 2) (Just 1),
     Event 2000000 (RunThread 2) (Just 1),
     Event 3000000 (StopThread 2 ThreadFinished) (Just 1),
     Event 4000000 (Shutdown) (Just 1),
    -- A thread from 0.2us to 0.3us
     Event 1000 (CreateThread 3) (Just 2),
     Event 2000 (RunThread 3) (Just 2),
     Event 3000 (StopThread 3 ThreadFinished) (Just 2),
     Event 4000 (Shutdown) (Just 2)
    ]

-------------------------------------------------------------------------------

test "tick2"
  = [-- A thread create  but no run
     Event 0 (Startup 1) (Just 0),
     Event 1000000000 (CreateThread 1) (Just 0),
     Event 4000000000 (Shutdown) (Just 0)
    ]

-------------------------------------------------------------------------------

test "tick3"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 1) (Just 0),
     Event 1000000000 (CreateThread 1) (Just 0),
     Event 2000000000 (RunThread 1) (Just 0),
     Event 3000000000 (StopThread 1 ThreadFinished) (Just 0),
     Event 4000000000 (Shutdown) (Just 0)
    ]

-------------------------------------------------------------------------------

test "tick4"
  = [-- A test for scale values close to 1.0
     Event 0 (Startup 1) (Just 0),
     Event 100 (CreateThread 1) (Just 0),
     Event 200 (RunThread 1) (Just 0),
     Event 300 (StopThread 1 ThreadFinished) (Just 0),
     Event 400 (Shutdown) (Just 0)
    ]

-------------------------------------------------------------------------------

test "tick5"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 1) (Just 0),
     Event 1000000000 (CreateThread 1) (Just 0),
     Event 2000000000 (RunThread 1) (Just 0),
     Event 3000000000 (StopThread 1 ThreadFinished) (Just 0),
     Event 4000000000 (Shutdown) (Just 0)
    ]

-------------------------------------------------------------------------------
-- A long tick run to check small and large tick labels

test "tick6" = chequered 2 100 10000000

-------------------------------------------------------------------------------

test "overlap"
  =   [-- A thread from 2s to 3s
       Event 0 (Startup 1) (Just 0),
       Event 1000 (CreateThread 1) (Just 0),
       Event 1100 (RunThread 1) (Just 0),
       Event 1200 (CreateThread 2) (Just 0),
       Event 1300 (StopThread 1 ThreadFinished) (Just 0),

       Event 1400 (RunThread 2) (Just 0),
       Event 1500 (CreateThread 3) (Just 0),
       Event 1500 (CreateThread 4) (Just 0),
       Event 1500 (StopThread 2 ThreadFinished) (Just 0),

       Event 1600 (RunThread 3) (Just 0),
       Event 1600 (CreateThread 5) (Just 0),
       Event 1600 (StopThread 3 ThreadFinished) (Just 0),

       Event 1700 (RunThread 4) (Just 0),
       Event 1700 (CreateThread 6) (Just 0),
       Event 1800 (StopThread 4 ThreadFinished) (Just 0),

       Event 3000 (Shutdown) (Just 0)
      ]

-------------------------------------------------------------------------------
-- These tests are for chequered patterns to help check for rendering
-- problems and also to help test the performance of scrolling etc.
-- Each line has a fixed frequency of a thread running and then performing GC.
-- Each successive HEC runs thread at half the frequency of the previous HEC.

test "ch1" = chequered 1 100 100000
test "ch2" = chequered 2 100 100000
test "ch3" = chequered 3 100 100000
test "ch4" = chequered 4 100 100000
test "ch5" = chequered 5 100 100000
test "ch6" = chequered 6 100 100000
test "ch7" = chequered 7 100 100000
test "ch8" = chequered 8 100 100000


-------------------------------------------------------------------------------

test _ = []

-------------------------------------------------------------------------------

chequered :: ThreadId -> Timestamp -> Timestamp -> [Event]
chequered numThreads basicDuration runLength
  = Event 0 (Startup (fromIntegral numThreads)) (Just 0) :
    makeChequered 1 numThreads basicDuration runLength

-------------------------------------------------------------------------------

makeChequered :: ThreadId -> ThreadId -> Timestamp -> Timestamp -> [Event]
makeChequered currentThread numThreads _basicDuration _runLength
              | currentThread > numThreads = [] -- All threads rendered
makeChequered currentThread numThreads basicDuration runLength
  = eventBlock ++
    makeChequered (currentThread+1) numThreads (2*basicDuration) runLength
    where
    eventBlock = Event 0 (CreateThread currentThread) (Just $ fromIntegral $ currentThread - 1)
                 : chequeredPattern currentThread 0 basicDuration runLength

-------------------------------------------------------------------------------

chequeredPattern :: ThreadId -> Timestamp -> Timestamp -> Timestamp -> [Event]
chequeredPattern currentThread currentPos basicDuration runLength
  = if currentPos + 2*basicDuration > runLength then
      [Event runLength Shutdown mcap]
    else
      [Event currentPos (RunThread currentThread) mcap,
       Event (currentPos+basicDuration) (StopThread currentThread ThreadYielding) mcap,
       Event (currentPos+basicDuration) StartGC mcap,
       Event (currentPos+2*basicDuration) EndGC mcap
      ] ++ chequeredPattern currentThread (currentPos+2*basicDuration) basicDuration runLength
 where mcap = Just $ fromIntegral $ currentThread - 1

-------------------------------------------------------------------------------
