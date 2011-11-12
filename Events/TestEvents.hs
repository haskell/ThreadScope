module Events.TestEvents (testTrace)
where

import GHC.RTS.Events
import Data.Word

-------------------------------------------------------------------------------


testTrace :: String -> EventLog
testTrace name = eventLog (test name)

-------------------------------------------------------------------------------

eventLog :: [Event] -> EventLog
eventLog events =
  let specBy1000 e@EventBlock{} =
        e{end_time = end_time e * 1000,
          block_events = map eBy1000 (block_events e)}
      specBy1000 e = e
      eBy1000 ev = ev{time = time ev * 1000,
                      spec = specBy1000 (spec ev)}
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
     Event 0 (Startup 1)
    ]

-------------------------------------------------------------------------------


test "empty1"
  = [
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000 0 []
    ]

-------------------------------------------------------------------------------

test "test0"
  = [
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000 0 [
          Event 4000000 Shutdown
      ]
    ]
-------------------------------------------------------------------------------

test "small"
  = [
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000 0 [
       Event 1000000 (CreateThread 1),
       Event 2000000 (RunThread 1),
       Event 3000000 (StopThread 1 ThreadFinished),
       Event 4000000 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------

test "tick"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 3),
     Event 0 $ EventBlock 4000000000 0 [
       Event 1000000000 (CreateThread 1),
       Event 2000000000 (RunThread 1),
       Event 3000000000 (StopThread 1 ThreadFinished),
       Event 4000000000 (Shutdown)
     ],
     -- A thread from 0.2ms to 0.3ms
     Event 0 $ EventBlock 4000000000 1 [
       Event 1000000 (CreateThread 2),
       Event 2000000 (RunThread 2),
       Event 3000000 (StopThread 2 ThreadFinished),
       Event 4000000 (Shutdown)
    ],
    -- A thread from 0.2us to 0.3us
     Event 0 $ EventBlock 4000000000 2 [
       Event 1000 (CreateThread 3),
       Event 2000 (RunThread 3),
       Event 3000 (StopThread 3 ThreadFinished),
       Event 4000 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------

test "tick2"
  = [-- A thread create  but no run
     Event 0 (Startup 1),
       Event 0 $ EventBlock 4000000000 0 [
       Event 1000000000 (CreateThread 1),
       Event 4000000000 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------

test "tick3"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000000 0 [
       Event 1000000000 (CreateThread 1),
       Event 2000000000 (RunThread 1),
       Event 3000000000 (StopThread 1 ThreadFinished),
       Event 4000000000 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------

test "tick4"
  = [-- A test for scale values close to 1.0
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000000 0 [
       Event 100 (CreateThread 1),
       Event 200 (RunThread 1),
       Event 300 (StopThread 1 ThreadFinished),
       Event 400 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------

test "tick5"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 1),
     Event 0 $ EventBlock 4000000000 0 [
       Event 1000000000 (CreateThread 1),
       Event 2000000000 (RunThread 1),
       Event 3000000000 (StopThread 1 ThreadFinished),
       Event 4000000000 (Shutdown)
     ]
    ]

-------------------------------------------------------------------------------
-- A long tick run to check small and large tick labels

test "tick6" = chequered 2 100 10000000

-------------------------------------------------------------------------------

test "overlap"
  = [-- A thread from 2s to 3s
     Event 0 (Startup 1),
     Event 0 $ EventBlock 3000 0 [
       Event 1000 (CreateThread 1),
       Event 1100 (RunThread 1),
       Event 1200 (CreateThread 2),
       Event 1300 (StopThread 1 ThreadFinished),

       Event 1400 (RunThread 2),
       Event 1500 (CreateThread 3),
       Event 1500 (CreateThread 4),
       Event 1500 (StopThread 2 ThreadFinished),

       Event 1600 (RunThread 3),
       Event 1600 (CreateThread 5),
       Event 1600 (StopThread 3 ThreadFinished),

       Event 1700 (RunThread 4),
       Event 1700 (CreateThread 6),
       Event 1800 (StopThread 4 ThreadFinished),

       Event 3000 (Shutdown)
     ]
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
  = Event 0 (Startup (fromIntegral numThreads)) :
    makeChequered 1 numThreads basicDuration runLength

-------------------------------------------------------------------------------

makeChequered :: ThreadId -> ThreadId -> Timestamp -> Timestamp -> [Event]
makeChequered currentThread numThreads _basicDuration _runLength
              | currentThread > numThreads = [] -- All threads rendered
makeChequered currentThread numThreads basicDuration runLength
  = Event 0 eventBlock :
    makeChequered (currentThread+1) numThreads (2*basicDuration) runLength
    where
    eventBlock :: EventInfo
    eventBlock = EventBlock runLength (fromIntegral (currentThread-1))
                 (Event 0 (CreateThread currentThread)
                 : chequeredPattern currentThread 0 basicDuration runLength)

-------------------------------------------------------------------------------

chequeredPattern :: ThreadId -> Timestamp -> Timestamp -> Timestamp -> [Event]
chequeredPattern currentThread currentPos basicDuration runLength
  = if currentPos + 2*basicDuration > runLength then
      [Event runLength (Shutdown)]
    else
      [Event currentPos (RunThread currentThread),
       Event (currentPos+basicDuration) (StopThread currentThread ThreadYielding),
       Event (currentPos+basicDuration) StartGC,
       Event (currentPos+2*basicDuration) EndGC
      ] ++ chequeredPattern currentThread (currentPos+2*basicDuration) basicDuration runLength

-------------------------------------------------------------------------------
