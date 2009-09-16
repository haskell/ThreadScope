module TestEvents (testTrace)
where

import GHC.RTS.Events 
import Data.Word

-------------------------------------------------------------------------------


testTrace :: String -> EventLog
testTrace name = eventLog (test name)

-------------------------------------------------------------------------------

eventLog :: [Event] -> EventLog
eventLog events
  = EventLog (Header testEventTypes) (Data events)

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

createSpark :: Word16
createSpark = 13

------------------------------------------------------------------------------

sparkToThread :: Word16
sparkToThread = 14

------------------------------------------------------------------------------

createSparkThread :: Word16
createSparkThread = 15

------------------------------------------------------------------------------

testEventTypes :: [EventType]
testEventTypes
  = [EventType create "Create thread" (Just 10),
     EventType runThread "Run thread" (Just 10),
     EventType stop "Stop thread" (Just 12),
     EventType runnable "Thread runnable" (Just 10),
     EventType migrate "Migrate thread" (Just 12),
     EventType runSpark "Run spark" (Just 10),
     EventType stealSpark "Steal spark" (Just 12),
     EventType shutdown "Shutdown" (Just 2),
     EventType wakeup "Wakeup thread" (Just 12),
     EventType startGC "Start GC" (Just 2),
     EventType finishGC "Finish GC" (Just 2),
     EventType reqSeqGC "Request sequetial GC" (Just 2),
     EventType reqParGC "Reqpargc parallel GC" (Just 2),
     EventType createSpark "Create spark" (Just 10),
     EventType sparkToThread "Spark to thread" (Just 0),
     EventType createSparkThread "Create spark thread" (Just 10)
    ]

-------------------------------------------------------------------------------

test :: String -> [Event]
test "test0"
  = [Event shutdown  4000000 (Shutdown 0)
    ]
-------------------------------------------------------------------------------

test "small"
  = [Event create    1000000 (CreateThread 0 1),
     Event runThread 2000000 (RunThread 0 1),
     Event stop      3000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

test "tick"
  = [-- A thread from 2s to 3s
     Event create    1000000000 (CreateThread 0 1),
     Event runThread 2000000000 (RunThread 0 1),
     Event stop      3000000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000000 (Shutdown 0),
     -- A thread from 0.2ms to 0.3ms
     Event create    1000000 (CreateThread 1 2),
     Event runThread 2000000 (RunThread 1 2),
     Event stop      3000000 (StopThread 1 2 ThreadFinished),
     Event shutdown  4000000 (Shutdown 1),
    -- A thread from 0.2us to 0.3us
     Event create    1000 (CreateThread 2 3),
     Event runThread 2000 (RunThread 2 3),
     Event stop      3000 (StopThread 2 3 ThreadFinished),
     Event shutdown  4000 (Shutdown 2)
    ]

-------------------------------------------------------------------------------

test "tick2"
  = [-- A thread create  but no run
     Event create    1000000000 (CreateThread 0 1),
     Event shutdown  4000000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

test "tick3"
  = [-- A thread from 2s to 3s
     Event create    1000000000 (CreateThread 0 1),
     Event runThread 2000000000 (RunThread 0 1),
     Event stop      3000000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

test "tick4"
  = [-- A test for scale values close to 1.0
     Event create    100 (CreateThread 0 1),
     Event runThread 200 (RunThread 0 1),
     Event stop      300 (StopThread 0 1 ThreadFinished),
     Event shutdown  400 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

test "tick5"
  = [-- A thread from 2s to 3s
     Event create    1000000000 (CreateThread 0 1),
     Event runThread 2000000000 (RunThread 0 1),
     Event stop      3000000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

test _ = []

-------------------------------------------------------------------------------
