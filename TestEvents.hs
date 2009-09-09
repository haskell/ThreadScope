module TestEvents (testTrace)
where

import GHC.RTS.Events 
import Data.Word

-------------------------------------------------------------------------------


testTrace :: String -> EventLog
testTrace "small" = eventLog events1
testTrace "tick"  = eventLog events2

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

events1 :: [Event]
events1
  = [Event create    1000000 (CreateThread 0 1),
     Event runThread 2000000 (RunThread 0 1),
     Event stop      3000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------

events2 :: [Event]
events2
  = [Event create    1000000000 (CreateThread 0 1),
     Event runThread 2000000000 (RunThread 0 1),
     Event stop      3000000000 (StopThread 0 1 ThreadFinished),
     Event shutdown  4000000000 (Shutdown 0)
    ]

-------------------------------------------------------------------------------
