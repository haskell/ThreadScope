{-# LANGUAGE CPP,BangPatterns,PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-
 - Author: Donnie Jones, Simon Marlow
 - Events.hs
 -   Parser functions for GHC RTS EventLog framework.
 -}
 
module GHC.RTS.Events (
       -- * The event log types                       
       EventLog(..),
       EventType(..),
       Event(..),
       EventTypeSpecificInfo(..),
       ThreadStopStatus(..),
       Header(..),
       Data(..),
       Timestamp,
       ThreadId,

       -- * Reading an event log from a file
       readEventLogFromFile,

       -- * Utilities
       CapEvent(..), sortEvents, groupEvents, sortGroups,

       -- * Printing
       showEventTypeSpecificInfo, showThreadStopStatus
  ) where

{- Libraries. -}
import Data.Word (Word16, Word32, Word64)
import Data.Binary
import Data.Binary.Get
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Function
import Data.List
import Data.Either
import Text.Printf

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

{- Type synonyms. -}
type Filename = String

-- EventType.
type EventTypeNum = Word16
type EventTypeDescLen = Word32
type EventTypeDesc = String
type EventTypeSize = Word16
-- Event.
type EventDescription = String
type Timestamp = Word64
type ThreadId = Word32
type CapNo = Word16
type Marker = Word32

{-
 - Data type delcarations to build the GHC RTS data format,
 - which is a (header, data) pair.
 -
 - Header contains EventTypes.
 - Data contains Events.
 -}
data EventLog =
  EventLog {
    header :: Header,
    dat    :: Data
  } deriving Show

newtype Header = Header {
     eventTypes :: [EventType]
  } deriving Show

data Data = Data {
     events :: [Event]
  } deriving Show

data EventType =
  EventType {
    num  :: EventTypeNum,
    desc :: EventTypeDesc,
    size :: Maybe EventTypeSize -- ^ 'Nothing' indicates variable size
  } deriving Show

data Event = 
  Event {
    time :: {-# UNPACK #-}!Timestamp,
    spec :: EventTypeSpecificInfo
  } deriving Show

data EventTypeSpecificInfo
  = Startup            { n_caps :: Int
                       }
  | EventBlock         { end_time     :: Timestamp, 
                         cap          :: Int, 
                         block_events :: [Event]
                       }
  | CreateThread       { thread :: {-# UNPACK #-}!ThreadId
                       }
  | RunThread          { thread :: {-# UNPACK #-}!ThreadId 
                       }
  | StopThread         { thread :: {-# UNPACK #-}!ThreadId,
                         status :: ThreadStopStatus
                       }
  | ThreadRunnable     { thread :: {-# UNPACK #-}!ThreadId
                       }
  | MigrateThread      { thread :: {-# UNPACK #-}!ThreadId,
                         newCap :: {-# UNPACK #-}!Int
                       }
  | RunSpark           { thread :: {-# UNPACK #-}!ThreadId
                       }
  | StealSpark         { thread :: {-# UNPACK #-}!ThreadId,
                         victimCap :: {-# UNPACK #-}!Int
                       }
  | CreateSparkThread  { sparkThread :: {-# UNPACK #-}!ThreadId
                       }
  | WakeupThread       { thread :: {-# UNPACK #-}!ThreadId, 
                         otherCap :: {-# UNPACK #-}!Int
                       }
  | Shutdown           { }
  | RequestSeqGC       { }
  | RequestParGC       { }
  | StartGC            { }
  | GCWork             { }
  | GCIdle             { }
  | GCDone             { }
  | EndGC              { }
  | Message            { msg :: String }
  | UserMessage        { msg :: String }
  | UnknownEvent       { ref  :: {-# UNPACK #-}!EventTypeNum }

  deriving Show

--sync with ghc/includes/Constants.h
data ThreadStopStatus
 = NoStatus
 | HeapOverflow
 | StackOverflow
 | ThreadYielding
 | ThreadBlocked
 | ThreadFinished
 | ForeignCall
 deriving (Enum, Show)

-- reader/Get monad that passes around the event types
type GetEvents a = ReaderT (IntMap EventType) (ErrorT String Get) a
  
type GetHeader a = ErrorT String Get a

getH :: Binary a => GetHeader a
getH = lift get

getE :: Binary a => GetEvents a
getE = lift $ lift get

------------------------------------------------------------------------------
-- Binary instances

getEventType :: GetHeader EventType
getEventType = do 
           etNum <- getH
           size <- getH :: GetHeader EventTypeSize
           let etSize = if size == 0xffff then Nothing else Just size
           -- 0xffff indicates variable-sized event
           etDescLen <- getH :: GetHeader EventTypeDescLen
           etDesc <- getEtDesc (fromIntegral etDescLen) 
           etExtraLen <- getH :: GetHeader Word32
           _skip  <- replicateM_ (fromIntegral etExtraLen) (lift getWord8)
           ete <- getH :: GetHeader Marker
           when (ete /= EVENT_ET_END) $
              throwError ("Event Type end marker not found.")
           return (EventType etNum etDesc etSize)
           where 
             getEtDesc :: Int -> GetHeader [Char]
             getEtDesc s = replicateM s (getH :: GetHeader Char)

getHeader :: GetHeader Header
getHeader = do 
           hdrb <- getH :: GetHeader Marker
           when (hdrb /= EVENT_HEADER_BEGIN) $
                throwError "Header begin marker not found"
           hetm <- getH :: GetHeader Marker
           when (hetm /= EVENT_HET_BEGIN) $ 
                throwError "Header Event Type begin marker not found"
           ets <- getEventTypes
           emark <- getH :: GetHeader Marker
           when (emark /= EVENT_HEADER_END) $
                throwError "Header end marker not found"
           return (Header ets)
     where    
       getEventTypes :: GetHeader [EventType]
       getEventTypes = do
           m <- getH :: GetHeader Marker
           case () of 
            _ | m == EVENT_ET_BEGIN -> do
                   et <- getEventType
                   nextET <- getEventTypes
                   return (et : nextET)
              | m == EVENT_HET_END ->
                   return []
              | otherwise ->
                   throwError "Malformed list of Event Types in header"

getEvent :: GetEvents (Maybe Event)
getEvent = do 
  etRef <- getE
  if (etRef == EVENT_DATA_END) 
     then return Nothing
     else do !ts   <- getE
             spec <- getEvSpecInfo etRef
             return (Just (Event ts spec))
           
getEvSpecInfo :: EventTypeNum -> GetEvents EventTypeSpecificInfo
getEvSpecInfo num = case fromIntegral num :: Int of

 EVENT_STARTUP -> do -- (n_caps)
  c <- getE :: GetEvents CapNo
  return Startup{ n_caps = fromIntegral c }

 EVENT_BLOCK_MARKER -> do -- (size, end_time, cap)
  block_size <- getE :: GetEvents Word32
  end_time <- getE :: GetEvents Timestamp
  c <- getE :: GetEvents CapNo
  lbs <- lift . lift $ getLazyByteString (fromIntegral block_size - 24)
  etypemap <- ask
  let e_events = runGet (runErrorT $ runReaderT getEventBlock etypemap) lbs
  return EventBlock{ end_time=end_time,
                     cap= fromIntegral c, 
                     block_events=case e_events of
                                    Left s -> error s
                                    Right es -> es }

 EVENT_CREATE_THREAD -> do  -- (thread)
  t <- getE
  return CreateThread{thread=t}

 EVENT_RUN_THREAD -> do  --  (thread)
  t <- getE
  return RunThread{thread=t}

 EVENT_STOP_THREAD -> do  -- (thread, status)
  t <- getE
  s <- getE :: GetEvents Word16
  return StopThread{thread=t, status= toEnum (fromIntegral s)}

 EVENT_THREAD_RUNNABLE -> do  -- (thread)
  t <- getE
  return ThreadRunnable{thread=t}

 EVENT_MIGRATE_THREAD -> do  --  (thread, newCap)
  t  <- getE
  nc <- getE :: GetEvents CapNo
  return MigrateThread{thread=t,newCap=fromIntegral nc}

 EVENT_RUN_SPARK -> do  -- (thread)
  t <- getE
  return RunSpark{thread=t}

 EVENT_STEAL_SPARK -> do  -- (thread, victimCap)
  t  <- getE
  vc <- getE :: GetEvents CapNo
  return StealSpark{thread=t,victimCap=fromIntegral vc}

 EVENT_CREATE_SPARK_THREAD -> do  -- (sparkThread)
  st <- getE :: GetEvents ThreadId
  return CreateSparkThread{sparkThread=st}

 EVENT_SHUTDOWN -> return Shutdown

 EVENT_THREAD_WAKEUP -> do  -- (thread, other_cap)
  t <- getE
  oc <- getE :: GetEvents CapNo
  return WakeupThread{thread=t,otherCap=fromIntegral oc}

 EVENT_REQUEST_SEQ_GC -> return RequestSeqGC

 EVENT_REQUEST_PAR_GC -> return RequestParGC

 EVENT_GC_START -> return StartGC
 EVENT_GC_WORK  -> return GCWork
 EVENT_GC_IDLE  -> return GCIdle
 EVENT_GC_DONE  -> return GCDone
 EVENT_GC_END   -> return EndGC

 EVENT_LOG_MSG -> do -- (msg)
  num <- getE :: GetEvents Word16
  bytes <- replicateM (fromIntegral num) getE 
  return Message{ msg = map (chr . fromIntegral) (bytes :: [Word8]) }

 EVENT_USER_MSG -> do -- (msg)
  num <- getE :: GetEvents Word16
  bytes <- replicateM (fromIntegral num) getE 
  return UserMessage{ msg = map (chr . fromIntegral) (bytes :: [Word8]) }

 other -> do -- unrecognised event, just skip it
  etypes <- ask
  case M.lookup (fromIntegral other) etypes of
    Nothing -> throwError ("getEvSpecInfo: undeclared event type: " ++ show other)
    Just t  -> do
      bytes <- case size t of
                 Just n  -> return n
                 Nothing -> getE :: GetEvents Word16
      skip  <- lift . lift $ replicateM_ (fromIntegral bytes) getWord8
      return UnknownEvent{ ref = num }


getData :: GetEvents Data
getData = do
   db <- getE :: GetEvents Marker
   when (db /= EVENT_DATA_BEGIN) $ throwError "Data begin marker not found"
   getEvents []
   where
              getEvents :: [Event] -> GetEvents Data
              getEvents events = do
                mb_e <- getEvent
                case mb_e of
                  Nothing -> return (Data (reverse events))
                  Just e  -> getEvents (e:events)

getEventBlock :: GetEvents [Event]
getEventBlock = do
  b <- lift . lift $ isEmpty
  if b then return [] else do
  mb_e <- getEvent
  case mb_e of
    Nothing -> return []
    Just e  -> do
      es <- getEventBlock
      return (e:es)

getEventLog :: ErrorT String Get EventLog
getEventLog = do
    header <- getHeader
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
    dat <- runReaderT getData imap
    return (EventLog header dat)

readEventLogFromFile :: FilePath -> IO (Either String EventLog)
readEventLogFromFile f = do
    s <- L.readFile f
    return $ runGet (do v <- runErrorT $ getEventLog
                        m <- isEmpty
                        m `seq` return v)  s

-- -----------------------------------------------------------------------------
-- Utilities

-- | An event annotated with the Capability that generated it, if any
data CapEvent 
  = CapEvent { ce_cap   :: Maybe Int,
               ce_event :: Event
               -- we could UNPACK ce_event, but the Event constructor
               -- might be shared, in which case we could end up
               -- increasing the space usage.
             }

sortEvents :: [Event] -> [CapEvent]
sortEvents = sortGroups . groupEvents

-- | Sort the raw event stream by time, annotating each event with the
-- capability that generated it.
sortGroups :: [(Maybe Int, [Event])] -> [CapEvent]
sortGroups groups = mergesort' (compare `on` (time . ce_event)) $
                      [ [ CapEvent cap e | e <- es ] 
                      | (cap, es) <- groups ]
     -- sorting is made much faster by the way that the event stream is
     -- divided into blocks of events.  
     --  - All events in a block belong to a particular capability
     --  - The events in a block are ordered by time
     --  - blocks for the same capability appear in time order in the event
     --    stream and do not overlap.
     --
     -- So to sort the events we make one list of events for each
     -- capability (basically just concat . filter), and then
     -- merge the resulting lists.

groupEvents :: [Event] -> [(Maybe Int, [Event])]
groupEvents es = (Nothing, n_events) : 
                 [ (Just (cap (head blocks)), concatMap block_events blocks)
                 | blocks <- groups ]
  where
   (blocks, anon_events) = partitionEithers (map separate es)
      where separate e | b@EventBlock{} <- spec e = Left  b
                       | otherwise                = Right e

   (cap_blocks, gbl_blocks) = partition (is_cap . cap) blocks
      where is_cap c = fromIntegral c /= ((-1) :: Word16)

   groups = groupBy ((==) `on` cap) $ sortBy (compare `on` cap) cap_blocks

     -- There are two sources of events without a capability: events
     -- in the raw stream not inside an EventBlock, and EventBlocks
     -- with cap == -1.  We have to merge those two streams.
   n_events = merge (compare `on` time) anon_events 
                 (concatMap block_events gbl_blocks)

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' _   [] = []
mergesort' _   [xs] = xs
mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
merge_pairs _   [] = []
merge_pairs _   [xs] = [xs]
merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _   [] ys = ys
merge _   xs [] = xs
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

-----------------------------------------------------------------------------
-- Some pretty-printing support

showEventTypeSpecificInfo :: EventTypeSpecificInfo -> String
showEventTypeSpecificInfo spec =
      case spec of
        Startup n_caps ->
          printf "startup: %d capabilities" n_caps
        EventBlock end_time cap _block_events ->
          printf "event block: cap %d, end time: %d\n" cap end_time
        CreateThread thread ->
          printf "creating thread %d" thread
        RunThread thread ->
          printf "running thread %d" thread
        StopThread thread status ->
          printf "stopping thread %d (%s)" thread (showThreadStopStatus status)
        ThreadRunnable thread ->
          printf "thread %d is runnable" thread
        MigrateThread thread newCap  ->
          printf "migrating thread %d to cap %d" thread newCap
        RunSpark thread ->
          printf "running a local spark (thread %d)" thread
        StealSpark thread victimCap ->
          printf "thread %d stealing a spark from cap %d" thread victimCap
        CreateSparkThread sparkThread ->
          printf "creating spark thread %d" sparkThread
        Shutdown ->
          printf "shutting down"
        WakeupThread thread otherCap ->
          printf "waking up thread %d on cap %d" thread otherCap
        RequestSeqGC ->
          printf "requesting sequential GC"
        RequestParGC ->
          printf "requesting parallel GC"
        StartGC ->
          printf "starting GC"
        EndGC ->
          printf "finished GC"
        GCWork ->
          printf "GC working"
        GCIdle ->
          printf "GC idle"
        GCDone ->
          printf "GC done"
	_ ->
          printf "unknown event type"


showThreadStopStatus :: ThreadStopStatus -> String
showThreadStopStatus HeapOverflow   = "heap overflow"
showThreadStopStatus StackOverflow  = "stack overflow"
showThreadStopStatus ThreadYielding = "thread yielding"
showThreadStopStatus ThreadBlocked  = "thread blocked"
showThreadStopStatus ThreadFinished = "thread finished"
showThreadStopStatus ForeignCall    = "making a foreign call"
