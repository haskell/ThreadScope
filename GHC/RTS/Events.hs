{-# LANGUAGE CPP #-}
{-
 - Author: Donnie Jones, Simon Marlow
 - Events.hs
 -   Parser functions for GHC RTS EventLog framework.
 -}
 
module GHC.RTS.Events where

{- Libraries. -}
import Data.Word (Word16, Word32, Word64)
import Debug.Trace
import Data.Binary
import Control.Monad

#define EVENTLOG_CONSTANTS_ONLY
#include "EventLogFormat.h"

{- Type synonyms. -}
type Filename = String

-- EventType.
type EventTypeNum = Word32
type EventTypeDescLen = Word32
type EventTypeDesc = String
-- Event.
type EventDescription = String
type Timestamp = Word64
type ThreadId = Word64

type Marker = Word32

{- Data Types. -}
data EventType = 
  EventType {
    etNum  :: EventTypeNum,
    etDesc :: EventTypeDesc,
    etSize :: Int
  } deriving Show

data Event = 
  Event {
    etRef :: EventTypeNum,
    ts    :: Timestamp,
    spec  :: EventTypeSpecificInfo
  } deriving Show

data EventTypeSpecificInfo
  = CreateThread   { cap :: Int, thread :: ThreadId  }
  | RunThread      { cap :: Int, thread :: ThreadId  }
  | StopThread     { cap :: Int, thread :: ThreadId, status :: ThreadStopStatus }
  | ThreadRunnable { cap :: Int, thread :: ThreadId  }
  | MigrateThread  { cap :: Int, thread :: ThreadId, newCap :: Int }
  | RunSpark       { cap :: Int, thread :: ThreadId  }
  | StealSpark     { cap :: Int, thread :: ThreadId, origCap :: Int }
  | WakeupThread   { cap :: Int, thread :: ThreadId, otherCap :: Int }
  | Shutdown       { cap :: Int }
  | RequestSeqGC   { cap :: Int }
  | RequestParGC   { cap :: Int }
  | StartGC        { cap :: Int }
  | EndGC          { cap :: Int }
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

newtype Header = Header EventTypes
     deriving Show

newtype EventTypes = EventTypes [EventType]
     deriving Show
  
------------------------------------------------------------------------------
-- Binary instances
instance Binary EventType where
  put (EventType etNum etDescLen etDesc) = do return () -- TODO.
  get = do etNum <- get 
           etDescLen <- get :: Get EventTypeDescLen
           etDesc <- getEtDesc (fromIntegral etDescLen) 
           etSize <- get :: Get Word16
           ete <- get :: Get Marker
           when (ete /= EVENT_ET_END) $
              error ("Event Type end marker not found.")
           return (EventType etNum etDesc (fromIntegral etSize))
           where 
             getEtDesc :: Int -> Get [Char]
             getEtDesc s = replicateM s (get :: Get Char)

instance Binary EventTypes where
  put (EventTypes ets) = do return () -- TODO.
  get = do hetm <- get :: Get Marker
           when (hetm /= EVENT_HET_BEGIN) $ 
                error "Header Event Type begin marker not found."
           EventTypes `fmap` getEventTypes
     where    
       getEventTypes :: Get [EventType]
       getEventTypes = do
           m <- get :: Get Marker
           case () of 
            _ | m == EVENT_ET_BEGIN -> do
                   et <- get :: Get EventType
                   nextET <- getEventTypes
                   return (et : nextET)
              | m == EVENT_HET_END ->
                   return []
              | otherwise ->
                   error "Malformed list of Event Types in header."

instance Binary Header where
  put (Header ets) = do return () -- TODO.
  get = do hdrb <- get :: Get Marker
           when (hdrb /= EVENT_HEADER_BEGIN) $ 
                error "Header begin marker not found."
           ets <- get
           emark <- get :: Get Marker
           when (emark /= EVENT_HEADER_END) $ 
                error "Header end marker not found."
           return (Header ets)

getEvent :: Get (Maybe Event)
getEvent = do 
  etRef <- get
  if (etRef == EVENT_DATA_END) 
     then return Nothing
     else do ts   <- get
             spec <- getEvSpecInfo etRef
             return (Just (Event etRef ts spec))
           
getEvSpecInfo :: EventTypeNum -> Get EventTypeSpecificInfo
getEvSpecInfo EVENT_CREATE_THREAD = do  -- (cap, thread)
  c  <- get :: Get Word16
  t <- get
  return CreateThread{cap=fromIntegral c,thread=t}
getEvSpecInfo EVENT_RUN_THREAD = do  -- (cap, thread)
  c  <- get :: Get Word16
  t <- get
  return RunThread{cap=fromIntegral c,thread=t}
getEvSpecInfo EVENT_STOP_THREAD = do  -- (cap, thread, status)
  c  <- get :: Get Word16
  t <- get
  s <- get :: Get Word16
  return StopThread{cap=fromIntegral c,
                    thread=t, 
                    status= toEnum (fromIntegral s)}
getEvSpecInfo EVENT_THREAD_RUNNABLE = do  -- (cap, thread)
  c  <- get :: Get Word16
  t <- get
  return ThreadRunnable{cap=fromIntegral c,thread=t}
getEvSpecInfo EVENT_MIGRATE_THREAD = do  -- (cap, thread, new_cap)
  c  <- get :: Get Word16
  t <- get
  c <- get :: Get Word16
  return MigrateThread{cap=fromIntegral c,thread=t,newCap=fromIntegral c}
getEvSpecInfo EVENT_RUN_SPARK = do  -- (cap, thread)
  c  <- get :: Get Word16
  t <- get
  return RunSpark{cap=fromIntegral c,thread=t}
getEvSpecInfo EVENT_STEAL_SPARK = do  -- (cap, thread, victim_cap)
  c  <- get :: Get Word16
  t <- get
  c <- get :: Get Word16
  return StealSpark{cap=fromIntegral c,thread=t,origCap=fromIntegral c}
getEvSpecInfo EVENT_SHUTDOWN = do  -- (cap)
  c  <- get :: Get Word16
  return Shutdown{cap=fromIntegral c}
getEvSpecInfo EVENT_THREAD_WAKEUP = do  -- (cap, thread, other_cap)
  c  <- get :: Get Word16
  t <- get
  c <- get :: Get Word16
  return WakeupThread{cap=fromIntegral c,thread=t,otherCap=fromIntegral c}
getEvSpecInfo EVENT_REQUEST_SEQ_GC = do  -- (cap)
  c  <- get :: Get Word16
  return RequestSeqGC{cap=fromIntegral c}
getEvSpecInfo EVENT_REQUEST_PAR_GC = do  -- (cap)
  c  <- get :: Get Word16
  return RequestParGC{cap=fromIntegral c}
getEvSpecInfo EVENT_GC_START = do  -- (cap)
  c  <- get :: Get Word16
  return StartGC{cap=fromIntegral c}
getEvSpecInfo EVENT_GC_END = do  -- (cap)
  c  <- get :: Get Word16
  return EndGC{cap=fromIntegral c}
getEvSpecInfo other = 
  error ("getEvSpecInfo: unknown event type: " ++ show other)

data Data = Data {
    events :: [Event]
  } deriving Show
          
instance Binary Data where
  put (Data { events = [] }) = do return () -- TODO.
  get = 
    do db <- get :: Get Marker 
       when (db /= EVENT_DATA_BEGIN) $ error "Data begin marker not found."
       getEvents [] 
    where
      getEvents :: [Event] -> Get Data
      getEvents events = 
        do mb_e <- getEvent
           case mb_e of
             Nothing -> return (Data events)
             Just e  -> getEvents (e:events)

-- Format defines the layout of the trace file.
data Format = 
  Format {
    fmtHeader :: Header,
    fmtData :: Data
  } deriving Show

instance Binary Format where
  put (Format hdr dat) = do return () -- TODO.
  get = do hdr <- get :: Get Header
           dat <- get :: Get Data
           return (Format hdr dat)

{- Functions. -}
buildFormat :: FilePath -> IO Format
buildFormat = decodeFile

{- EOF. -}
