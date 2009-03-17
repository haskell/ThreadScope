{-# LANGUAGE CPP #-}
{-
 - Author: Donnie Jones, Simon Marlow
 - Events.hs
 -   Parser functions for GHC RTS EventLog framework.
 -}
 
module GHC.RTS.Events (
       EventLog(..),
       EventType(..),
       Event(..),
       EventTypeSpecificInfo(..),
       ThreadStopStatus(..),
       Header(..),
       Data(..),
       readEventLogFromFile,
  ) where

{- Libraries. -}
import Data.Word (Word16, Word32, Word64)
import Debug.Trace
import Data.Binary
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Control.Monad.Reader

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
type ThreadId = Word64

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
    ref  :: EventTypeNum,
    time :: Timestamp,
    spec :: EventTypeSpecificInfo
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
  | UnknownEvent
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
type GetEventLog a = ReaderT (IntMap EventType) Get a
  
getT :: (MonadTrans m, Binary a) => m Get a
getT = lift get

------------------------------------------------------------------------------
-- Binary instances

instance Binary EventType where
  put (EventType etNum etDescLen etDesc) = do return () -- TODO.
  get = do etNum <- get 
           etDescLen <- get :: Get EventTypeDescLen
           etDesc <- getEtDesc (fromIntegral etDescLen) 
           size <- get :: Get EventTypeSize
           -- 0xffff indicates variable-sized event
           let etSize = if size == 0xffff then Nothing else Just size
           ete <- get :: Get Marker
           when (ete /= EVENT_ET_END) $
              error ("Event Type end marker not found.")
           return (EventType etNum etDesc etSize)
           where 
             getEtDesc :: Int -> Get [Char]
             getEtDesc s = replicateM s (get :: Get Char)

instance Binary Header where
  put (Header ets) = do return () -- TODO.
  get = do hdrb <- get :: Get Marker
           when (hdrb /= EVENT_HEADER_BEGIN) $
                error "Header begin marker not found."
           hetm <- get :: Get Marker
           when (hetm /= EVENT_HET_BEGIN) $ 
                error "Header Event Type begin marker not found."
           ets <- getEventTypes
           emark <- get :: Get Marker
           when (emark /= EVENT_HEADER_END) $
                error "Header end marker not found."
           return (Header ets)
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

getEvent :: GetEventLog (Maybe Event)
getEvent = do 
  etRef <- getT
  if (etRef == EVENT_DATA_END) 
     then return Nothing
     else do ts   <- getT
             spec <- getEvSpecInfo etRef
             return (Just (Event etRef ts spec))
           
getEvSpecInfo :: EventTypeNum -> GetEventLog EventTypeSpecificInfo
getEvSpecInfo num = case num of

 EVENT_CREATE_THREAD -> do  -- (cap, thread)
  c  <- getT :: GetEventLog Word16
  t <- getT
  return CreateThread{cap=fromIntegral c,thread=t}

 EVENT_RUN_THREAD -> do  --  (cap, thread)
  c  <- getT :: GetEventLog Word16
  t <- getT
  return RunThread{cap=fromIntegral c,thread=t}

 EVENT_STOP_THREAD -> do  -- (cap, thread, status)
  c  <- getT :: GetEventLog Word16
  t <- getT
  s <- getT :: GetEventLog Word16
  return StopThread{cap=fromIntegral c,thread=t, status= toEnum (fromIntegral s)}

 EVENT_THREAD_RUNNABLE -> do  -- (cap, thread)
  c  <- getT :: GetEventLog Word16
  t <- getT
  return ThreadRunnable{cap=fromIntegral c,thread=t}

 EVENT_MIGRATE_THREAD -> do  --  (cap, thread, new_cap)
  c  <- getT :: GetEventLog Word16
  t <- getT
  c <- getT :: GetEventLog Word16
  return MigrateThread{cap=fromIntegral c,thread=t,newCap=fromIntegral c}

 EVENT_RUN_SPARK -> do  -- (cap, thread)
  c  <- getT :: GetEventLog Word16
  t <- getT
  return RunSpark{cap=fromIntegral c,thread=t}

 EVENT_STEAL_SPARK -> do  -- (cap, thread, victim_cap)
  c  <- getT :: GetEventLog Word16
  t <- getT
  c <- getT :: GetEventLog Word16
  return StealSpark{cap=fromIntegral c,thread=t,origCap=fromIntegral c}

 EVENT_SHUTDOWN -> do  -- (cap)
  c  <- getT :: GetEventLog Word16
  return Shutdown{cap=fromIntegral c}

 EVENT_THREAD_WAKEUP -> do  -- (cap, thread, other_cap)
  c  <- getT :: GetEventLog Word16
  t <- getT
  c <- getT :: GetEventLog Word16
  return WakeupThread{cap=fromIntegral c,thread=t,otherCap=fromIntegral c}

 EVENT_REQUEST_SEQ_GC -> do  -- (cap)
  c  <- getT :: GetEventLog Word16
  return RequestSeqGC{cap=fromIntegral c}

 EVENT_REQUEST_PAR_GC -> do  -- (cap)
  c  <- getT :: GetEventLog Word16
  return RequestParGC{cap=fromIntegral c}

 EVENT_GC_START -> do  -- (cap)
  c  <- getT :: GetEventLog Word16
  return StartGC{cap=fromIntegral c}

 EVENT_GC_END -> do  -- (cap)
  c  <- getT :: GetEventLog Word16
  return EndGC{cap=fromIntegral c}

 other -> do -- unrecognised event, just skip it
  etypes <- ask
  case M.lookup (fromIntegral other) etypes of
    Nothing -> error ("getEvSpecInfo: undeclared event type: " ++ show other)
    Just t  -> do
      bytes <- case size t of
                 Just n  -> return n
                 Nothing -> getT :: GetEventLog Word16
      skip  <- lift $ replicateM_ (fromIntegral bytes) getWord8
      return UnknownEvent


getData :: GetEventLog Data
getData = do
   db <- getT :: GetEventLog Marker
   when (db /= EVENT_DATA_BEGIN) $ error "Data begin marker not found."
   getEvents []
   where
              getEvents :: [Event] -> GetEventLog Data
              getEvents events = do
                mb_e <- getEvent
                case mb_e of
                  Nothing -> return (Data events)
                  Just e  -> getEvents (e:events)

instance Binary EventLog where
  put = undefined -- Todo
  get = do
    header <- get :: Get Header
    let imap = M.fromList [ (fromIntegral (num t),t) | t <- eventTypes header]
    dat <- runReaderT getData imap
    return (EventLog header dat)

readEventLogFromFile :: FilePath -> IO EventLog
readEventLogFromFile = decodeFile

{- EOF. -}
