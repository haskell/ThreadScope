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
       Timestamp,
       ThreadId,
       readEventLogFromFile,
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
           etDescLen <- getH :: GetHeader EventTypeDescLen
           etDesc <- getEtDesc (fromIntegral etDescLen) 
           size <- getH :: GetHeader EventTypeSize
           -- 0xffff indicates variable-sized event
           let etSize = if size == 0xffff then Nothing else Just size
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
     else do ts   <- getE
             spec <- getEvSpecInfo etRef
             return (Just (Event etRef ts spec))
           
getEvSpecInfo :: EventTypeNum -> GetEvents EventTypeSpecificInfo
getEvSpecInfo num = case num of

 EVENT_CREATE_THREAD -> do  -- (cap, thread)
  c  <- getE :: GetEvents Word16
  t <- getE
  return CreateThread{cap=fromIntegral c,thread=t}

 EVENT_RUN_THREAD -> do  --  (cap, thread)
  c  <- getE :: GetEvents Word16
  t <- getE
  return RunThread{cap=fromIntegral c,thread=t}

 EVENT_STOP_THREAD -> do  -- (cap, thread, status)
  c  <- getE :: GetEvents Word16
  t <- getE
  s <- getE :: GetEvents Word16
  return StopThread{cap=fromIntegral c,thread=t, status= toEnum (fromIntegral s)}

 EVENT_THREAD_RUNNABLE -> do  -- (cap, thread)
  c  <- getE :: GetEvents Word16
  t <- getE
  return ThreadRunnable{cap=fromIntegral c,thread=t}

 EVENT_MIGRATE_THREAD -> do  --  (cap, thread, new_cap)
  c  <- getE :: GetEvents Word16
  t <- getE
  c <- getE :: GetEvents Word16
  return MigrateThread{cap=fromIntegral c,thread=t,newCap=fromIntegral c}

 EVENT_RUN_SPARK -> do  -- (cap, thread)
  c  <- getE :: GetEvents Word16
  t <- getE
  return RunSpark{cap=fromIntegral c,thread=t}

 EVENT_STEAL_SPARK -> do  -- (cap, thread, victim_cap)
  c  <- getE :: GetEvents Word16
  t <- getE
  c <- getE :: GetEvents Word16
  return StealSpark{cap=fromIntegral c,thread=t,origCap=fromIntegral c}

 EVENT_SHUTDOWN -> do  -- (cap)
  c  <- getE :: GetEvents Word16
  return Shutdown{cap=fromIntegral c}

 EVENT_THREAD_WAKEUP -> do  -- (cap, thread, other_cap)
  c  <- getE :: GetEvents Word16
  t <- getE
  c <- getE :: GetEvents Word16
  return WakeupThread{cap=fromIntegral c,thread=t,otherCap=fromIntegral c}

 EVENT_REQUEST_SEQ_GC -> do  -- (cap)
  c  <- getE :: GetEvents Word16
  return RequestSeqGC{cap=fromIntegral c}

 EVENT_REQUEST_PAR_GC -> do  -- (cap)
  c  <- getE :: GetEvents Word16
  return RequestParGC{cap=fromIntegral c}

 EVENT_GC_START -> do  -- (cap)
  c  <- getE :: GetEvents Word16
  return StartGC{cap=fromIntegral c}

 EVENT_GC_END -> do  -- (cap)
  c  <- getE :: GetEvents Word16
  return EndGC{cap=fromIntegral c}

 other -> do -- unrecognised event, just skip it
  etypes <- ask
  case M.lookup (fromIntegral other) etypes of
    Nothing -> throwError ("getEvSpecInfo: undeclared event type: " ++ show other)
    Just t  -> do
      bytes <- case size t of
                 Just n  -> return n
                 Nothing -> getE :: GetEvents Word16
      skip  <- lift . lift $ replicateM_ (fromIntegral bytes) getWord8
      return UnknownEvent


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
                  Nothing -> return (Data events)
                  Just e  -> getEvents (e:events)

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
