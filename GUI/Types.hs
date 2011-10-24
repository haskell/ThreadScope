module GUI.Types (
    ViewParameters(..),
    Trace(..),
    Timestamp,
    Interval,
  ) where

import GHC.RTS.Events

-----------------------------------------------------------------------------

data Trace
  = TraceHEC      Int
  | TraceInstantHEC Int
  | TraceCreationHEC Int
  | TraceConversionHEC Int
  | TracePoolHEC  Int
  | TraceHistogram
  | TraceGroup    String
  | TraceActivity
  -- more later ...
  --  | TraceThread   ThreadId
  deriving Eq

type Interval = (Timestamp, Timestamp)

-- the parameters for a timeline render; used to figure out whether
-- we're drawing the same thing twice.
data ViewParameters = ViewParameters {
    width, height :: Int,
    viewTraces    :: [Trace],
    hadjValue     :: Double,
    scaleValue    :: Double,
    maxSpkValue   :: Double,
    detail        :: Int,
    bwMode, labelsMode :: Bool,
    histogramHeight :: Int,
    minterval :: Maybe Interval,
    xScaleAreaHeight :: Int
  }
  deriving Eq
