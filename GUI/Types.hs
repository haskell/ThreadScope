module GUI.Types (
    ViewParameters(..),
    Trace(..),
    Timestamp
  ) where

import GHC.RTS.Events

-----------------------------------------------------------------------------

data Trace
  = TraceHEC      Int
  | SparkCreationHEC Int
  | SparkConversionHEC Int
  | SparkPoolHEC  Int
  | TraceThread   ThreadId
  | TraceGroup    String
  | TraceActivity
  -- more later ...
  deriving Eq

-- the parameters for a timeline render; used to figure out whether
-- we're drawing the same thing twice.
data ViewParameters = ViewParameters {
    width, height :: Int,
    viewTraces    :: [Trace],
    hadjValue     :: Double,
    scaleValue    :: Double,
    detail        :: Int,
    bwMode, labelsMode :: Bool
  }
  deriving Eq
