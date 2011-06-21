module GUI.Types (
    ViewParameters(..),
    Trace(..),
  ) where

import GHC.RTS.Events

-----------------------------------------------------------------------------

data Trace
  = TraceHEC      Int
  | SparksHEC     Int
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
