module GUI.Utils ( withBackgroundProcessing ) where

import Graphics.UI.Gtk
import Control.Exception
import Control.Concurrent

-- Causes the gtk main loop to yield to other Haskell threads whenever
-- it is idle.  This should be used only when there is
-- compute-intensive activity going on in other threads.
withBackgroundProcessing :: IO a -> IO a
withBackgroundProcessing f =
  bracket
    (idleAdd (yield >> return True) priorityDefaultIdle)
    idleRemove
    (\_ -> f)
