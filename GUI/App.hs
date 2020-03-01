-------------------------------------------------------------------------------
-- | Module : GUI.App
--
-- Platform-specific application functionality
-------------------------------------------------------------------------------

module GUI.App (initApp) where

-------------------------------------------------------------------------------

-- | Initialize application
-- Perform application initialization for non-Mac OS X platforms
initApp :: IO ()
initApp = return ()
