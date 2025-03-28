{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module GUI.DataFiles
  ( ui
  , loadLogo
  ) where
import Control.Exception (IOException, Handler(..), catches)
import System.IO

import Data.FileEmbed
import Graphics.UI.Gtk (Pixbuf, pixbufNewFromFile)
import Language.Haskell.TH
import System.Glib (GError)
import System.IO.Temp
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

uiFile :: FilePath
uiFile = "threadscope.ui"

logoFile :: FilePath
logoFile = "threadscope.png"



-- | Textual representation of the UI file
ui :: Q Exp
ui = [| TE.decodeUtf8 $(makeRelativeToProject uiFile >>= embedFile) |]




renderLogo :: B.ByteString -> IO (Maybe Pixbuf)
renderLogo bytes =
  withSystemTempFile logoFile $ \path h -> do
    B.hPut h bytes
    hClose h
    Just <$> pixbufNewFromFile path
  `catches`
    -- in case of a failure in the file IO or pixbufNewFromFile, return Nothing
    [ Handler $ \(_ :: IOException) -> return Nothing
    , Handler $ \(_ :: GError) -> return Nothing
    ]

-- | Load the logo file as a 'Pixbuf'.
loadLogo :: Q Exp
loadLogo = [| renderLogo $(makeRelativeToProject logoFile >>= embedFile) |]
