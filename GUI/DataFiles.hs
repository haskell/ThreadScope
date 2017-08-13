{-# LANGUAGE TemplateHaskell #-}
module GUI.DataFiles
  ( ui
  , loadLogo
  ) where
import System.IO

import Data.FileEmbed
import Graphics.UI.Gtk (Pixbuf, pixbufNewFromFile)
import Language.Haskell.TH
import System.IO.Temp
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

uiFile :: FilePath
uiFile = "threadscope.ui"

logoFile :: FilePath
logoFile = "threadscope.png"

-- | Textual representaion of the UI file
ui :: Q Exp
ui = [| TE.decodeUtf8 $(makeRelativeToProject uiFile >>= embedFile) |]

renderLogo :: B.ByteString -> IO Pixbuf
renderLogo bytes =
  withSystemTempFile logoFile $ \path h -> do
    B.hPut h bytes
    hClose h
    pixbufNewFromFile path

-- | Load the logo file as a 'Pixbuf'.
loadLogo :: Q Exp
loadLogo = [| renderLogo $(makeRelativeToProject logoFile >>= embedFile) |]
