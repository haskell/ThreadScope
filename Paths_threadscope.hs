module Paths_threadscope where

import Data.Version

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

version :: Version
version = read "0.2.7"