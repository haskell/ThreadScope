module Paths_threadscope where

import Data.Version

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

version :: Version
version = makeVersion [0,2,7]