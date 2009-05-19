module Options
where

-------------------------------------------------------------------------------

data Option
  = Debug
  | Filename String
    deriving Eq

-------------------------------------------------------------------------------

parseOptions :: [String] -> [Option]
parseOptions [] = []
parseOptions ("--debug":rest)
  = Debug : parseOptions rest
parseOptions (filename:rest)
  = Filename filename : parseOptions rest

-----------------------------------------------------------------------------
