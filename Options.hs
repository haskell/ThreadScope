module Options
where

-------------------------------------------------------------------------------

data Option
  = Debug
  | Filename String
  | TestTrace String
    deriving Eq

-------------------------------------------------------------------------------

parseOptions :: [String] -> [Option]
parseOptions [] = []
parseOptions ("--debug":rest)
  = Debug : parseOptions rest
parseOptions ("--test":rest)
  = if rest == [] then
      error ("--test needs an argument")
    else
      TestTrace (head rest) : parseOptions (tail rest)
parseOptions (filename:rest)
  = Filename filename : parseOptions rest

-----------------------------------------------------------------------------
