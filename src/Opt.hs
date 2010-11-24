module Opt where

import System.Console.GetOpt
import System.Environment

data Options = Options {
  optDb :: String,
  optExit :: Bool
  }

defOpts :: Options
defOpts = Options {
  optDb = "db",
  optExit = False
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "d" ["db"] (ReqArg (\ a o -> o {optDb = a}) "FILE")
    "Change database flie (./db is the default)",
  Option "X" ["exit-immediately"] (NoArg (\ o  -> o {optExit = True}))
    "For profiling, or adding games from a script or something."
  ]

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

