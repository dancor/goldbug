module Opt where

import System.Console.GetOpt
import System.Environment

data Options = Options {
  optDb :: String,
  optExit :: Bool,
  optDbMode :: DbMode
  }

data DbMode = DbWhole | DbHalf | DbCorner | DbBigCorner

defOpts :: Options
defOpts = Options {
  optDb = "db",
  optExit = False,
  optDbMode = DbWhole
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "d" ["db"] (ReqArg (\ a o -> o {optDb = a}) "FILE")
    "Change database flie (./db is the default)",
  Option "X" ["exit-immediately"] (NoArg (\ o  -> o {optExit = True}))
    "For profiling, or adding games from a script or something.",
  Option "m" ["mode"] (ReqArg (\ a o -> o {optDbMode = case a of
      "whole" -> DbWhole
      "half" -> DbHalf
      "corner" -> DbCorner
      "big-corner" -> DbBigCorner
      _ -> error "mode not recognized"
    }) "whole|half|corner|big-corner")
    "Change the database lookup mode (default is whole board)."
  ]

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

