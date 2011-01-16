module Opt where

import Db
import System.Console.GetOpt
import System.Environment
import qualified Data.Set as S

data Options = Options {
  optDb :: String,
  optExit :: Bool,
  optDbPart :: DbPart,
  optGenTree :: Bool,
  optReqNGames :: Int
  }

defOpts :: Options
defOpts = Options {
  optDb = "db",
  optExit = False,
  optDbPart = DbWhole,
  optGenTree = False,
  optReqNGames = 0
  }

options :: [OptDescr (Options -> Options)]
options = [
  Option "d" ["db"] (ReqArg (\ a o -> o {optDb = a}) "FILE")
    "Change database flie (./db is the default)",
  Option "X" ["exit-immediately"] (NoArg (\ o  -> o {optExit = True}))
    "For profiling, adding games from a script, etc.",
  Option "m" ["mode"] (ReqArg (\ a o -> o {optDbPart = case a of
      "whole" -> DbWhole
      "half" -> DbHalf
      "corner" -> DbCorner
      "big-corner" -> DbBigCorner
      _ -> error "mode not recognized"
    }) "MODE")
    "Change the database lookup mode (whole\n\
    \  (default), half, corner, big-corner).",
  Option "t" ["gen-tree"] (NoArg (\ o -> o {optGenTree = True}))
    "Generate a tree of best moves and common\n\
    \  responses.",
  Option "g" ["require-n-games"] 
    (ReqArg (\ a o -> o {optReqNGames = read a}) "N")
    "Only consider moves that occur in at least N\n\
    \  games."
  ]

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options
