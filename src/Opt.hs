module Opt where

import System.Console.GetOpt
import System.Environment

data Options = Options {
  optDb :: String}

defOpts :: Options
defOpts = Options {
  optDb = "db"}

options :: [OptDescr (Options -> Options)]
options = [
  Option "d" ["db"] (ReqArg (\ a o -> o {optDb = a}) "FILE")
    "Change database flie (./db is the default)"]

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

