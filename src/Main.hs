module Main where

import Data.Binary
import GBLib
import System.Console.GetOpt
import System.Directory
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

main :: IO ()
main = do
  (opts, sgfs) <- doArgs "usage" defOpts options
  let
    dbF = optDb opts
  case sgfs of
    [] -> do
      db <- loadDb $ optDb opts
      mainLoop dbF 0 [] db
    _ -> do
      db <- doesFileExist dbF >>= \ r -> if r
        then loadDb $ optDb opts
        else return dbEmpty
      db' <- dbAddFiles sgfs db
      saveDb dbF db'

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

