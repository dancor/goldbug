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
  z <- zobristHashes
  (opts, sgfs) <- doArgs "usage" defOpts options
  let
    dbF = optDb opts
  db <- doesFileExist dbF >>= \ r -> if r
    then loadDb $ optDb opts
    else dbEmpty
  dbAddFiles z dbF sgfs db
  mainLoop z dbF 0 [] db

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

