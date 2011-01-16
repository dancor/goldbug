module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import System.Directory
import System.FilePath
import System.FilePath.Glob
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser

import Anal
import Color
import Db (Db(..), Wins, posInfo, PosInfo(..))
import DbIO
import Game
import Move (Move(..), Pt2(..))
import Opt
import Zob
import qualified Db
import qualified Move

main :: IO ()
main = do
  (opts, sgfs) <- doArgs "usage" defOpts options
  let dbPath = optDb opts
  db <- doesFileExist (dbPath </> gamesFile) >>= \ r -> if r
    then 
      loadDb (optDb opts) $ 
        if null sgfs then [optDbPart opts] else Db.dbPartList
    else return Db.empty
  db' <- if null sgfs then return db else dbAddFiles zob dbPath sgfs db
  unless (optExit opts) $ if optGenTree opts
    then error "todo" --genTree opts
    else mainLoop opts zob [] db'

{-
genTree opts =
  if null possMvInfos
    then []
    else bestMvHere : genTree 
  where
  possMvInfos = getPossMvInfos opts zob ?db? mvs
  bestMvHere = fst $ bestMove 0.05 possMvInfos
-}

pTotalGames :: PosInfo -> Int
pTotalGames (PosInfo b w) = b + w

getPossMvInfos :: Options -> Zob -> Db -> [Move] -> [(Move, PosInfo)]
getPossMvInfos opts z db mvs = 
  sortBy (flip . comparing $ pTotalGames . snd) .
  filter ((>= optReqNGames opts) . pTotalGames . snd) . 
  filter ((/= PosInfo 0 0) . snd) .
  map (tryMove z (optDbPart opts) db mvs) $ filter (`notElem` mvs) Move.all

tryMove :: Zob -> Db.DbPart -> Db -> [Move] -> Move -> (Move, PosInfo)
tryMove z dbPart db mvs mv = 
  (mv, posInfo dbPart (posHash z $ mvs ++ [mv]) db)

mainLoop :: Options -> Zob -> [Move] -> Db -> IO ()
mainLoop opts z mvs db = do
  let
    asYouWere = mainLoop opts z mvs db
    wtf = putStrLn "could not parse move" >> asYouWere
    curPosInfo = posInfo (optDbPart opts) (posHash z mvs) db
    possMvInfos = getPossMvInfos opts z db mvs
    rs = unlines . map (\ (s, r) -> s ++ " " ++ showPosInfo r) $
      map (first Move.pretty) possMvInfos
    bestMvHere = fst $ bestMove 0.05 possMvInfos
  putStrLn ""
  putStrLn $ showPosInfo curPosInfo
  putStr rs
  unless (null possMvInfos) . putStrLn $
    "best mv: " ++ Move.pretty bestMvHere
  nextMvS <- getLine
  case nextMvS of
    "q" -> return ()
    "r" -> mainLoop opts z [] db
    "u" -> mainLoop opts z (if null mvs then [] else init mvs) db
    'l':' ':ptnS -> do
      sgfs <- nub . concat <$> mapM namesMatching (words ptnS)
      db' <- dbAddFiles z (optDb opts) sgfs db
      mainLoop opts z mvs db'
    'c':' ':reqNGamesS -> case readMb reqNGamesS of
      Just g -> mainLoop (opts {optReqNGames = g}) z mvs db
      Nothing -> wtf
    "b" -> case possMvInfos of
      [] -> putStrLn "no moves" >> asYouWere
      _ -> mainLoop opts z (mvs ++ [bestMvHere]) db
    'h':_ -> do
      putStr "q - quit\n\
\r - reset to empty board\n\
\u - undo last move\n\
\l - load more sgf files (space-delimited glob patterns)\n\
\c - count-cutoff: only show next-moves with at least this many games\n\
\b - best-move: most common move not statistically worse than another\n"
      asYouWere
    _ -> case Move.read nextMvS of
      Just nextMv -> mainLoop opts z (mvs ++ [nextMv]) db
      Nothing -> wtf

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s
