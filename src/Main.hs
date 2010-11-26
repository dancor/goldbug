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
import Db (Db(..), posInfo)
import Game
import Move (Move(..), Pt2(..))
import Opt
import Sgf
import Zob
import qualified Db
import qualified Move

main :: IO ()
main = do
  (opts, sgfs) <- doArgs "usage" defOpts options
  let
    dbF = optDb opts
  db <- doesFileExist dbF >>= \ r -> if r
    then loadDb $ optDb opts
    else return Db.empty
  db' <- if null sgfs then return db else dbAddFiles zob dbF sgfs db
  unless (optExit opts) $ mainLoop opts zob dbF 0 [] db'

mainLoop :: Options -> Zob -> FilePath -> Int -> [Move] -> Db -> IO ()
mainLoop opts zob dbF reqNGms mvs db = do
  let
    dbModeFunc = case optDbMode opts of
      DbWhole -> wholeWins
      DbHalf -> halfWins
      DbCorner -> cornerWins
      DbBigCorner -> bigCornerWins
    tryMove :: Move -> (Move, PosInfo)
    tryMove mv = (mv, posInfo (posHash zob $ mvs ++ [mv]) $ dbModeFunc db)
    asYouWere = mainLoop opts zob dbF reqNGms mvs db
    wtf = putStrLn "could not parse move" >> asYouWere
    curPosInfo = posInfo (posHash zob mvs) $ dbModeFunc db
    possMvInfos = sortBy (flip . comparing $ uncurry (+) . snd) .
      filter ((>= reqNGms) . uncurry (+) . snd) . filter ((/= (0, 0)) . snd) .
      map tryMove $ filter (`notElem` mvs) Move.all
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
    "r" -> mainLoop opts zob dbF reqNGms [] db
    "u" -> mainLoop opts zob dbF reqNGms (init mvs) db
    'l':' ':ptnS -> do
      sgfs <- nub . concat <$> mapM globSane (words ptnS)
      db' <- dbAddFiles zob dbF sgfs db
      mainLoop opts zob dbF reqNGms mvs db'
    'c':' ':reqNGmsS -> case readMb reqNGmsS of
      Just reqNGms' -> mainLoop opts zob dbF reqNGms' mvs db
      Nothing -> wtf
    "b" -> case possMvInfos of
      [] -> putStrLn "no moves" >> asYouWere
      _ -> mainLoop opts zob dbF reqNGms (mvs ++ [bestMvHere]) db
    'h':_ -> do
      putStr "q - quit\n\
\r - reset to empty board\n\
\u - undo last move\n\
\l - load more sgf files (space-delimited glob patterns)\n\
\c - count-cutoff: only show next-moves with at least this many games\n\
\b - best-move: most common move not statistically worse than another\n"
      asYouWere
    _ -> case Move.read nextMvS of
      Just nextMv -> mainLoop opts zob dbF reqNGms (mvs ++ [nextMv]) db
      Nothing -> wtf

readMb :: Read a => String -> Maybe a
readMb s = fmap fst . listToMaybe $ reads s

-- work nicely with both rooted "/a/*" and non-rooted "a/*" glob patterns
-- i believe this naive approach introduces some delay (~300ms?), because
-- globDir underneath computes all unmatched files along the way.  but good
-- enough for me for now.
globSane :: String -> IO [String]
globSane ('/':ptn) = globDir1 (compile ptn) "/"
globSane ptn = globDir1 (compile ptn) ""

loadDb :: FilePath -> IO Db
loadDb f = do
  putStrLn "loading database.."
  db <- either error id . Ser.decode <$> BS.readFile f
  putStrLn $ "loaded database (" ++ dbSummary db ++ ")"
  return db

saveDb :: String -> Db -> IO ()
saveDb dbF db = do
  putStrLn $ "saving database (" ++ dbSummary db ++ ").."
  BS.writeFile dbF (Ser.encode db)

sequenceM :: (Monad m) => [a -> m a] -> a -> m a
sequenceM = foldr (>=>) return

actEveryNGens :: (Monad m) => Int -> (a -> m a) -> a -> [a -> m (Int, a)] ->
  m (Int, a)
actEveryNGens n act a fs = sequenceM 
  (intersperse 
    (\ i@(m, b) -> if m >= n then liftM ((,) 0) $ act b else return i) $
    map (\ f (m, b) -> liftM (first (m +)) $ f b) fs)
  (0, a)

dbAddFiles :: Zob -> FilePath -> [FilePath] -> Db -> IO Db
dbAddFiles zob dbF fs db = do
  let l = length fs
  putStrLn $ "adding " ++ show l ++ " sgf files to database.."
  (lastAdded, db') <- actEveryNGens 1000 (\ d -> saveDb dbF d >> return d) db $
    zipWith (\ i f -> 
      (putStr (show i ++ "/" ++ show l ++ ": ") >>) . dbAddFile zob f)
      [1 ..] fs
  when (lastAdded > 0) $ saveDb dbF db'
  return db'

dbSummary :: Db -> String
dbSummary db = show (uncurry (+) $ posInfo 0 $ wholeWins db) ++ " games"

dbAddFile :: Zob -> FilePath -> Db -> IO (Int, Db)
dbAddFile zob f db = do
  putStrLn $ "adding: " ++ f
  c <- readFile f
  ((added, skipped), db') <- case parseSgf f c of
    Left e -> putStrLn ("skipping file with sgf error: " ++ show e) >> 
      return ((0, 0), db)
    Right games -> return $ foldr (dbAddGame zob) ((0, 0), db) games
  when (skipped > 0) .
    putStrLn $ "skipped " ++ show skipped ++ " game(s) already in db"
  return (added, db')

dbAddGame :: Zob -> Game -> ((Int, Int), Db) -> ((Int, Int), Db)
dbAddGame zob gm ((a, s), db) = if Db.hasGame zob gm db
  then ((a, s + 1), db)
  else ((a + 1, s), Db.addGame zob gm db)
