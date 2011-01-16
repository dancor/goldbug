module DbIO where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Db
import Game
import Opt
import Sgf
import System.Directory
import System.FilePath
import Zob
import qualified Data.ByteString as BS
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Serialize as Ser

dbPartFile :: DbPart -> FilePath
dbPartFile DbWhole = "whole"
dbPartFile DbHalf = "half"
dbPartFile DbCorner = "corner"
dbPartFile DbBigCorner = "big-corner"

gamesFile :: FilePath
gamesFile = "games"

loadDb :: FilePath -> [DbPart] -> IO Db
loadDb dbPath dbP = do
  putStrLn "Loading database.."
  let 
    dec :: (Ser.Serialize a) => BS.ByteString -> a
    dec = either error id . Ser.decode
  db <- (\ (g:ps) -> Db (dec g) . M.fromList . zip dbP $ map dec ps) <$> 
    mapM (\ p -> BS.readFile $ dbPath </> p) (gamesFile : map dbPartFile dbP)
  putStrLn $ "Loaded database (" ++ dbSummary db ++ ")"
  return db

saveDb :: FilePath -> Db -> IO ()
saveDb dbPath db = do
  when (isPartialLoad db) $ error "Cannot save partially-loaded database!"
  putStrLn $ "Saving database (" ++ dbSummary db ++ ").."
  createDirectoryIfMissing False dbPath
  mapM_ (\ (p, i) -> BS.writeFile (dbPath </> p) i) $ 
    (gamesFile, Ser.encode $ games db) : 
    (map (first dbPartFile) . M.toList . M.map Ser.encode $ wins db)

dbAddFiles :: Zob -> FilePath -> [FilePath] -> Db -> IO Db
dbAddFiles z dbF fs db = do
  let l = length fs
  putStrLn $ "Adding " ++ show l ++ " sgf files to database.."
  (lastAdded, db') <- actEveryNGens 100000 
    (\ d -> saveDb dbF d >> return d) db $
    zipWith (\ i f -> 
      (putStr (show i ++ "/" ++ show l ++ ": ") >>) . dbAddFile z f)
      [1 ..] fs
  when (lastAdded > 0) $ saveDb dbF db'
  return db'

dbSummary :: Db -> String
dbSummary db = show (IS.size $ games db) ++ " games"

dbAddFile :: Zob -> FilePath -> Db -> IO (Int, Db)
dbAddFile z f db = do
  putStrLn $ "Adding: " ++ f
  c <- readFile f
  ((added, skipped), db') <- case parseSgf f c of
    Left e -> putStrLn ("Skipping file with sgf error: " ++ show e) >> 
      return ((0, 0), db)
    Right games -> return $ foldr (dbAddGame z) ((0, 0), db) games
  when (skipped > 0) .
    putStrLn $ "Skipped " ++ show skipped ++ " game(s) already in db"
  return (added, db')

dbAddGame :: Zob -> Game -> ((Int, Int), Db) -> ((Int, Int), Db)
dbAddGame z gm ((a, s), db) = if Db.hasGame z gm db
  then ((a, s + 1), db)
  else ((a + 1, s), Db.addGame z gm db)

sequenceM :: (Monad m) => [a -> m a] -> a -> m a
sequenceM = foldr (>=>) return

actEveryNGens :: (Monad m) => Int -> (a -> m a) -> a -> [a -> m (Int, a)] ->
  m (Int, a)
actEveryNGens n act a fs = sequenceM 
  (intersperse 
    (\ i@(m, b) -> if m >= n then liftM ((,) 0) $ act b else return i) $
    map (\ f (m, b) -> liftM (first (m +)) $ f b) fs)
  (0, a)
