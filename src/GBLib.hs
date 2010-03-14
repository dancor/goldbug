module GBLib where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Either
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe
import Data.Ord
import Text.Printf
import System.FilePath.Glob
import System.Random.Mersenne
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntSet as IntSet
import qualified Data.Judy as J

import Sgf

-- less important for opening study, but should eventually implement captures
-- for the position hashing.

-- should eventually make the database consistently 32bit even on 64bit
-- machines?

type Db = (IntSet, (J.JudyL Int, J.JudyL Int))

type Hash = Word

-- zobrist hash table
type Zobs = [[[Hash]]]

mainLoop :: Zobs -> String -> Int -> [Mv] -> Db -> IO ()
mainLoop z dbF reqNGms mvs db = do
  let
    tryMove mv = (,) mv <$> posInfo (posHash z $ mvs ++ [mv]) db
    wtf = putStrLn "could not parse move" >> mainLoop z dbF reqNGms mvs db
  gs <- posInfo (posHash z mvs) db
  rs <- unlines .
    map (\ (s, r) -> s ++ " " ++ showWinLoss r) .
    reverse .
    sortBy (comparing $ (\ (w, l) -> w + l) . snd) .
    map (first showMv) .
    filter ((\ (wWin, bWin) -> wWin + bWin >= reqNGms) . snd) .
    filter ((/= (0, 0)) . snd) .
    filter ((`notElem` mvs) . fst) <$> mapM tryMove
    [(c, (x, y)) | c <- [Black, White], x <- [0..18], y <- [0..18]]
  putStrLn ""
  putStrLn $ showWinLoss gs
  putStr rs
  nextMvS <- getLine
  case nextMvS of
    "q" -> return ()
    "r" -> mainLoop z dbF reqNGms [] db
    "u" -> mainLoop z dbF reqNGms (init mvs) db
    'l':' ':ptnS -> do
      sgfs <- nub . concat <$> mapM globSane (words ptnS)
      db' <- dbAddFiles z sgfs db
      saveDb dbF db'
      mainLoop z dbF reqNGms mvs db'
    'c':' ':reqNGmsS -> case readMb reqNGmsS of
      Just reqNGms' -> mainLoop z dbF reqNGms' mvs db
      Nothing -> wtf
    'h':_ -> do
      putStrLn "q - quit\n\
\r - reset to empty board\n\
\u - undo last move\n\
\l - load more sgf files (space-delimited glob patterns)\n\
\c - count-cutoff: only show next-moves with at least this many games"
      mainLoop z dbF reqNGms mvs db
    _ -> case readMv nextMvS of
      Just nextMv -> mainLoop z dbF reqNGms (mvs ++ [nextMv]) db
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

loadDb :: String -> IO Db
loadDb f = do
  putStrLn "loading database.."
  db <- secondM (bothondM jEncode) =<< decodeFile f
  summary <- dbSummary db
  putStrLn $ "loaded database (" ++ summary ++ ")"
  return db

firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM = runKleisli . first . Kleisli

secondM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
secondM = runKleisli . second . Kleisli

bothond :: (a -> b) -> (a, a) -> (b, b)
bothond f (x, y) = (f x, f y)

bothondM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
bothondM f (x, y) = liftM2 (,) (f x) (f y)

-- todo: something better by modifying judy package?
-- also, looks like cut and paste job on keys vs elems in judy package?
-- todo: not just Int?
jDecode :: J.JudyL Int -> IO [(Word, Int)]
jDecode j = liftM2 zip (J.keys j) (J.elems j)

jEncode :: [(Word, Int)] -> IO (J.JudyL Int)
jEncode l = do
  j <- J.new
  mapM_ (\ (k, v) -> J.insert k v j) l
  return j

saveDb :: String -> Db -> IO ()
saveDb dbF db = do
  summary <- dbSummary db
  putStrLn $ "saving database (" ++ summary ++ ").."
  encodeFile dbF =<< secondM (bothondM jDecode) db

dbAddFiles :: Zobs -> [String] -> Db -> IO Db
dbAddFiles z sgfs db = do
  putStrLn $ "adding " ++ show (length sgfs) ++ " sgf files to database.."
  foldM (flip $ dbAddFile z) db sgfs

dbSummary :: Db -> IO String
dbSummary db = do
  (bWin, wWin) <- posInfo 0 db
  return $ show (bWin + wWin) ++ " games"

dbAddFile :: Zobs -> String -> Db -> IO Db
dbAddFile z f db = do
  --c <- BS.unpack <$> BS.readFile f
  c <- readFile f
  case (parseSgf f c :: Either String [Game]) of
    Left e -> error e
    Right games -> foldM (flip $ dbAddGame z) db games
  {-
  case parseSgf f c of
    Left e -> error $ show e
    Right (games, _) -> foldM (flip $ dbAddGame z) db games
  -}

jInsertWith :: (Int -> Int) -> Word -> Int -> J.JudyL Int -> IO ()
jInsertWith f k v j = do
  vOldMb <- J.lookup k j
  let
    vNew = case vOldMb of
      Nothing -> v
      Just vOld -> f vOld
  J.insert k vNew j

dbAddGame :: Zobs -> Game -> Db -> IO Db
dbAddGame z g orig@(gameIdx, posToGame) =
  if IntSet.member gH gameIdx
    then return orig
    else do
      (\ j -> mapM_ (\ k -> jInsertWith (+ 1) k 1 j) pH) $ insF posToGame
      return (IntSet.insert gH gameIdx, posToGame)
  where
  pH = posHashes z $ gameMoves g
  -- FIXME?: look into why doesn't last work?  sum might be no slower anyway..
  --gH = last pH
  gH = fromIntegral $ sum pH
  insF = case gameWinner g of
    Black -> fst
    White -> snd

dbEmpty :: IO Db
dbEmpty = do
  j1 <- J.new
  j2 <- J.new
  return (IntSet.empty, (j1, j2))

showWinLoss :: (Int, Int) -> String
showWinLoss r@(w, l) = show r ++ " " ++
  printf "%.2f" (100 * fI w / (fI w + fI l) :: Float)

showMv :: Mv -> String
showMv (c, (x, y)) = cS ++ xS ++ yS
  where
  cS = case c of
    Black -> "b"
    White -> "w"
  xS = [chr (ord 'A' + fI x + if x >= 8 then 1 else 0)]
  yS = show (19 - y)

readMv :: String -> Maybe Mv
readMv (cS:xS:yS) = do
  c <- case cS of
    'b' -> Just Black
    'w' -> Just White
    _ -> Nothing
  let
    x = fI $ (\ n -> if n > 8 then n - 1 else n) $ ord xS - ord 'A'
    y = 19 - read yS
  Just (c, (x, y))
readMv _ = Nothing

posInfo :: Hash -> Db -> IO (Int, Int)
posInfo p (_, (bWin, wWin)) = liftM2 (,)
  (fromMaybe 0 <$> J.lookup p bWin)
  (fromMaybe 0 <$> J.lookup p wWin)

gameMoves :: Game -> [Mv]
gameMoves = snd
{-
gameMoves g = catMaybes .
  map (sndPullMb . second noPasses . fromJust . move) . rights . map action $
  mainPath t
  where
  TreeGo t = tree g
-}

gameWinner :: Game -> Color
gameWinner = fst
{-
gameWinner g = c
  where
  Win c _ =
    head . catMaybes . map result . catMaybes . map gameInfo $ mainPath t
  TreeGo t = tree g
-}

sndPullMb :: (t, Maybe t1) -> Maybe (t, t1)
sndPullMb (a, Just b) = Just (a, b)
sndPullMb _ = Nothing

{-
noPasses :: MoveGo -> Maybe Point
noPasses Pass = Nothing
noPasses (Play a) = Just a

mainPath :: Tree a -> [a]
mainPath t = (rootLabel t :) $ case subForest t of
  [] -> []
  subT:_ -> mainPath subT
-}

posHash :: Zobs -> [Mv] -> Hash
posHash z = last . posHashes z

posHashes :: Zobs -> [Mv] -> [Hash]
posHashes z = scanl xor 0 .
  map (\ (c, (x, y)) -> z !! fI x !! fI y !! fromEnum c)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

zobristHashes :: IO [[[Hash]]]
zobristHashes = do
  g <- newMTGen (Just 2457193872)
  stream2d . splitN 2 <$> randoms g

stream2d :: [a] -> [[a]]
stream2d s =
  [map (s !!)
    ([2*t..2*t+r-1] ++ map (r-1+) (drop r sqs)) | (r, t) <- zip [1..] tris]

sqs :: [Int]
sqs = [n*n | n <- [0..]]

tris :: [Int]
tris = [n*(n+1)`div`2 | n <- [0..]]

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = l : splitN n r
  where
  (l, r) = splitAt n xs
