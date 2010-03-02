module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Bits
import Data.Char
import Data.Either
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe
import Data.Ord
import Text.Printf
import Data.SGF
-- import SGFBinary
import SGFBinaryColor
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Random
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

-- less important for opening study, but should eventually implement captures
-- for the position hashing.

-- games by hash, map of each position to games it was in
--type Db = (IntMap Game, IntMap IntSet)
-- just record game winner for now (cur game binary is large and hacky)
--type Db = (IntMap Color, IntMap IntSet)
--type Db = (IntMap Color, IntMap (Int, Int))
-- just store what we need actually
type Db = (IntSet, IntMap (Int, Int))

type Hash = Int

type Mv = (Color, Point)

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
  case sgfs of
    [] -> do
      db <- loadDb $ optDb opts
      mainLoop [] db
    _ -> do
      db <- doesFileExist (optDb opts) >>= \ r -> if r
        then loadDb $ optDb opts
        else return dbEmpty
      putStrLn "adding sgf files to database.."
      db' <- foldM (flip dbAddFile) db sgfs
      putStrLn "saving database.."
      encodeFile (optDb opts) $! db'

mainLoop :: [Mv] -> Db -> IO ()
mainLoop mvs db = do
  let
    gs = posInfo (posHash mvs) db
    tryMove mv = (mv, posInfo (posHash $ mvs ++ [mv]) db)
    rs = unlines .
      map (\ (s, r) -> s ++ " " ++ showWinLoss r) .
      reverse .
      sortBy (comparing $ (\ (w, l) -> w + l) . snd) .
      map (first showMv) . filter ((/= (0, 0)) . snd) $
      filter ((`notElem` mvs) . fst)
      [tryMove (c, (x, y)) | c <- [Black, White], x <- [0..18], y <- [0..18]]
  putStrLn ""
  putStrLn $ showWinLoss gs
  putStr rs
  nextMvS <- getLine
  case nextMvS of
    "q" -> return ()
    "r" -> mainLoop [] db
    "u" -> mainLoop (init mvs) db
    _ -> mainLoop (mvs ++ [readMv nextMvS]) db

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

readMv :: String -> Mv
readMv (cS:xS:yS) = (c, (x, y))
  where
  c = case cS of
    'b' -> Black
    'w' -> White
  x = fI $ (\ n -> if n > 8 then n - 1 else n) $ ord xS - ord 'A'
  y = 19 - read yS

loadDb :: String -> IO Db
loadDb = (putStrLn "loading database.." >>) . decodeFile

doArgs :: String -> c -> [OptDescr (c -> c)] -> IO (c, [String])
doArgs header defOpts options = do
  args <- getArgs
  return $ case getOpt Permute options args of
    (o, n, []) -> (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $ concat errs ++ usageInfo header options

posInfo :: Hash -> Db -> (Int, Int)
posInfo p (_, db) = fromMaybe (0, 0) $ IntMap.lookup p db

dbAddFile :: String -> Db -> IO Db
dbAddFile f db = do
  c <- BS.unpack <$> BS.readFile f
  --print $ posHashes . gameMoves . head . fst <$> runParser collection () f c
  return $ case runParser collection () f c of
    Left e -> error $ show e
    Right (games, _) -> foldl' (flip dbAddGame) db games

dbAddGame :: Game -> Db -> Db
dbAddGame g orig@(gameIdx, posToGame) =
  if IntSet.member gH gameIdx
    then orig
    else
      --(IntMap.insert gH g gameIdx, foldr ins posToGame pH)
      --(IntMap.insert gH (gameWinner g) gameIdx, foldr ins posToGame pH)
      (IntSet.insert gH gameIdx, foldr ins posToGame pH)
  where
  pH = posHashes $ gameMoves g
  -- FIXME?: look into why doesn't last work?  sum might be no slower anyway..
  --gH = last pH
  gH = sum pH
  ins k = IntMap.insertWith pairAdd k $ case gameWinner g of
    Black -> (1, 0)
    White -> (0, 1)
  pairAdd (a, b) (c, d) = (a + c, b + d)

dbEmpty :: Db
dbEmpty = (IntSet.empty, IntMap.empty)

gameMoves :: Game -> [Mv]
gameMoves g = catMaybes .
  map (sndPullMb . second noPasses . fromJust . move) . rights . map action $
  mainPath t
  where
  TreeGo t = tree g

gameWinner :: Game -> Color
gameWinner g = c
  where
  Win c _ =
    head . catMaybes . map result . catMaybes . map gameInfo $ mainPath t
  TreeGo t = tree g

sndPullMb :: (t, Maybe t1) -> Maybe (t, t1)
sndPullMb (a, Just b) = Just (a, b)
sndPullMb _ = Nothing

noPasses :: MoveGo -> Maybe Point
noPasses Pass = Nothing
noPasses (Play a) = Just a

mainPath :: Tree a -> [a]
mainPath t = (rootLabel t :) $ case subForest t of
  [] -> []
  subT:_ -> mainPath subT

posHash = last . posHashes

posHashes :: [Mv] -> [Hash]
posHashes = scanl xor 0 .
  map (\ (c, (x, y)) -> zobristHashes !! fI x !! fI y !! fromEnum c)

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

zobristHashes :: [[[Hash]]]
zobristHashes = stream2d . splitN 2 . randoms $ mkStdGen 26150218091920

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
