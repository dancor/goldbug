module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Bits
import Data.Either
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe
import Data.SGF
import SGFBinary
import System.Random
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

-- less important for opening study, but should eventually implement captures
-- for the position hashing.

-- games by hash, map of each position to games it was in
type Db = (IntMap Game, IntMap IntSet)

type Hash = Int

main :: IO ()
main = do
  db <- dbAddFile "test.sgf" dbEmpty
  encodeFile "db" db
  putStrLn "hi"

dbAddFile :: String -> Db -> IO Db
dbAddFile f db = do
  c <- BS.unpack <$> BS.readFile f
  print $ posHashes . gameToMoves . head . fst <$> runParser collection () f c
  return $ case runParser collection () f c of
    Left e -> error $ show e
    Right (games, _) -> foldl' (flip dbAddGame) db games

dbAddGame :: Game -> Db -> Db
dbAddGame g (gameIdx, posToGame) = (gameIdx, posToGame)

dbEmpty :: Db
dbEmpty = (IntMap.empty, IntMap.empty)

gameToMoves :: Game -> [(Color, Point)]
gameToMoves g = catMaybes .
  map (sndPullMb . second noPasses . fromJust . move) . rights . map action $
  mainPath t
  where
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

posHashes :: [(Color, Point)] -> [Hash]
posHashes = scanl xor 0 .
  map (\ (c, (x, y)) -> zobristHashes !! fI x !! fI y !! fromEnum c)
  where
  fI = fromIntegral

zobristHashes :: [[[Hash]]]
zobristHashes = stream2d . splitN 2 . randoms $ mkStdGen 26150218091920

stream2d :: [a] -> [[a]]
stream2d s =
  [map (s !!)
    ([2*t..2*t+r-1] ++ map (r-1+) (drop r sqs)) | (r, t) <- zip [1..] tris]

sqs = [n*n | n <- [0..]]

tris = [n*(n+1)`div`2 | n <- [0..]]

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = l : splitN n r
  where
  (l, r) = splitAt n xs
