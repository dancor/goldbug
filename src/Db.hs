{-# LANGUAGE TemplateHaskell #-}

module Db where

import Control.Arrow
import Data.DeriveTH
import Data.List
import Data.Maybe
import Data.Serialize
import System.FilePath
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Color
import Game
import Move
import Zob

data Wins = Wins {
  bWins :: IM.IntMap Int,
  wWins :: IM.IntMap Int
  }

data DbPart = 
  DbWhole  |  -- 19x19
  DbHalf   |  -- 19x11
  DbCorner |  -- 9x9
  DbBigCorner -- 12x12
  deriving (Enum, Eq, Ord)

data Db = Db {
  games :: !IS.IntSet,
  wins :: M.Map DbPart Wins
  }

data PosInfo = PosInfo {
  pBWins :: !Int,
  pWWins :: !Int
  }
  deriving (Eq)

bWinsDo, wWinsDo :: (IM.IntMap Int -> IM.IntMap Int) -> Wins -> Wins
bWinsDo f w = w {bWins = f $ bWins w}
wWinsDo f w = w {wWins = f $ wWins w}

addGame :: Zob -> Game -> Db -> Db
addGame zob gm db = if isPartialLoad db
  then 
    error "Cannot add game to partially-loaded database!"
  else 
    db {
      games = IS.insert (fromIntegral gmHash) (games db),
      wins = M.differenceWith insHashes (wins db) . M.fromList $ zip dbPartList
        [wholeHashes, halfHashes, cornerHashes, bigCornerHashes]
      }
  where
  insHashes :: Wins -> [Hash] -> Maybe Wins
  insHashes w hs = Just $ insF 
    (\ winMap -> foldl' insHash winMap $ map fromIntegral hs)
    w
  insHash m h = IM.insertWith (\ _ n -> n + 1) h 1 m
  insF = case winner gm of
    Black -> bWinsDo
    White -> wWinsDo
  mvs = moves gm
  nMax = 18
  nMid = nMax `div` 2
  nBig = 12
  wholeHashes = posHashes zob mvs
  halfHashes = concatMap (posHashes zob) [
    filter (\ (Move _ (Pt2 _ y)) -> y <= nMid + 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 x (nMax - y))) $
      filter (\ (Move _ (Pt2 _ y)) -> y >= nMid - 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 y x)) $
      filter (\ (Move _ (Pt2 x _)) -> x <= nMid + 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 y (nMax - x))) $
      filter (\ (Move _ (Pt2 x _)) -> x >= nMid - 1) mvs
    ]
  cornerHashes = concatMap (posHashes zob) [
    filter (\ (Move _ (Pt2 x y)) -> x < nMid && y < nMid) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 (nMax - x) y)) $
      filter (\ (Move _ (Pt2 x y)) -> x > nMid && y < nMid) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 x (nMax - y))) $
      filter (\ (Move _ (Pt2 x y)) -> x < nMid && y > nMid) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 (nMax - x) (nMax - y))) $
      filter (\ (Move _ (Pt2 x y)) -> x > nMid && y > nMid) mvs
    ]
  bigCornerHashes = concatMap (posHashes zob) [
    filter (\ (Move _ (Pt2 x y)) -> x < nBig && y < nBig) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 (nMax - x) y)) $
      filter (\ (Move _ (Pt2 x y)) -> x > nBig && y < nBig) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 x (nMax - y))) $
      filter (\ (Move _ (Pt2 x y)) -> x < nBig && y > nBig) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 (nMax - x) (nMax - y))) $
      filter (\ (Move _ (Pt2 x y)) -> x > nBig && y > nBig) mvs
    ]
  gmHash = last wholeHashes

isPartialLoad :: Db -> Bool
isPartialLoad db = sort (M.keys $ wins db) /= sort dbPartList

dbPartList :: [DbPart]
dbPartList = [DbWhole, DbHalf, DbCorner, DbBigCorner]

hasGame :: Zob -> Game -> Db -> Bool
hasGame zob gm db = IS.member 
  (fromIntegral . last . posHashes zob $ moves gm)
  (games db)

empty :: Db
empty = Db {
  games = IS.empty, 
  wins = M.fromList . zip dbPartList $ repeat winsEmpty
  }

winsEmpty :: Wins
winsEmpty = Wins IM.empty IM.empty

posInfo :: DbPart -> Hash -> Db -> PosInfo
posInfo p h = (\ (Wins b w) -> PosInfo (lk b) (lk w)) . 
  fromMaybe (error "Database part missing!") . M.lookup p . wins
  where
  lk = fromMaybe 0 . IM.lookup (fromIntegral h)

$(derive makeSerialize ''Wins)
