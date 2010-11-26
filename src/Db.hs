{-# LANGUAGE TemplateHaskell #-}

module Db where

import Control.Arrow
import Data.DeriveTH
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Maybe
import Data.Serialize
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Color
import Game
import Move
import Zob

type BWWins = (IntMap Int, IntMap Int)

data Db = Db {
  games :: !IntSet,
  -- 19x19
  wholeWins :: BWWins,
  -- 19x11
  halfWins :: BWWins,
  -- 9x9
  cornerWins :: BWWins,
  -- 12x12
  bigCornerWins :: BWWins
  }

addGame :: Zob -> Game -> Db -> Db
addGame zob gm db = db {
  games = IntSet.insert (fromIntegral gmHash) (games db),
  wholeWins = insF (\ winMap -> foldl' 
    (\ m h -> IntMap.insertWith (\ _ n -> n + 1) h 1 m) 
    winMap (map fromIntegral gmPosHashes)) (wholeWins db),
  halfWins = insF (\ winMap -> foldl' 
    (\ m h -> IntMap.insertWith (\ _ n -> n + 1) h 1 m) 
    winMap (map fromIntegral halfHs)) (halfWins db)
  }
  where
  insF = case winner gm of
    Black -> first
    White -> second
  mvs = moves gm
  nMax = 18
  nMid = nMax `div` 2
  halfHs = concatMap (posHashes zob) [
    filter (\ (Move _ (Pt2 _ y)) -> y <= nMid + 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 x (nMax - y))) $
      filter (\ (Move _ (Pt2 _ y)) -> y >= nMid - 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 y x)) $
      filter (\ (Move _ (Pt2 x _)) -> x <= nMid + 1) mvs,
    map (\ (Move c (Pt2 x y)) -> Move c (Pt2 y (nMax - x))) $
      filter (\ (Move _ (Pt2 x _)) -> x >= nMid - 1) mvs
    ]
  gmPosHashes = posHashes zob mvs
  gmHash = last gmPosHashes

hasGame :: Zob -> Game -> Db -> Bool
hasGame zob gm db = IntSet.member 
  (fromIntegral . last . posHashes zob $ moves gm)
  (games db)

empty :: Db
empty = Db {
  games = IntSet.empty, 
  wholeWins = bwEmpty,
  halfWins = bwEmpty,
  cornerWins = bwEmpty,
  bigCornerWins = bwEmpty
  }

bwEmpty = (IntMap.empty, IntMap.empty)

posInfo :: Hash -> BWWins -> (Int, Int)
posInfo h = 
  first (fromMaybe 0 . IntMap.lookup (fromIntegral h)) .
  second (fromMaybe 0 . IntMap.lookup (fromIntegral h))

$(derive makeSerialize ''Db)
