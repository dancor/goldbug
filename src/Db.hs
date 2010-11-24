{-# LANGUAGE TemplateHaskell #-}

module Db (Db(..), addGame, hasGame, empty) where

import Data.DeriveTH
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List
import Data.Serialize
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Color
import Game
import Zob

data Db = Db {
  games :: !IntSet,
  posWins :: IntMap (Int, Int)
  }

addGame :: Zob -> Game -> Db -> Db
addGame zob gm db = db {
  games = IntSet.insert (fromIntegral gmHash) (games db),
  posWins = foldl' (\ m h -> case winner gm of
    Black -> IntMap.insertWith (\ _ (b, w) -> (b + 1, w)) h (1, 0) m
    White -> IntMap.insertWith (\ _ (b, w) -> (b, w + 1)) h (0, 1) m
    ) (posWins db) (map fromIntegral gmPosHashes)
  }
  where
  gmPosHashes = gameHashes zob gm
  gmHash = last gmPosHashes

hasGame :: Zob -> Game -> Db -> Bool
hasGame zob gm db = IntSet.member 
  (fromIntegral . last $ gameHashes zob gm)
  (games db)

empty :: Db
empty = Db {games = IntSet.empty, posWins = IntMap.empty}

$(derive makeSerialize ''Db)
