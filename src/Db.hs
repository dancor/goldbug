{-# LANGUAGE TemplateHaskell #-}

module Db (Db(..), addGame, hasGame, empty) where

import Control.Arrow
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
  posWins :: (IntMap Int, IntMap Int)
  }

addGame :: Zob -> Game -> Db -> Db
addGame zob gm db = db {
  games = IntSet.insert (fromIntegral gmHash) (games db),
  posWins = (case winner gm of
    Black -> first
    White -> second)
    (\ winMap -> foldl' 
      (\ m h -> IntMap.insertWith (\ _ n -> n + 1) h 1 m) 
      winMap (map fromIntegral gmPosHashes)) (posWins db)
  }
  where
  gmPosHashes = gameHashes zob gm
  gmHash = last gmPosHashes

hasGame :: Zob -> Game -> Db -> Bool
hasGame zob gm db = IntSet.member 
  (fromIntegral . last $ gameHashes zob gm)
  (games db)

empty :: Db
empty = Db {games = IntSet.empty, posWins = (IntMap.empty, IntMap.empty)}

$(derive makeSerialize ''Db)
