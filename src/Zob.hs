module Zob (module ZobTable, posHash, posHashes) where

import Data.Bits
import Data.Word
import Move
import ZobTable

posHash :: Zob -> [Move] -> Hash
posHash zob = last . posHashes zob

posHashes :: Zob -> [Move] -> [Hash]
posHashes zob mvs = minimum $ map (\ r -> scanl xor 0 $ map (zob . rot r) mvs)
  [0..7]

rot :: Int -> Move -> Move
rot 0 (Move c (Pt2 x y)) = Move c $ Pt2 x y
rot 1 (Move c (Pt2 x y)) = Move c $ Pt2 (n - x) y
rot 2 (Move c (Pt2 x y)) = Move c $ Pt2 x (n - y)
rot 3 (Move c (Pt2 x y)) = Move c $ Pt2 (n - x) (n - y)
rot 4 (Move c (Pt2 x y)) = Move c $ Pt2 y x
rot 5 (Move c (Pt2 x y)) = Move c $ Pt2 y (n - x)
rot 6 (Move c (Pt2 x y)) = Move c $ Pt2 (n - y) x
rot 7 (Move c (Pt2 x y)) = Move c $ Pt2 (n - y) (n - x)

n :: Int
n = bdN - 1
