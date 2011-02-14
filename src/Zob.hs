module Zob (module ZobTable, posHash, posHashes) where

import Data.Bits
import Data.Word
import Color (colSwap)
import Move
import ZobTable

posHash :: Zob -> [Move] -> Hash
posHash zob = last . posHashes zob

posHashes :: Zob -> [Move] -> [Hash]
posHashes zob mvs = 
  minimum . map (\ (r, inv) -> scanl xor 0 $ map (zob . rot r inv) mvs) $ 
  [(r, inv) | r <- [0..7], inv <- [False, True]]

rot :: Int -> Bool -> Move -> Move
rot r inv (Move c (Pt2 x y)) = Move (if inv then c else colSwap c) $ case r of
  0 -> Pt2 x y
  1 -> Pt2 (n - x) y
  2 -> Pt2 x (n - y)
  3 -> Pt2 (n - x) (n - y)
  4 -> Pt2 y x
  5 -> Pt2 y (n - x)
  6 -> Pt2 (n - y) x
  7 -> Pt2 (n - y) (n - x)

n :: Int
n = bdN - 1
