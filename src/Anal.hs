module Anal where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Text.Printf
import qualified Data.IntMap as IntMap

import Db
import Color
import Move
import Zob

showPosInfo :: PosInfo -> String
showPosInfo (PosInfo b w) = show (b, w) ++ " " ++ printf "%.2f" 
  (100 * fromIntegral b / fromIntegral (b + w) :: Float)

-- idk if this approach is actually statistically sound
pval :: Int -> Int -> Int -> Int -> Double
pval b1 w1 b2 w2 = pvalue $ chiSqVal exp1 b1d exp2 b2d where
  pvalue chiSq = 2 / (1 + exp(0.496937 * sqrt(chiSq * (chiSq + 10.28))))
  chiSqVal e1 o1 e2 o2 = (e1 - o1)^2 / e1 + (e2 - o2)^2 / e2
  exp1 = n1 * b / n
  exp2 = n2 * b / n
  b = b1d + b2d
  n = n1 + n2
  n1 = b1d + w1d
  n2 = b2d + w2d
  b1d = fromIntegral b1
  b2d = fromIntegral b1
  w1d = fromIntegral w2
  w2d = fromIntegral w2

sndMoveBetter :: Double -> (Move, PosInfo) -> (Move, PosInfo) -> Bool
sndMoveBetter p (Move c _, (PosInfo b1 w1)) (_, (PosInfo b2 w2)) =
  didBetter && pval b1 w1 b2 w2 < p
  where
  didBetter = case c of
    Black -> b2 * (b1 + w1) > b1 * (b2 + w2)
    White -> b2 * (b1 + w1) < b1 * (b2 + w2)

bestMove :: Double -> [(Move, PosInfo)] -> (Move, PosInfo)
bestMove p (mv:mvs) = if or $ map (sndMoveBetter p mv) mvs
  then bestMove p mvs
  else mv
