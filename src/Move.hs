module Move (Pt2(..), Move(..), pretty, read, all, allOrd, bdN) where

import Control.Applicative
import Data.Char
import FUtil
import Prelude hiding (read, all)

import Color

data Pt2 a = Pt2 a a
  deriving (Eq, Show)

data Move = Move Color (Pt2 Int)
  deriving (Eq, Show)

pretty :: Move -> String
pretty (Move c (Pt2 x y)) = cS ++ xS ++ yS
  where
  cS = case c of
    Black -> "b"
    White -> "w"
  xS = [chr $ ord 'A' + x + if x >= 8 then 1 else 0]
  yS = show (bdN - y)

read :: String -> Maybe Move
read (cS:xS:yS) = do
  c <- case cS of
    'b' -> Just Black
    'w' -> Just White
    _ -> Nothing
  let
    x = fromIntegral $ (\ n -> if n > 8 then n - 1 else n) $ ord xS - ord 'A'
  y <- (bdN -) <$> readMb yS
  Just $ Move c (Pt2 x y)
read _ = Nothing

all :: [Move]
all = liftA2 Move [Black, White] $ liftA2 Pt2 [0 .. bdN - 1] [0 .. bdN - 1]

allOrd = liftA2 Move [Black, White] $
  [Pt2 x y | y <- [0 .. bdMid], x <- [n - y .. n]] ++
  [Pt2 x y | y <- [0 .. bdMid], x <- [bdMid .. n - y - 1]] ++
  [Pt2 x y | y <- [0 .. bdMid], x <- [y .. bdMid - 1]] ++
  [Pt2 x y | y <- [0 .. bdMid], x <- [0 .. y - 1]] ++
  [Pt2 x y | y <- [bdMid + 1 .. n], x <- [0 .. y]] ++
  [Pt2 x y | y <- [bdMid + 1 .. n], x <- [y + 1 .. bdMid]] ++
  [Pt2 x y | y <- [bdMid + 1 .. n], x <- [bdMid + 1 .. n - y]] ++
  [Pt2 x y | y <- [bdMid + 1 .. n], x <- [n - y  + 1 .. n]]
  where
  n = bdN - 1

bdN :: Int
bdN = 19

bdMid :: Int
bdMid = bdN `div` 2
