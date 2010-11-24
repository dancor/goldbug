module Move (Pt2(..), Move(..), pretty, read, all) where

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
  yS = show (19 - y)

read :: String -> Maybe Move
read (cS:xS:yS) = do
  c <- case cS of
    'b' -> Just Black
    'w' -> Just White
    _ -> Nothing
  let
    x = fromIntegral $ (\ n -> if n > 8 then n - 1 else n) $ ord xS - ord 'A'
  y <- (19 -) <$> readMb yS
  Just $ Move c (Pt2 x y)
read _ = Nothing

all :: [Move]
all = liftA2 Move [Black, White] $ liftA2 Pt2 [0 .. n - 1] [0 .. n - 1]
  where n = 19
