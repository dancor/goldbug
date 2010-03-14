module Sgf (parseSgf, Point, Mv, Color(..), Game) where

import Control.Applicative
import Control.Monad
import Data.Char
import FUtil

type Point = (Int, Int)

type Mv = (Color, Point)

data Color = Black | White deriving (Enum, Eq, Ord)

type Game = (Color, [Mv])

-- test crappy parsing:
-- - very specific to my data set
--   - no game branching
--   - assume certain substrings only appear in certain places
-- - def do not handle ]-escaping like: [a\]b]

parseSgf :: String -> String -> Either String [Game]
parseSgf f s = case breakOnSubl "]RE[" s of
  Nothing -> Right []
  Just (_, cCh:rest) -> do
    let
      (mvsStr, rest') = break (== ')') . drop 1 $ dropWhile (/= ';') rest
    c <- cRead cCh
    mvs <- mapM mvRead $ breaks (== ';') mvsStr
    ((c, mvs):) <$> parseSgf f rest'

mvRead :: String -> Either String Mv
mvRead (cCh:'[':xCh:yCh:']':[]) =
  liftM2 (,) (cRead cCh) (liftM2 (,) (xRead xCh) (xRead yCh))
mvRead s = Left $ "Expected mv but got " ++ s

cRead :: Char -> Either String Color
cRead cCh = case cCh of
  'B' -> Right Black
  'W' -> Right White
  _ -> Left $ "Expected B or W but got " ++ [cCh]

xRead :: Char -> Either String Int
xRead = Right . (subtract $ ord 'a') . ord
