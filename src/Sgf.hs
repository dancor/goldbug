--module Sgf (parseSgf, Point, Mv, Color(..), Game) where
module Sgf where

import Control.Applicative
import Control.Monad
import Data.Char
import FUtil

import Color
import Game
import Move

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
    mvs <- mapM moveRead $ breaks (== ';') mvsStr
    (Game c mvs :) <$> parseSgf f rest'

moveRead :: String -> Either String Move
moveRead (cCh:'[':xCh:yCh:']':[]) =
  liftM2 Move (cRead cCh) $ liftM2 Pt2 (xRead xCh) (xRead yCh)
moveRead s = Left $ "Expected mv but got " ++ s

cRead :: Char -> Either String Color
cRead cCh = case cCh of
  'B' -> Right Black
  'W' -> Right White
  _ -> Left $ "Expected B or W but got " ++ [cCh]

xRead :: Char -> Either String Int
xRead = Right . (subtract $ ord 'a') . ord
