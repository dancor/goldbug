module Game where

import Color
import Move

data Game = Game {
  winner :: Color,
  moves :: [Move]
  }
  deriving (Show)

