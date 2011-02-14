module Color where

data Color = Black | White deriving (Enum, Eq, Ord, Show)

colSwap Black = White
colSwap White = Black
