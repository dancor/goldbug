module Zob (module ZobTable, posHash, posHashes, gameHashes) where

import Data.Bits
import Data.Word
import Game
import Move
import ZobTable

posHash :: Zob -> [Move] -> Hash
posHash zob = last . posHashes zob

posHashes :: Zob -> [Move] -> [Hash]
posHashes zob = scanl xor 0 . map zob

gameHashes :: Zob -> Game -> [Hash]
gameHashes zob = posHashes zob . moves
