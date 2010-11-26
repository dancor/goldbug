module Zob (module ZobTable, posHash, posHashes) where

import Data.Bits
import Data.Word
import Move
import ZobTable

posHash :: Zob -> [Move] -> Hash
posHash zob = last . posHashes zob

posHashes :: Zob -> [Move] -> [Hash]
posHashes zob = scanl xor 0 . map zob
