import Control.Monad
import Control.Monad.ST
import Data.Word
import System.Random.MWC

import Color
import Move

main :: IO ()
main = writeFile "ZobTable.hs" . unlines $
  "-- this file is generated from genZobTable.hs":
  "module ZobTable where":
  "import Data.Word":
  "import Color":
  "import Move":
  "type Hash = Word64":
  "type Zob = Move -> Hash":
  "zob :: Zob":
  zipWith (\ m r -> "zob (" ++ show m ++ ") = " ++ show (r :: Word64))
    Move.all (runST $ create >>= replicateM (length Move.all) . uniform) ++
  "zob _ = 0":
  []
