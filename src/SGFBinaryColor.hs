{-# LANGUAGE TemplateHaskell #-}

module SGFBinaryColor where

import Control.Monad
import Data.Binary
import Data.DeriveTH
import Data.SGF

$(derive makeBinary ''Color)
