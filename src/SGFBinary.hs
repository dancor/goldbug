{-# LANGUAGE TemplateHaskell #-}

module SGFBinary where

import Control.Monad
import Data.Binary
import Data.DeriveTH
import Data.SGF

-- $(derive makeBinary ''Figure)
instance Binary Figure where
  get = error "Figure get"
  put = error "Figrue put"

instance (Binary a, Ord a) => Binary (Setup a) where
  get = liftM4 Setup get get get get
  put (Setup b w r p) = put b >> put w >> put r >> put p
instance (Binary a, Binary b, Binary c, Binary d, Binary e, Ord b) =>
    Binary (GameNode a b c d e) where
  get = liftM5 GameNode get get get get get
  put (GameNode a b c d e) = put a >> put b >> put c >> put d >> put e

-- $(derive makeBinary ''Void)
instance Binary Void where
  get = error "Void get"
  put = error "Void put"

$(derive makeBinary ''Annotation)
$(derive makeBinary ''Certainty)
$(derive makeBinary ''Color)
$(derive makeBinary ''Emphasis)
$(derive makeBinary ''FuzzyBool)
$(derive makeBinary ''Game)
$(derive makeBinary ''GameInfo)
$(derive makeBinary ''GameInfoGo)
$(derive makeBinary ''GameInfoLinesOfAction)
$(derive makeBinary ''GameInfoOcti)
$(derive makeBinary ''GameInfoType)
$(derive makeBinary ''GameResult)
$(derive makeBinary ''GameTree)
$(derive makeBinary ''GameType)
$(derive makeBinary ''InitialPlacement)
$(derive makeBinary ''InitialPosition)
$(derive makeBinary ''Judgment)
$(derive makeBinary ''MajorVariation)
$(derive makeBinary ''Mark)
$(derive makeBinary ''Markup)
$(derive makeBinary ''MatchInfo)
$(derive makeBinary ''MinorVariation)
$(derive makeBinary ''Move)
$(derive makeBinary ''MoveGo)
$(derive makeBinary ''Numbering)
$(derive makeBinary ''PartialDate)
$(derive makeBinary ''Quality)
$(derive makeBinary ''Rank)
$(derive makeBinary ''RankScale)
$(derive makeBinary ''Round)
$(derive makeBinary ''RuleSet)
$(derive makeBinary ''RuleSetBackgammon)
$(derive makeBinary ''RuleSetGo)
$(derive makeBinary ''RuleSetOcti)
$(derive makeBinary ''VariationType)
$(derive makeBinary ''ViewerSetting)
$(derive makeBinary ''WinType)
