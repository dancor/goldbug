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
-- $(derive makeBinary ''Setup)
instance Binary (Setup a) where
  get = error "Setup get"
  put = error "Setup put"
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
$(derive makeBinary ''GameNode)
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
