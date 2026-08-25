{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.IO qualified as LText
import Language.Haskell.TH.Quote qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.Microstache qualified as Mustache

deriving instance TH.Lift Mustache.PName

deriving instance TH.Lift Mustache.Key

deriving instance TH.Lift Mustache.Node

deriving instance TH.Lift Mustache.Template

htmlTemplate :: String -> TH.Q TH.Exp
htmlTemplate name = do
  let templatePath = "src/html/" <> name
  TH.addDependentFile templatePath
  templateStr <- TH.runIO $ LText.readFile templatePath
  case Mustache.compileMustacheText "password reset email" templateStr of
    Left e -> error (show e)
    Right template -> TH.lift template

mustache :: TH.QuasiQuoter
mustache =
  TH.QuasiQuoter
    { TH.quoteExp = \str ->
        case Mustache.compileMustacheText
          "quote"
          (LText.pack str) of
          Left e -> error (show e)
          Right template -> TH.lift template
    , TH.quotePat = error "quotePat not defined for moustache"
    , TH.quoteType = error "quoteType not defined for moustache"
    , TH.quoteDec = error "quoteDec not defined for moustache"
    }
