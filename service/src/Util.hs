{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Util where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.IO          as LText
import qualified Language.Haskell.TH.Quote  as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Microstache           as Mustache

-- Orphan instances for Microstache Templates
instance (TH.Lift a, TH.Lift b) => TH.Lift (Map a b) where
  lift m = [|Map.fromAscList $(TH.lift $ Map.toAscList m)|]

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
                    case Mustache.compileMustacheText "quote"
                                     (LText.pack str) of
                      Left e -> error (show e)
                      Right template -> TH.lift template
  , TH.quotePat  = error "quotePat not defined for moustache"
  , TH.quoteType = error "quoteType not defined for moustache"
  , TH.quoteDec  = error "quoteDec not defined for moustache"
  }
