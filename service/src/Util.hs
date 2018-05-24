{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Util where

import           Data.Monoid
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.IO          as LText
import           Instances.TH.Lift          ()
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Text.Microstache           as Mustache

-- Orphan instances for Microstache Templates

deriving instance TH.Lift Mustache.PName
deriving instance TH.Lift Mustache.Key
deriving instance TH.Lift Mustache.Node
deriving instance TH.Lift Mustache.Template

htmlTemplate :: String -> TH.Q TH.Exp
htmlTemplate name = do
  let templatePath = "src/html/" <> name
      cssPaths =
        [ "src/html/mui-email-inline.min.css"
        , "src/html/mui-email-styletag.min.css"
        ]
  TH.addDependentFile templatePath
  templateStrRaw <- TH.runIO $ LText.readFile templatePath
  styleContent <-
    TH.runIO $ LText.intercalate "\n" <$> mapM LText.readFile cssPaths
  let styleString = LText.unlines ["<style>", styleContent, "</style>"]
      templateStr =
        LText.replace "<style-placeholder/>" styleString templateStrRaw
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
