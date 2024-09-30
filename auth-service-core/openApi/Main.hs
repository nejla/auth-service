{-# LANGUAGE LambdaCase #-}

module Main where

import AuthService.OpenAPI
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  getArgs >>= \case
      [ "external", path ] -> writeTranslatedDefinition path
      [ "internal", path ] -> writeOriginalDefinition path
      _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " <> progName <>" [external|internal] path"
        exitFailure
