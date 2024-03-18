{-# LANGUAGE LambdaCase #-}

module Main where

import AuthService.OpenAPI
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  getArgs >>= \case
      [ path ] -> writeDefinition path
      _ -> hPutStrLn stderr "Missing argument: filepath"
