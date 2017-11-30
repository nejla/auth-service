-- Copyright 2015 Lambdatrade AB
-- All rights reserved

module Helpers where

import           Control.Monad.Trans
import           Data.Char
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.IO


dropPrefix :: [Char] -> [Char] -> [Char]
dropPrefix pre str=
    case pre `List.stripPrefix` str of
     Just (n:ns) -> toLower n : ns
     _ -> error "prefix now found"

debug :: MonadIO m => Text -> m ()
debug = liftIO  . Text.hPutStrLn stderr

showText :: Show a => a -> Text
showText = Text.pack . show
