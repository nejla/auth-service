module SignedAuth.Util where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import qualified Data.List     as List

-- | Convert the first character in a String to lower case
downcase :: String -> String
downcase [] = []
downcase (x:xs) = toLower x : xs

-- | Convert the first character in a String to upper case
upcase :: String -> String
upcase [] = []
upcase (x:xs) = toUpper x : xs

-- | Remove a prefix from a String, throwing an error of the prefix is not found
withoutPrefix :: String -- ^ Prefix to remove
              -> String -- ^ Input string
              -> String
withoutPrefix pre' l = case List.stripPrefix pre' l of
    Nothing -> error $ pre' <> " is not a prefix of " <> l
    Just l' -> l'

-- | Default options for creating JSON instances using Aeson.
aesonTHOptions :: [Char] -- ^ field prefix to strip
               -> Options
aesonTHOptions pre' = defaultOptions{ fieldLabelModifier = mkName'
                                    , constructorTagModifier = mkCName
                                    }
  where
    mkName' = cctu . withoutPrefix pre'
    mkCName = cctu . withoutPrefix (upcase pre')
    cctu = camelTo2 '_'
