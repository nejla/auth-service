{-# LANGUAGE QuasiQuotes #-}

module AuthService.OpenAPI where

import qualified AuthService.Api            as API
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BSL
import           Data.Data                  ( Proxy(..) )
import qualified Data.HashMap.Strict.InsOrd as InsOrdHMap
import           Data.OpenApi
import           Data.String.Interpolate
import           Servant.OpenApi

api :: OpenApi
api = toOpenApi (Proxy :: Proxy API.Api)

-- NGinx translates some of the paths, we adjust the openAPI definitions to match
pathMap :: [(FilePath, FilePath)]
pathMap =
  [ ("/logout/{token}", "/logout")
  , ("/disable-sessions/{token}", "/disable-sessions")
  , ("/change-password/{token}", "/change-password")
  , ("/user-info-by-token/{token}", "/user-info")
  , ("/check-token/{token}", "/check-token")
  ]

-- Translate a single path item from `from` path to `to` path
translatePath
  :: (FilePath, FilePath) -> [(FilePath, PathItem)] -> [(FilePath, PathItem)]
translatePath (from, _) [] =
  error [i|translatePath: Could not find path "#{from}" in OpenAPI spec|]
translatePath (from, to) ((path, item) : items)
  | path == from = (to, item) : items
  | otherwise = (path, item) : translatePath (from, to) items

-- Apply a function to all path items, keeping ordering
withPaths
  :: ([(FilePath, PathItem)] -> [(FilePath, PathItem)]) -> OpenApi -> OpenApi
withPaths f api =
  api
  { _openApiPaths =
      -- Going via list is the only way to keep the ordering.
      InsOrdHMap.fromList (f $ InsOrdHMap.toList $ _openApiPaths api)
  }

translateApi :: OpenApi -> OpenApi
translateApi = withPaths (\paths -> foldr translatePath paths pathMap)

-- translatePaths :: OpenApi -> OpenApi
-- translatePaths api = api {
--   _openApiPAths =
--       }

writeOriginalDefinition :: FilePath -> IO ()
writeOriginalDefinition path =
  BSL.writeFile path (Aeson.encode api)

writeTranslatedDefinition :: FilePath -> IO ()
writeTranslatedDefinition path =
  BSL.writeFile path (Aeson.encode $ translateApi api)
