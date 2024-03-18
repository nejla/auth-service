module AuthService.OpenAPI where

import qualified AuthService.Api      as API

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Data            ( Proxy(..) )

import           Servant.OpenApi

apiDefinition :: BSL.ByteString
apiDefinition = Aeson.encode $ toOpenApi (Proxy :: Proxy API.Api)

writeDefinition :: FilePath -> IO ()
writeDefinition path = BSL.writeFile path apiDefinition
