{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Compat where

import qualified Servant.API as Servant

#if MIN_VERSION_servant(0,17,0)
type PostNoContent = Servant.PostNoContent
#else
type PostNoContent = Servant.PostNoContent '[Servant.JSON] Servant.NoContent
#endif
