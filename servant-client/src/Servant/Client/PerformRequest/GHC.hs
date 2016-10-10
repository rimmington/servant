{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.PerformRequest.GHC (
  ServantError(..),
  ClientEnv (ClientEnv), baseUrl,
  Request, requestHeaders, setRequestHeaders, setMethod, setRequestBody, setQueryString,
  RequestBody, emptyRequestBody, byteBody,
  Response, responseStatus, responseBody, responseHeaders,
  parseRequest,
  performHttpRequest,
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Client
import           Network.HTTP.Types (Header, Method)

import           Servant.Client.ServantError (ServantError (ConnectionError))
import           Servant.Common.BaseUrl (BaseUrl)

data ClientEnv = ClientEnv Manager BaseUrl

#if !MIN_VERSION_http_client(0,4,30)
-- 'parseRequest' is introduced in http-client-0.4.30
-- it differs from 'parseUrl', by not throwing exceptions on non-2xx http statuses
--
-- See for implementations:
-- http://hackage.haskell.org/package/http-client-0.4.30/docs/src/Network-HTTP-Client-Request.html#parseRequest
-- http://hackage.haskell.org/package/http-client-0.5.0/docs/src/Network-HTTP-Client-Request.html#parseRequest
parseRequest :: MonadThrow m => String -> m Request
parseRequest url = liftM disableStatusCheck (parseUrl url)
  where
    disableStatusCheck req = req { checkStatus = \ _status _headers _cookies -> Nothing }
#endif

performHttpRequest :: ClientEnv -> Request
  -> IO (Either ServantError (Response LBS.ByteString))
performHttpRequest (ClientEnv manager _) request =
  catchConnectionError $ httpLbs request manager

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: HttpException)

setRequestHeaders :: [Header] -> Request -> Request
setRequestHeaders hs r = r { requestHeaders = hs }

setRequestBody :: RequestBody -> Request -> Request
setRequestBody body r = r { requestBody = body }

setMethod :: Method -> Request -> Request
setMethod m r = r { method = m }

baseUrl :: ClientEnv -> BaseUrl
baseUrl (ClientEnv _ b) = b

emptyRequestBody :: RequestBody
emptyRequestBody = RequestBodyLBS ""

byteBody :: LBS.ByteString -> RequestBody
byteBody = RequestBodyLBS
