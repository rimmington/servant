{-# LANGUAGE CPP #-}

module Servant.Client.PerformRequest (
  ClientEnv (ClientEnv), baseUrl,
  Request, requestHeaders, setRequestHeaders, setMethod, setRequestBody, setQueryString,
  RequestBody, emptyRequestBody, byteBody,
  Response, responseStatus, responseBody, responseHeaders,
  parseRequest,
  performHttpRequest,
) where

import           Control.Monad.Catch (MonadThrow)
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Types (Header, Query, Status, Method)

#ifdef __GHCJS__
import qualified Servant.Client.PerformRequest.GHCJS as M
import           Servant.Client.PerformRequest.GHCJS (Request, RequestBody, Response, ClientEnv (ClientEnv))
#else
import qualified Servant.Client.PerformRequest.GHC as M
import           Servant.Client.PerformRequest.GHC (Request, RequestBody, Response, ClientEnv (ClientEnv))
#endif
import           Servant.Client.ServantError (ServantError)
import           Servant.Common.BaseUrl (BaseUrl)

baseUrl :: ClientEnv -> BaseUrl
baseUrl = M.baseUrl

performHttpRequest :: ClientEnv -> Request -> IO (Either ServantError (Response ByteString))
performHttpRequest = M.performHttpRequest

parseRequest :: MonadThrow m => String -> m Request
parseRequest = M.parseRequest

requestHeaders :: Request -> [Header]
requestHeaders = M.requestHeaders

setRequestHeaders :: [Header] -> Request -> Request
setRequestHeaders = M.setRequestHeaders

setMethod :: Method -> Request -> Request
setMethod = M.setMethod

setRequestBody :: RequestBody -> Request -> Request
setRequestBody = M.setRequestBody

setQueryString :: Query -> Request -> Request
setQueryString = M.setQueryString

responseStatus :: Response a -> Status
responseStatus = M.responseStatus

responseBody :: Response a -> a
responseBody = M.responseBody

responseHeaders :: Response a -> [Header]
responseHeaders = M.responseHeaders

emptyRequestBody :: RequestBody
emptyRequestBody = M.emptyRequestBody

byteBody :: ByteString -> RequestBody
byteBody = M.byteBody
