{-# LANGUAGE OverloadedStrings #-}

module Servant.Client.PerformRequest.GHCJS.Types where

import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Exception (Exception)
import           Data.Bool (bool)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Network.HTTP.Types (Header, Query, Status, Method, renderQuery)
import           Network.URI (URI (URI), URIAuth (URIAuth), parseURI, escapeURIString, isAllowedInURI)
import           Text.Read (readMaybe)

import           Servant.Common.BaseUrl (BaseUrl)

data ClientEnv = ClientEnv BaseUrl

baseUrl :: ClientEnv -> BaseUrl
baseUrl (ClientEnv b) = b

data Request = Request { method :: Method
                       , requestHeaders :: [Header]
                       , secure :: Bool
                       , host :: String
                       , port :: Int
                       , path :: BS.ByteString
                       , queryString :: BS.ByteString
                       , requestBody :: RequestBody }
data RequestBody = RequestBodyLBS LBS.ByteString
data Response a = Response { responseStatus :: Status
                           , responseHeaders :: [Header]
                           , responseBody :: a }

data InvalidUrlException = InvalidUrlException String deriving (Show, Typeable)
instance Exception InvalidUrlException

emptyRequestBody :: RequestBody
emptyRequestBody = RequestBodyLBS ""

setQueryString :: Query -> Request -> Request
setQueryString s r = r { queryString = renderQuery True s }

parseRequest :: MonadThrow m => String -> m Request
parseRequest s' = maybe bad conv . parseURI $ encode s
  where
    conv (URI sch (Just (URIAuth user regName portStr)) pth qry _) = do
        sec <- case sch of
            "http:"  -> pure False
            "https:" -> pure True
            _        -> bad
        prt <- case portStr of
            ':':r -> maybe bad pure $ readMaybe r
            _     -> pure $ bool 80 443 sec
        let mth = maybe "GET" BS.pack mmethod
        pure $
            Request { method = mth
                    , requestHeaders = []
                    , secure = sec
                    , host = user ++ regName
                    , port = prt
                    , path = BS.pack $ if null pth then "/" else pth
                    , queryString = BS.pack qry
                    , requestBody = emptyRequestBody }
    conv _ = bad
    encode = escapeURIString isAllowedInURI
    (mmethod, s) =
        case break (== ' ') s' of
            (x, ' ':y) | all (\c -> 'A' <= c && c <= 'Z') x -> (Just x, y)
            _ -> (Nothing, s')
    bad = throwM $ InvalidUrlException s'

setRequestHeaders :: [Header] -> Request -> Request
setRequestHeaders hs r = r { requestHeaders = hs }

setMethod :: Method -> Request -> Request
setMethod m r = r { method = m }

setRequestBody :: RequestBody -> Request -> Request
setRequestBody b r = r { requestBody = b }

byteBody :: LBS.ByteString -> RequestBody
byteBody = RequestBodyLBS
