{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Servant.Common.Req (
  module Servant.Common.Req,
  ClientEnv (ClientEnv),
  Response,
  ServantError (..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError(..))
#else
import Control.Monad.Error.Class (MonadError(..))
#endif
import Control.Monad.Trans.Except


import GHC.Generics
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem)
import Data.String
import Data.String.Conversions
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.HTTP.Types.Header as HTTP
import Network.URI hiding (path)
import Servant.API.ContentTypes
import Servant.Client.PerformRequest
import Servant.Client.ServantError (ServantError)
import qualified Servant.Client.ServantError as SE
import Servant.Common.BaseUrl
import Web.HttpApiData

data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (ByteString, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] Nothing [] []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

addHeader :: ToHttpApiData a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, decodeUtf8 (toHeader val))]
                             }

setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t r = r { reqBody = Just (b, t) }

reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort path) =
    setheaders . setAccept . setrqb . setQS <$> parseRequest url

  where url = show $ nullURI { uriScheme = case reqScheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = reqHost
                                         , uriPort = ":" ++ show reqPort
                                         }
                             , uriPath = path ++ reqPath req
                             }

        setrqb = setRequestBody $ reqBody req
        setQS = setQueryString $ queryTextToQuery (qs req)
        setheaders r = setRequestHeaders (requestHeaders r <> fmap toProperHeader (headers req)) r
        setAccept r = setRequestHeaders (filter ((/= "Accept") . fst) (requestHeaders r)
                                        <> [("Accept", renderHeader $ reqAccept req)
                                              | not . null . reqAccept $ req]) r
        toProperHeader (name, val) =
          (fromString name, encodeUtf8 val)

-- * performing requests

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"

-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Manager' and 'BaseUrl' used for requests in the reader environment.

newtype ClientM a = ClientM { runClientM' :: ReaderT ClientEnv (ExceptT ServantError IO) a }
                    deriving ( Functor, Applicative, Monad, MonadIO, Generic
                             , MonadReader ClientEnv
                             , MonadError ServantError
                             )

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ (flip runReaderT env) $ runClientM' cm


performRequest :: Method -> Req
               -> ClientM ( Int, ByteString, MediaType
                          , [HTTP.Header], Response ByteString)
performRequest reqMethod req = do
  clientEnv <- ask
  reqHost <- asks baseUrl
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = setMethod reqMethod partialRequest

  eResponse <- liftIO $ performHttpRequest clientEnv request
  case eResponse of
    Left err ->
      throwError . SE.ConnectionError $ SomeException err

    Right response -> do
      let status = responseStatus response
          body = responseBody response
          hdrs = responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> throwError $ SE.InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (status_code >= 200 && status_code < 300) $
        throwError $ SE.FailureResponse status ct body
      return (status_code, body, ct, hdrs, response)

performRequestCT :: MimeUnrender ct result => Proxy ct -> Method -> Req
    -> ClientM ([HTTP.Header], result)
performRequestCT ct reqMethod req = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] })
  unless (matches respCT (acceptCT)) $ throwError $ SE.UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> throwError $ SE.DecodeFailure err respCT respBody
    Right val -> return (hdrs, val)

performRequestNoBody :: Method -> Req -> ClientM [HTTP.Header]
performRequestNoBody reqMethod req = do
  (_status, _body, _ct, hdrs, _response) <- performRequest reqMethod req
  return hdrs
