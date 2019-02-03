{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-} -- for requestBody

module Mock
    ( ClientMock
    ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Control.Monad.Reader
import Control.Concurrent.MVar
import Control.Exception
import Control.DeepSeq (rnf)
import GHC.Exts (fromList)

import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Client as HTTP

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as Builder

import Servant
import Servant.API
import Servant.Client
import Servant.Client.Core
import Servant.Client.Internal.HttpClient (requestToClientRequest)

-- We'd like to be able to perform requests without doing actual networking.
-- If we have a WAI 'Application', we can do just that, so the following
-- type is a suitable monad to run our client functions in.
type ClientMock = ReaderT Application IO

-- servant-client can generate clients running in any monad that implements
-- 'RunClient'. Ideally we might want to copy the implementation[*] for
-- 'ClientM' almost verbatim, because it has cookie-handling logic that is
-- not implemented here. However, for a proof-of-concept the following
-- implementation is fine (as long as you don't use streaming).
--
-- [*] https://hackage.haskell.org/package/servant-client-0.14/docs/src/Servant.Client.Internal.HttpClient.html#line-118
instance RunClient ClientMock where
    runRequest :: Request -> ClientMock Response
    runRequest request = ReaderT $ \app -> do
        responseMVar <- newEmptyMVar @Response
        let storeResponse :: Wai.Response -> IO Wai.ResponseReceived
            storeResponse waiResponse = do
                response <- fromWaiResponse waiResponse
                putMVar responseMVar response
                pure Wai.ResponseReceived
        Wai.ResponseReceived <- app (toWaiRequest request) storeResponse
        takeMVar responseMVar

    throwServantError :: ServantError -> ClientMock a
    throwServantError = lift . throwIO

----------------------------------------------------------------------------
-- Lossy conversions between Servant's request/responses
-- and WAI requests/responses
----------------------------------------------------------------------------

-- 'toWaiRequest' makes a Servant request consumable by a WAI 'Application'.
toWaiRequest :: Request -> Wai.Request
toWaiRequest request =
    Wai.defaultRequest
        { Wai.requestMethod  = HTTP.method clientRequest
        , Wai.rawPathInfo    = HTTP.path clientRequest
        , Wai.pathInfo       = map decodeUtf8 . BC.split '/' . BC.drop 1 $
                                 HTTP.path clientRequest
        , Wai.rawQueryString = HTTP.queryString clientRequest
        , Wai.queryString    = HTTP.parseQuery $
                                 HTTP.queryString clientRequest
        , Wai.requestHeaders = HTTP.requestHeaders clientRequest
        , Wai.requestBody    = toWaiRequestBody $
                                 HTTP.requestBody clientRequest
        , Wai.isSecure       = HTTP.secure clientRequest
        -- NB: the 'remoteHost' is doing to be localhost in our case, since
        -- we're doing all requests locally. It's already a default in
        -- 'defaultRequest', therefore we don't include 'HTTP.host' as a
        -- field here.
        }
  where
    localhost = BaseUrl Https "0.0.0.0" 80 ""
    clientRequest = requestToClientRequest localhost request

-- 'toWaiRequestBody' converts a http-client representation of a request
-- body to a simpler one employed by WAI.
--
-- The proof-of-concept doesn't handle streaming request bodies.
toWaiRequestBody :: HTTP.RequestBody -> IO ByteString
toWaiRequestBody body = case body of
    HTTP.RequestBodyLBS body' ->
        pure (BSL.toStrict body')
    HTTP.RequestBodyBS body' ->
        pure body'
    HTTP.RequestBodyBuilder _ body' ->
        pure (BSL.toStrict (Builder.toLazyByteString body'))
    HTTP.RequestBodyIO body' ->
        toWaiRequestBody =<< body'
    HTTP.RequestBodyStream{} ->
        error "toWaiRequestBody: RequestBodyStream yet unsupported"
    HTTP.RequestBodyStreamChunked{} ->
        error "toWaiRequestBody: RequestBodyStreamChunked yet unsupported"

-- 'fromWaiResponse' executes a response.
--
-- The proof-of-concept only handles the most common response type.
fromWaiResponse :: Wai.Response -> IO Response
fromWaiResponse (Wai.ResponseBuilder status headers builder) =
    pure Response
        { responseStatusCode = status
        , responseHeaders = fromList headers
        , responseHttpVersion = HTTP.http11  -- always HTTP/1.1 for servant-client
        , responseBody = Builder.toLazyByteString builder
        }
fromWaiResponse Wai.ResponseFile{} =
    error "fromWaiResponse: ResponseFile yet unsupported"
fromWaiResponse Wai.ResponseStream{} =
    error "fromWaiResponse: ResponseStream yet unsupported"
fromWaiResponse Wai.ResponseRaw{} =
    error "fromWaiResponse: ResponseRaw yet unsupported"
