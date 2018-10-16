{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Mock
    ( ClientMock
    ) where

import Data.Text (Text)
import Control.Monad.Reader
import Control.Concurrent.MVar
import Control.Exception

import qualified Data.ByteString.Builder as Builder
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.HTTP.Types as HTTP

import Servant
import Servant.API
import Servant.Client
import Servant.Client.Core

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
        -- The 'Application' gives us its response and we return it.
        --
        -- Care has to be taken here because the 'Wai.Response' is actually
        -- just a specification of how the response is constructed (e.g. it
        -- might read files from the server's host), and after the
        -- 'Application' finishes, the response might well become invalid.
        --
        -- However, since we "execute" this response by converting it to a
        -- servant-client 'Response', we don't care if the response becomes
        -- invalid.
        responseMVar <- newEmptyMVar
        let storeResponse :: Wai.Response -> IO Wai.ResponseReceived
            storeResponse waiResponse = do
                let response = fromWaiResponse
                                   (requestHttpVersion request)
                                   waiResponse
                evaluate response
                putMVar responseMVar response
                pure Wai.ResponseReceived
        Wai.ResponseReceived <- app request storeResponse
        takeMVar responseMVar

    streamingRequest :: Request -> ClientMock StreamingResponse
    streamingRequest = error "implement if needed"

    throwServantError :: ServantError -> ClientMock a
    throwServantError = lift . throwIO

----------------------------------------------------------------------------
-- Lossy conversions between Servant's request/responses
-- and WAI requests/responses
----------------------------------------------------------------------------

-- 'toWaiRequest' makes a Servant request consumable by a WAI 'Application'.
toWaiRequest :: Request -> Wai.Request
toWaiRequest request =
    let clientRequest = requestToClientRequest request in
    Wai.defaultRequest
        { Wai.requestMethod  = method clientRequest
        , Wai.rawPathInfo    = path clientRequest
        , Wai.pathInfo       = path clientRequest
        , Wai.rawQueryString = queryString clientRequest
        , Wai.queryString    = queryString clientRequest
        , Wai.requestHeaders = requestHeaders clientRequest
        , Wai.requestBody    = requestBody clientRequest
        , Wai.isSecure       = secure clientRequest
        -- NB: the 'remoteHost' is doing to be localhost in our case, since
        -- we're doing all requests locally.
        }

-- 'fromWaiResponse' executes a response and ensures that once the Servant
-- response has been evaluated, the original WAI response can be safely
-- discarded.
fromWaiResponse :: Wai.Response -> Response
fromWaiResponse (Wai.ResponseBuilder status headers builder) =
    let body = Builder.toLazyByteString builder in
    rnf body `seq` Response
        { responseStatusCode = status
        , responseHeaders = headers
        , responseHttpVersion = HTTP.http11  -- always HTTP/1.1 for servant-client
        , responseBody = body
        }
fromWaiResponse Wai.ResponseFile{} =
    error "fromWaiResponse: implement for 'ResponseFile' is needed"
fromWaiResponse Wai.ResponseStream{} =
    error "fromWaiResponse: implement for 'ResponseStream' is needed"
fromWaiResponse Wai.ResponseRaw{} =
    error "fromWaiResponse: implement for 'ResponseRaw' is needed"
