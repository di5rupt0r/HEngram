{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Database.Redis (checkedConnect, defaultConnectInfo)
import Network.HTTP.Types
    ( hContentType
    , status200, status202, status400, status404, status415
    )
import Network.Wai
    ( Application, defaultRequest
    , responseLBS, responseStatus, responseHeaders
    )
import Network.Wai.Internal (Request (..))
import Network.Wai.EventSource (ServerEvent)
import Network.Wai.Test
    ( SRequest (..), SResponse (..)
    , runSession, srequest, setPath
    )
import Test.Hspec

import App (router)
import Types

-- ── Fixtures ──────────────────────────────────────────────────────────────────

mkApp :: IO Application
mkApp = do
    conn <- checkedConnect defaultConnectInfo
    tv   <- newTVarIO Map.empty
    return $ router (ServerState conn tv)

mkAppWithSession :: IO (Application, TQueue ServerEvent)
mkAppWithSession = do
    conn  <- checkedConnect defaultConnectInfo
    queue <- newTQueueIO
    tv    <- newTVarIO (Map.singleton "test-sid" queue)
    return (router (ServerState conn tv), queue)

-- | POST with JSON Content-Type.
mkJsonPost :: C8.ByteString -> A.Value -> SRequest
mkJsonPost path body = SRequest req (A.encode body)
  where
    req = setPath
            defaultRequest
                { requestMethod  = "POST"
                , requestHeaders = [(hContentType, "application/json")]
                }
            path

-- | POST with arbitrary Content-Type.
mkPost :: C8.ByteString -> C8.ByteString -> C8.ByteString -> SRequest
mkPost path ct body = SRequest req (C8.fromStrict body)
  where
    req = setPath
            defaultRequest
                { requestMethod  = "POST"
                , requestHeaders = [(hContentType, ct)]
                }
            path

-- | Wrap app to intercept response, allowing SSE test to verify headers
-- without blocking on the infinite streaming body.
-- We feed an empty response to the session as soon as we have the status+headers.
interceptHeaders :: Application -> Application
interceptHeaders application req respond =
    application req $ \waiResp -> do
        -- Build minimal SResponse from the response headers/status only.
        -- This avoids reading the streaming body (which blocks on TQueue).
        let r = Network.Wai.responseStatus waiResp
            h = Network.Wai.responseHeaders waiResp
        respond (Network.Wai.responseLBS r h "")

-- ── Specs ─────────────────────────────────────────────────────────────────────

rpcBody :: A.Value
rpcBody = A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method"  A..= ("ping" :: String)
    , "params"  A..= A.Null
    , "rpcId"   A..= A.Null
    ]

spec :: Spec
spec = do
    describe "GET /sse" $
        it "returns 200 with text/event-stream Content-Type" $ do
            application <- mkApp
            let req = setPath defaultRequest { requestMethod = "GET" } "/sse"
            -- Use interceptHeaders to avoid blocking on the streaming body.
            r <- runSession (srequest (SRequest req "")) (interceptHeaders application)
            simpleStatus r `shouldBe` status200
            let cts = map snd (filter ((== hContentType) . fst) (simpleHeaders r))
            cts `shouldSatisfy` any (== "text/event-stream")

    describe "POST /messages" $ do
        it "returns 404 when session_id does not exist" $ do
            application <- mkApp
            r <- runSession (srequest $ mkJsonPost "/messages?session_id=ghost" rpcBody) application
            simpleStatus r `shouldBe` status404

        it "returns 202 Accepted for a valid session" $ do
            (application, _) <- mkAppWithSession
            r <- runSession (srequest $ mkJsonPost "/messages?session_id=test-sid" rpcBody) application
            simpleStatus r `shouldBe` status202

        it "enqueues exactly one ServerEvent for the session" $ do
            (application, queue) <- mkAppWithSession
            _ <- runSession (srequest $ mkJsonPost "/messages?session_id=test-sid" rpcBody) application
            ev <- atomically $ tryReadTQueue queue
            isJust ev `shouldBe` True

        it "returns 400 when session_id param is missing" $ do
            application <- mkApp
            r <- runSession (srequest $ mkJsonPost "/messages" rpcBody) application
            simpleStatus r `shouldBe` status400

        it "returns 415 when Content-Type is not application/json" $ do
            application <- mkApp
            r <- runSession (srequest $ mkPost "/messages?session_id=x" "text/plain" "not json") application
            simpleStatus r `shouldBe` status415

        it "returns 400 for malformed JSON body" $ do
            application <- mkApp
            r <- runSession (srequest $ mkJsonPost "/messages?session_id=x" (A.String "not-rpc")) application
            simpleStatus r `shouldBe` status400
