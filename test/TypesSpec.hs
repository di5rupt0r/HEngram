{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypesSpec (spec) where

import Data.Aeson (decode, encode, Value (..), (.=), object)
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "RpcRequest" $ do

        it "round-trips through JSON" $ do
            let req = RpcRequest "2.0" "ping" Nothing Nothing
            decode (encode req) `shouldBe` Just req

        it "round-trips with params and id" $ do
            let v   = Number 42
                req = RpcRequest "2.0" "tools/call" (Just v) (Just v)
            decode (encode req) `shouldBe` Just req

        it "parses a valid JSON-RPC 2.0 object" $ do
            let raw = "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"params\":null,\"id\":null}"
            (decode raw :: Maybe RpcRequest) `shouldNotBe` Nothing

        it "fails to parse when 'method' is missing" $ do
            let raw = "{\"jsonrpc\":\"2.0\",\"rpcId\":null}"
            (decode raw :: Maybe RpcRequest) `shouldBe` Nothing

        it "fails to parse when 'jsonrpc' is missing" $ do
            let raw = "{\"method\":\"ping\",\"params\":null,\"rpcId\":null}"
            (decode raw :: Maybe RpcRequest) `shouldBe` Nothing

        it "encodes 'method' field correctly" $ do
            let req = RpcRequest "2.0" "initialize" Nothing Nothing
            case decode (encode req) :: Maybe Value of
                Just (Object m) ->
                    KM.lookup "method" m `shouldBe` Just (String "initialize")
                _ -> expectationFailure "expected a JSON object"

    describe "RpcResponse" $ do

        it "round-trips through JSON" $ do
            -- Note: Maybe Value fields with Just Null encode to JSON null,
            -- which Aeson's generic instance decodes back as Nothing.
            -- We use a concrete value to test roundtrip fidelity.
            let res = RpcResponse "2.0" (Just (Number 1)) Nothing (Just (Number 42))
            decode (encode res) `shouldBe` Just res

        it "round-trips with null result" $ do
            let res = RpcResponse "2.0" Nothing Nothing Nothing
            decode (encode res) `shouldBe` Just res

        it "serializes resError field" $ do
            let errV = object [("code" .= ((-32600) :: Int)), ("message" .= ("Invalid" :: String))]
                res  = RpcResponse "2.0" Nothing (Just errV) (Just (Number 1))
            case decode (encode res) :: Maybe Value of
                Just (Object m) ->
                    KM.lookup "error" m `shouldNotBe` Nothing
                _ -> expectationFailure "expected a JSON object"
