{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Embeddings
    ( withLlamaServer
    , getEmbedding
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson (FromJSON(..), (.:), withObject, (.=), object)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Process (CreateProcess(..), createProcess, proc, terminateProcess, waitForProcess, StdStream(..), callProcess)

-- | Supervisor for llama-server lifecycle.
withLlamaServer :: IO a -> IO a
withLlamaServer action = do
    ensureLlamaServer
    putStrLn "Spawning llama-server (embedding mode)..."
    let serverProc = proc "env"
            [ "LD_LIBRARY_PATH=./bin"
            , "./bin/llama-server"
            , "-m", "./models/nomic-embed-text-v1.5.f16.gguf"
            , "--port", "8080"
            , "--embedding"
            , "--log-disable"
            ]
    -- Using bracket to ensure process is terminated on exit or exception.
    bracket (createProcess serverProc { std_out = Inherit, std_err = Inherit })
            (\(_, _, _, ph) -> do
                putStrLn "Terminating llama-server..."
                terminateProcess ph
                _ <- waitForProcess ph
                return ())
            (\_ -> do
                putStrLn "Waiting for llama-server to warm up (5s)..."
                threadDelay 5000000
                action)

-- | Ensures local ./bin/llama-server and model exist.
ensureLlamaServer :: IO ()
ensureLlamaServer = do
    createDirectoryIfMissing True "bin"
    createDirectoryIfMissing True "models"

    binExists <- doesFileExist "./bin/llama-server"
    unless binExists $ do
        putStrLn "Downloading llama-server (b4610)..."
        let url = "https://github.com/ggml-org/llama.cpp/releases/download/b4610/llama-b4610-bin-ubuntu-x64.zip"
        callProcess "curl" ["-L", "-o", "llama.zip", url]
        putStrLn "Extracting llama-server and dependencies..."
        callProcess "unzip" ["-o", "-j", "llama.zip", "-d", "bin/"]
        removeFile "llama.zip"
        callProcess "chmod" ["+x", "bin/llama-server"]

    modelExists <- doesFileExist "./models/nomic-embed-text-v1.5.f16.gguf"
    unless modelExists $ do
        putStrLn "Downloading nomic-embed-text-v1.5.f16.gguf..."
        let mUrl = "https://huggingface.co/nomic-ai/nomic-embed-text-v1.5-GGUF/resolve/main/nomic-embed-text-v1.5.f16.gguf"
        callProcess "curl" ["-L", "-o", "./models/nomic-embed-text-v1.5.f16.gguf", mUrl]

-- | JSON Response structure for native llama-server Embedding API.
newtype EmbeddingResponse = EmbeddingResponse { embedding :: [Float] } deriving (Show, Generic)

instance FromJSON EmbeddingResponse where
    parseJSON = withObject "EmbeddingResponse" $ \v -> do
        EmbeddingResponse <$> v .: "embedding"

-- | Fetch embedding from the local llama-server.
getEmbedding :: Text -> IO [Float]
getEmbedding txt = do
    let request
          = setRequestPath "/embedding"
          $ setRequestMethod "POST"
          $ setRequestBodyJSON (object [ "content" .= txt ])
          $ "http://localhost:8080"

    response <- httpJSON request
    return $ embedding (getResponseBody response)
