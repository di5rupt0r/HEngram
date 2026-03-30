{-# LANGUAGE OverloadedStrings #-}

module Redis where

import Control.Exception (SomeException, try, throwIO, Exception)
import Data.Binary.Put (runPut, putFloatle)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Database.Redis

-- | Custom exception for Redis connectivity failures
newtype RedisConnectionError = RedisConnectionError String
    deriving (Show)

instance Exception RedisConnectionError

-- | Initialize Redis connection. Throws RedisConnectionError if Redis is unreachable.
initRedis :: IO Connection
initRedis = do
    result <- try (checkedConnect defaultConnectInfo) :: IO (Either SomeException Connection)
    case result of
        Left ex   -> throwIO $ RedisConnectionError $
            "Cannot connect to Redis at localhost:6379 — is Redis running?\n" <>
            "  Start it with: sudo systemctl start redis\n" <>
            "  Original error: " <> show ex
        Right conn -> return conn

-- | Create the RediSearch index with a 384-dim COSINE FLOAT32 VECTOR field.
-- Uses raw sendRequest since hedis has no native FT.CREATE wrapper.
-- Returns Left on error, Right on success.
createRediSearchIndex :: Connection -> IO (Either Reply Reply)
createRediSearchIndex conn = runRedis conn $ sendRequest
    [ "FT.CREATE", "engram_index"
    , "ON",     "HASH"
    , "PREFIX", "1", "engram:node:"
    , "SCHEMA"
    , "content", "TEXT"
    , "domain",  "TAG"
    , "vector",  "VECTOR", "FLAT", "6"
    ,   "TYPE",            "FLOAT32"
    ,   "DIM",             "384"
    ,   "DISTANCE_METRIC", "COSINE"
    ]

-- | Link two nodes with a relationship type.
linkNodes :: Connection -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either Reply Reply)
linkNodes conn source target relType = runRedis conn $ do
    _ <- (sendRequest ["SADD", "engram:edges:" <> source, target] :: Redis (Either Reply Reply))
    sendRequest ["HSET", "engram:edge:data:" <> source <> ":" <> target, "type", relType]
-- | L2 normalization for a vector of floats.
normalizeL2 :: [Float] -> [Float]
normalizeL2 xs =
    let mag = sqrt (sum (map (\x -> x * x) xs))
    in if mag < 1e-9 then xs else map (/ mag) xs

-- | Encode a [Float] into a strict ByteString of little-endian FLOAT32s.
vectorToByteString :: [Float] -> BS.ByteString
vectorToByteString = LBS.toStrict . runPut . mapM_ putFloatle
