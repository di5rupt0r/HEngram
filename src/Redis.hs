{-# LANGUAGE OverloadedStrings #-}

module Redis where

import Control.Exception (SomeException, try, throwIO, Exception)
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
    , "PREFIX", "1", "engram:"
    , "SCHEMA"
    , "content", "TEXT"
    , "vector",  "VECTOR", "FLAT", "6"
    ,   "TYPE",            "FLOAT32"
    ,   "DIM",             "384"
    ,   "DISTANCE_METRIC", "COSINE"
    ]
