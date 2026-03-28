{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RedisSpec (spec) where

import Control.Exception (try, SomeException)
import Database.Redis (runRedis, sendRequest, Reply, Redis, Connection)
import Test.Hspec

import Redis

spec :: Spec
spec = do
    describe "initRedis" $ do
        it "connects to a running Redis without throwing" $ do
            result <- try initRedis :: IO (Either SomeException Connection)
            case result of
                Left  e -> expectationFailure $ "initRedis threw: " <> show e
                Right _ -> return ()

    describe "createRediSearchIndex" $ do
        it "is idempotent — calling twice does not throw" $ do
            conn <- initRedis
            _    <- createRediSearchIndex conn
            r2   <- createRediSearchIndex conn
            -- Either Left (index exists error) or Right (created) is acceptable
            case (r2 :: Either Reply Reply) of
                Left  _ -> return ()
                Right _ -> return ()

        it "creates an index inspectable via FT.INFO" $ do
            conn <- initRedis
            _    <- createRediSearchIndex conn
            let ftInfo = sendRequest ["FT.INFO", "engram_index"]
            info <- runRedis conn (ftInfo :: Redis (Either Reply Reply))
            case info of
                Left  _ -> expectationFailure "FT.INFO returned an error; index may not exist"
                Right _ -> return ()
