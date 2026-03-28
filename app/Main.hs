{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Network.Wai.Handler.Warp (run)

import App (app)
import Redis (initRedis, createRediSearchIndex)
import Types (ServerState (..))

main :: IO ()
main = do
    putStrLn "Initializing HEngram: Pure Functional MCP Server..."

    conn <- initRedis
    putStrLn "Connected to Redis."

    indexResult <- createRediSearchIndex conn
    case indexResult of
        Left  _ -> putStrLn "RediSearch index already exists (or Redis error) — continuing."
        Right _ -> putStrLn "RediSearch index 'engram_index' created (384-dim COSINE FLOAT32)."

    sessionMap <- newTVarIO Map.empty
    let state = ServerState conn sessionMap
        port  = 8765 :: Int

    putStrLn $ "HEngram serving on http://localhost:" <> show port
    run port (app state)
