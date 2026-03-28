{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Data.Aeson (encode)
import Data.Binary.Builder (Builder, fromLazyByteString, fromByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Wai (Middleware, pathInfo)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant

import Types
import Redis

-- ── AppM natural transformation ─────────────────────────────────────────────

nt :: ServerState -> AppM a -> Handler a
nt state h = runReaderT h state

-- ── /messages Servant handler ────────────────────────────────────────────────

messagesHandler :: SessionId -> RpcRequest -> AppM NoContent
messagesHandler sid req = do
    state <- ask
    mQueue <- liftIO $ atomically $ Map.lookup sid <$> readTVar (sessions state)
    case mQueue of
        Nothing     -> throwError err404 { errBody = "Session not found" }
        Just queue  -> do
            let payload  = fromLazyByteString (encode req)
                nameBldr = fromByteString "message" :: Builder
                event    = ServerEvent (Just nameBldr) Nothing [payload]
            liftIO $ atomically $ writeTQueue queue event
            return NoContent

-- ── Servant application (/messages only) ────────────────────────────────────

servantApp :: ServerState -> Application
servantApp state =
    serve messagesProxy $
        hoistServer messagesProxy (nt state) messagesHandler

-- ── Raw WAI handler for GET /sse ─────────────────────────────────────────────
--
-- Uses eventSourceAppIO with a TQueue so:
--   1. We can atomically write the handshake before the client starts reading.
--   2. readTQueue blocks cheaply (no busy-wait) until the next event arrives.

sseApp :: ServerState -> Application
sseApp state _req cb = do
    sessionId <- toText <$> nextRandom
    queue     <- newTQueueIO

    -- Write the session_id handshake BEFORE registering or streaming,
    -- so it is guaranteed to be the first item the client dequeues.
    let nameBldr = fromByteString "session_id" :: Builder
        dataBldr = fromByteString (TE.encodeUtf8 sessionId) :: Builder
    atomically $ writeTQueue queue (ServerEvent (Just nameBldr) Nothing [dataBldr])

    -- Register the queue so /messages can push into it
    atomically $ modifyTVar' (sessions state) (Map.insert sessionId queue)

    -- eventSourceAppIO calls the IO action repeatedly to get the next event;
    -- readTQueue blocks until one is available (no spinning).
    eventSourceAppIO (atomically $ readTQueue queue) _req cb

-- ── WAI Router ───────────────────────────────────────────────────────────────

router :: ServerState -> Application
router state req cb =
    case pathInfo req of
        ["sse"] -> sseApp state req cb
        _       -> servantApp state req cb

-- ── CORS middleware ──────────────────────────────────────────────────────────

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
        { corsOrigins        = Nothing
        , corsMethods        = ["GET", "POST", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization"]
        }

-- ── Main ─────────────────────────────────────────────────────────────────────

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
    run port $ corsPolicy $ router state
