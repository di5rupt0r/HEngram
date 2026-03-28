{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core WAI application logic, separated from Main so tests can import it.
module App
    ( app         -- ^ Full application: CORS + router
    , router      -- ^ Raw WAI router (no CORS, useful in tests)
    ) where

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
import Network.Wai.Middleware.Cors
import Servant

import Types

-- ── AppM natural transformation ─────────────────────────────────────────────

nt :: ServerState -> AppM a -> Handler a
nt state h = runReaderT h state

-- ── /messages Servant handler ────────────────────────────────────────────────

messagesHandler :: SessionId -> RpcRequest -> AppM NoContent
messagesHandler sid req = do
    state  <- ask
    mQueue <- liftIO $ atomically $ Map.lookup sid <$> readTVar (sessions state)
    case mQueue of
        Nothing     -> throwError err404 { errBody = "Session not found" }
        Just queue  -> do
            let payload  = fromLazyByteString (encode req)
                nameBldr = fromByteString "message" :: Builder
                event    = ServerEvent (Just nameBldr) Nothing [payload]
            liftIO $ atomically $ writeTQueue queue event
            return NoContent

-- ── Servant sub-application (/messages) ─────────────────────────────────────

servantApp :: ServerState -> Application
servantApp state =
    serve messagesProxy $
        hoistServer messagesProxy (nt state) messagesHandler

-- ── SSE WAI handler (GET /sse) ───────────────────────────────────────────────

sseApp :: ServerState -> Application
sseApp state _req cb = do
    sessionId <- toText <$> nextRandom
    queue     <- newTQueueIO

    let nameBldr = fromByteString "session_id" :: Builder
        dataBldr = fromByteString (TE.encodeUtf8 sessionId) :: Builder
    atomically $ writeTQueue queue (ServerEvent (Just nameBldr) Nothing [dataBldr])
    atomically $ modifyTVar' (sessions state) (Map.insert sessionId queue)

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

-- ── Full Application ─────────────────────────────────────────────────────────

app :: ServerState -> Application
app state = corsPolicy $ router state
