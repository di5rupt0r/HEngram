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
import Data.Aeson (encode, FromJSON(..), (.:), withObject, (.=), object, Value(..), Object)
import Data.Binary.Builder (Builder, fromLazyByteString, fromByteString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.UUID (toText, fromText)
import Data.UUID.V4 (nextRandom)
import Network.Wai (Middleware, pathInfo)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.Wai.Middleware.Cors
import Servant

import Types
import Embeddings (getEmbedding)
import Redis (normalizeL2, vectorToByteString, linkNodes)
import Database.Redis (runRedis, sendRequest, Reply(..), Redis, Connection)

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
            let mResponse = case method req of
                    "ping" -> Just $ RpcResponse "2.0" (Just (object [])) Nothing (rpcId req)
                    "initialize" -> Just $ RpcResponse "2.0"
                        (Just (object
                            [ "protocolVersion" .= ("2024-11-05" :: Text)
                            , "capabilities" .= object []
                            , "serverInfo" .= object ["name" .= ("hengram" :: Text), "version" .= ("0.1.0" :: Text)]
                            ]))
                        Nothing (rpcId req)
                    "tools/list" -> Just $ RpcResponse "2.0"
                        (Just (object
                            [ "tools" .=
                                [ object
                                    [ "name" .= ("engram_memorize" :: Text)
                                    , "description" .= ("Memorize a piece of information into the knowledge graph" :: Text)
                                    , "inputSchema" .= object
                                        [ "type" .= ("object" :: Text)
                                        , "properties" .= object
                                            [ "domain"   .= object ["type" .= ("string" :: Text)]
                                            , "content"  .= object ["type" .= ("string" :: Text)]
                                            , "node_type" .= object ["type" .= ("string" :: Text)]
                                            ]
                                        , "required" .= (["domain", "content", "node_type"] :: [Text])
                                        ]
                                    ]
                                , object
                                    [ "name" .= ("engram_search" :: Text)
                                    , "description" .= ("Search for information in the knowledge graph" :: Text)
                                    , "inputSchema" .= object
                                        [ "type" .= ("object" :: Text)
                                        , "properties" .= object
                                            [ "query"  .= object ["type" .= ("string" :: Text)]
                                            , "domain" .= object ["type" .= ("string" :: Text)]
                                            ]
                                        , "required" .= (["query"] :: [Text])
                                        ]
                                    ]
                                , object
                                    [ "name" .= ("engram_link" :: Text)
                                    , "description" .= ("Create a relationship between two nodes in the knowledge graph" :: Text)
                                    , "inputSchema" .= object
                                        [ "type" .= ("object" :: Text)
                                        , "properties" .= object
                                            [ "source_id" .= object ["type" .= ("string" :: Text)]
                                            , "target_id" .= object ["type" .= ("string" :: Text)]
                                            , "rel_type"  .= object ["type" .= ("string" :: Text)]
                                            ]
                                        , "required" .= (["source_id", "target_id", "rel_type"] :: [Text])
                                        ]
                                    ]
                                ]
                            ]))
                        Nothing (rpcId req)
                    "tools/call" -> Nothing -- Handled below as it requires IO
                    "notifications/initialized" -> Nothing
                    _ -> case rpcId req of
                        Nothing -> Nothing -- It's a notification, do not reply
                        Just idVal -> Just $ RpcResponse "2.0" Nothing
                            (Just (object [ "code" .= (-32601 :: Int), "message" .= ("Method not found" :: String) ]))
                            (Just idVal)

            case mResponse of
                Just res -> liftIO $ atomically $ writeTQueue queue (mkEvent res)
                Nothing -> case method req of
                    "tools/call" -> handleToolCall state queue req
                    _ -> return ()
            
            return NoContent

-- ── Tool Call Dispatcher ──────────────────────────────────────────────────

handleToolCall :: ServerState -> TQueue ServerEvent -> RpcRequest -> AppM ()
handleToolCall state queue req = do
    let mArgs = do
            Object p <- params req
            String name <- KM.lookup (K.fromText "name") p
            Object args <- KM.lookup (K.fromText "arguments") p
            return (name, args)
    
    case mArgs of
        Just ("engram_memorize", args) -> do
            let mData = (,,) <$> (getString "domain" args)
                             <*> (getString "content" args)
                             <*> (getString "node_type" args)
            case mData of
                Just (dom, cont, nType) -> do
                    rawVec <- liftIO $ getEmbedding cont
                    let vec = vectorToByteString $ normalizeL2 $ take 384 rawVec
                    uuid <- liftIO nextRandom
                    let key = "engram:node:" <> toText uuid
                    liftIO $ runRedis (redisConn state) $ do
                        _ <- (sendRequest ["HSET", TE.encodeUtf8 key, "domain", TE.encodeUtf8 dom, "type", TE.encodeUtf8 nType, "content", TE.encodeUtf8 cont, "vector", vec] :: Redis (Either Reply Reply))
                        _ <- (sendRequest ["SADD", "manifest:domain:" <> TE.encodeUtf8 dom, TE.encodeUtf8 key] :: Redis (Either Reply Reply))
                        return ()
                    let res = toolSuccess req $ "Successfully memorized into domain '" <> dom <> "' with ID " <> toText uuid
                    liftIO $ atomically $ writeTQueue queue (mkEvent res)
                Nothing -> sendError queue req "Missing required arguments for engram_memorize"
        
        Just ("engram_search", args) -> do
            let mQuery = getString "query" args
            case mQuery of
                Just query -> do
                    rawVec <- liftIO $ getEmbedding query
                    let vec = vectorToByteString $ normalizeL2 $ take 384 rawVec
                        domain = getString "domain" args
                        hybridQuery = case domain of
                            Just d  -> "(@domain:{" <> d <> "}) ((@content:($q)) | ([KNN 5 @vector $vec AS score]))"
                            Nothing -> "(@content:($q)) | ([KNN 5 @vector $vec AS score])"
                    
                    reply <- liftIO $ runRedis (redisConn state) $
                        sendRequest ["FT.SEARCH", "engram_index", TE.encodeUtf8 hybridQuery, "PARAMS", "4", "q", TE.encodeUtf8 query, "vec", vec, "DIALECT", "2"]
                    
                    case reply of
                        Right (MultiBulk (Just (Bulk (Just _count) : results))) -> do
                            -- Parse results and fetch edges for each
                            formatted <- liftIO $ formatHybridResults (redisConn state) results
                            let res = toolSuccess req formatted
                            liftIO $ atomically $ writeTQueue queue (mkEvent res)
                        _ -> sendError queue req "RediSearch hybrid query failed"
                Nothing -> sendError queue req "Missing required argument 'query'"
        
        Just ("engram_link", args) -> do
            let mLink = (,,) <$> (getString "source_id" args)
                             <*> (getString "target_id" args)
                             <*> (getString "rel_type" args)
            case mLink of
                Just (src, tgt, rel) -> do
                    liftIO $ linkNodes (redisConn state) (TE.encodeUtf8 src) (TE.encodeUtf8 tgt) (TE.encodeUtf8 rel)
                    let res = toolSuccess req $ "Successfully linked " <> src <> " -> " <> tgt <> " (" <> rel <> ")"
                    liftIO $ atomically $ writeTQueue queue (mkEvent res)
                Nothing -> sendError queue req "Missing required arguments for engram_link"
        
        _ -> sendError queue req "Invalid tool name or arguments"

-- ── Helpers ──────────────────────────────────────────────────────────────────

mkEvent :: RpcResponse -> ServerEvent
mkEvent res =
    let payload  = fromLazyByteString (encode res)
        nameBldr = fromByteString "message" :: Builder
    in ServerEvent (Just nameBldr) Nothing [payload]

getString :: Text -> Object -> Maybe Text
getString k obj = case KM.lookup (K.fromText k) obj of
    Just (String s) -> Just s
    _               -> Nothing

toolSuccess :: RpcRequest -> Text -> RpcResponse
toolSuccess req txt = RpcResponse "2.0"
    (Just (object [ "content" .= [object ["type" .= ("text" :: Text), "text" .= txt]], "isError" .= False ]))
    Nothing
    (rpcId req)

sendError :: TQueue ServerEvent -> RpcRequest -> Text -> AppM ()
sendError queue req msg = do
    let res = RpcResponse "2.0" Nothing
            (Just (object [ "code" .= (-32602 :: Int), "message" .= msg ]))
            (rpcId req)
    liftIO $ atomically $ writeTQueue queue (mkEvent res)

formatHybridResults :: Database.Redis.Connection -> [Reply] -> IO Text
formatHybridResults _ [] = return "No results found."
formatHybridResults conn replies = do
    formattedList <- mapM (formatOne conn) (pairs replies)
    return $ T.unlines formattedList
  where
    pairs [] = []
    pairs (i:f:rs) = (i,f) : pairs rs
    pairs _ = []

    formatOne c (Bulk (Just key), MultiBulk (Just fields)) = do
        let content = lookupField "content" fields
            score   = lookupField "score" fields
        -- Fetch Edges
        edgeReply <- runRedis c $ sendRequest ["SMEMBERS", "engram:edges:" <> key]
        let related = case edgeReply of
                Right (MultiBulk (Just ms)) -> 
                    let ids = [TE.decodeUtf8 bid | Bulk (Just bid) <- ms]
                    in if null ids then "" else "\n   └ Related: " <> T.intercalate ", " ids
                _ -> ""
        return $ "[" <> TE.decodeUtf8 key <> "] (Score: " <> score <> "): " <> content <> related
    formatOne _ _ = return "Malformed result"

    lookupField _ [] = "n/a"
    lookupField target (Bulk (Just f) : Bulk (Just v) : fs)
        | f == target = TE.decodeUtf8 v
        | otherwise   = lookupField target fs
    lookupField t (_:fs) = lookupField t fs

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

    let nameBldr = fromByteString "endpoint" :: Builder
        dataBldr = fromByteString (TE.encodeUtf8 $ "/messages?session_id=" <> sessionId) :: Builder
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
