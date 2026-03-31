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
import Data.Aeson (encode, (.=), object, Value(..), Object)
import qualified Data.Aeson as Aeson
import Data.Binary.Builder (Builder, fromLazyByteString, fromByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Wai (Middleware, pathInfo)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.Wai.Middleware.Cors
import Servant

import Types
import Embeddings (getEmbedding)
import Redis (normalizeL2, vectorToByteString, linkNodes)
import Database.Redis (runRedis, sendRequest, Reply(..), Redis, Connection, exists)

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
                                , object
                                    [ "name" .= ("engram_recall" :: Text)
                                    , "description" .= ("Retrieve a specific node from the knowledge graph by ID, including its full content and all typed outgoing relationships" :: Text)
                                    , "inputSchema" .= object
                                        [ "type" .= ("object" :: Text)
                                        , "properties" .= object
                                            [ "node_id" .= object ["type" .= ("string" :: Text)]
                                            ]
                                        , "required" .= (["node_id"] :: [Text])
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

canonicalNodeTypes :: [Text]
canonicalNodeTypes =
  [ "fact", "preference", "project", "person", "decision"
  , "constraint", "event", "concept", "procedure", "summary" ]

canonicalDomains :: [Text]
canonicalDomains =
  [ "personal", "swe", "infra", "health", "finance"
  , "recon", "fitness", "research", "misc" ]

domainAliases :: [(Text, Text)]
domainAliases =
  [ ("software", "swe"), ("software engineering", "swe"), ("dev", "swe")
  , ("coding", "swe"), ("programacao", "swe"), ("desenvolvimento", "swe")
  , ("infrastructure", "infra"), ("servidor", "infra"), ("server", "infra")
  , ("saude", "health"), ("treino", "fitness"), ("gym", "fitness")
  , ("bodybuilding", "fitness"), ("academia", "fitness")
  , ("seguranca", "recon"), ("security", "recon"), ("pentest", "recon")
  , ("pesquisa", "research"), ("estudo", "research")
  , ("geral", "misc"), ("general", "misc"), ("outros", "misc")
  ]

overlapScore :: Text -> Text -> Double
overlapScore t1 t2 =
    let s1 = T.unpack $ T.toLower $ T.strip t1
        s2 = T.unpack $ T.toLower $ T.strip t2
        shared = length [c | c <- s1, c `elem` s2]
        maxLen = max (length s1) (length s2)
    in if maxLen == 0 then 0 else fromIntegral shared / fromIntegral maxLen

normalizeNodeType :: Text -> Text
normalizeNodeType input =
    let normalized = T.toLower $ T.strip input
    in if normalized `elem` canonicalNodeTypes
       then normalized
       else 
           let scores = [(overlapScore normalized c, c) | c <- canonicalNodeTypes]
               bestMatch = snd $ maximumBy (comparing fst) scores
           in bestMatch

normalizeDomain :: Text -> Text
normalizeDomain input =
    let normalized = T.toLower $ T.strip input
    in case lookup normalized domainAliases of
        Just alias -> alias
        Nothing -> 
            if normalized `elem` canonicalDomains
            then normalized
            else
                let scores = [(overlapScore normalized c, c) | c <- canonicalDomains]
                    bestMatch = snd $ maximumBy (comparing fst) scores
                in bestMatch

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
                    let normDom   = normalizeDomain dom
                        normType  = normalizeNodeType nType
                    rawVec <- liftIO $ getEmbedding cont
                    let vec = vectorToByteString $ normalizeL2 $ take 384 rawVec
                    
                    -- Task 4: Semantic Deduplication
                    let dedupQuery = "(@domain:{" <> normDom <> "}) [KNN 3 @vector $vec AS __dup_score__]"
                    dedupReply <- liftIO $ runRedis (redisConn state) $
                        sendRequest ["FT.SEARCH", "engram_index", TE.encodeUtf8 dedupQuery, "PARAMS", "2", "vec", vec, "SORTBY", "__dup_score__", "ASC", "LIMIT", "0", "1", "DIALECT", "2"]
                    
                    let mDuplicate = case dedupReply of
                            Right (MultiBulk (Just (Bulk (Just count) : Bulk (Just key) : MultiBulk (Just fields) : _))) | count /= "0" ->
                                let sText = lookupHGetAll "__dup_score__" fields
                                in case readMaybe (T.unpack sText) of
                                    Just s  -> Just (TE.decodeUtf8 key, s)
                                    _       -> Nothing
                            _ -> Nothing
                    
                    case mDuplicate of
                        Just (dupId, score) | score < (dedupThreshold state) -> do
                            let res = toolSuccess req $ "Duplicate detected: this knowledge already exists as node " <> dupId <> " (similarity score: " <> T.pack (show score) <> "). Use engram_link to connect related concepts or engram_recall to inspect the existing node."
                            liftIO $ atomically $ writeTQueue queue (mkEvent res)
                        _ -> do
                            uuid <- liftIO nextRandom
                            let key = "engram:node:" <> toText uuid
                            liftIO $ runRedis (redisConn state) $ do
                                _ <- (sendRequest ["HSET", TE.encodeUtf8 key, "domain", TE.encodeUtf8 normDom, "type", TE.encodeUtf8 normType, "content", TE.encodeUtf8 cont, "vector", vec] :: Redis (Either Reply Reply))
                                _ <- (sendRequest ["SADD", "manifest:domain:" <> TE.encodeUtf8 normDom, TE.encodeUtf8 key] :: Redis (Either Reply Reply))
                                return ()
                            let res = toolSuccess req $ "Successfully memorized into domain '" <> normDom <> "' with node_type '" <> normType <> "' and ID " <> toText uuid
                            liftIO $ atomically $ writeTQueue queue (mkEvent res)
                Nothing -> sendError queue req "Missing required arguments for engram_memorize"
        
        Just ("engram_search", args) -> do
            let mQuery = getString "query" args
            case mQuery of
                Just query -> do
                    rawVec <- liftIO $ getEmbedding query
                    let vec = vectorToByteString $ normalizeL2 $ take 384 rawVec
                        domain = getString "domain" args
                        
                        -- Phase 1 search
                        hybridQuery1 = case domain of
                            Just d  -> "(@domain:{" <> d <> "}) ((@content:($q)) | ([KNN 5 @vector $vec AS score]))"
                            Nothing -> "(@content:($q)) | ([KNN 5 @vector $vec AS score])"
                    
                    reply1 <- liftIO $ runRedis (redisConn state) $
                        sendRequest ["FT.SEARCH", "engram_index", TE.encodeUtf8 hybridQuery1, "PARAMS", "4", "q", TE.encodeUtf8 query, "vec", vec, "DIALECT", "2"]
                    
                    (finalResults, fUsed) <- case reply1 of
                        Right (MultiBulk (Just (Bulk (Just count) : resList))) | count /= "0" -> do
                            rs <- liftIO $ parseHybridResults (redisConn state) resList
                            return (rs, False)
                        _ | isJust domain -> do
                            -- Phase 2 search: Fallback to global
                            let hybridQuery2 = "(@content:($q)) | ([KNN 5 @vector $vec AS score])"
                            reply2 <- liftIO $ runRedis (redisConn state) $
                                sendRequest ["FT.SEARCH", "engram_index", TE.encodeUtf8 hybridQuery2, "PARAMS", "4", "q", TE.encodeUtf8 query, "vec", vec, "DIALECT", "2"]
                            case reply2 of
                                Right (MultiBulk (Just (Bulk (Just _) : resList2))) -> do
                                    rs <- liftIO $ parseHybridResults (redisConn state) resList2
                                    return (rs, True)
                                _ -> return ([], False)
                        Right (MultiBulk (Just (Bulk (Just _) : resList3))) -> do
                                rs <- liftIO $ parseHybridResults (redisConn state) resList3
                                return (rs, False)
                        _ -> return ([], False)

                    let searchResp = SearchResponse fUsed finalResults
                        jsonRes = T.pack $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode searchResp
                        res = toolSuccess req jsonRes
                    liftIO $ atomically $ writeTQueue queue (mkEvent res)
                Nothing -> sendError queue req "Missing required argument 'query'"
        
        Just ("engram_recall", args) -> do
            let mNodeId = getString "node_id" args
            case mNodeId of
                Just nodeId -> do
                    let key = TE.encodeUtf8 nodeId
                    reply <- liftIO $ runRedis (redisConn state) $ sendRequest ["HGETALL", key]
                    case reply of
                        Right (MultiBulk (Just fields)) | not (null fields) -> do
                            let cont  = lookupHGetAll "content" fields
                                dom   = lookupHGetAll "domain" fields
                                nType = lookupHGetAll "type" fields
                            edges <- liftIO $ fetchEdges (redisConn state) key
                            let recallResult = SearchResult nodeId dom nType cont Nothing edges
                                jsonRes = T.pack $ T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode [recallResult]
                                res = toolSuccess req jsonRes
                            liftIO $ atomically $ writeTQueue queue (mkEvent res)
                        _ -> sendError queue req $ "Node not found: " <> nodeId
                Nothing -> sendError queue req "Missing required argument 'node_id'"
        
        Just ("engram_link", args) -> do
            let mLink = (,,) <$> (getString "source_id" args)
                             <*> (getString "target_id" args)
                             <*> (getString "rel_type" args)
            case mLink of
                Just (src, tgt, rel) -> do
                    let srcKey = TE.encodeUtf8 src
                        tgtKey = TE.encodeUtf8 tgt
                    
                    existsReply <- liftIO $ runRedis (redisConn state) $ do
                        e1 <- exists srcKey
                        e2 <- exists tgtKey
                        return (e1, e2)
                    
                    case existsReply of
                        (Right True, Right True) -> do
                            _ <- liftIO $ linkNodes (redisConn state) srcKey tgtKey (TE.encodeUtf8 rel)
                            let res = toolSuccess req $ "Successfully linked " <> src <> " -> " <> tgt <> " (" <> rel <> ")"
                            liftIO $ atomically $ writeTQueue queue (mkEvent res)
                        (Right False, Right False) -> sendError queue req "engram_link failed: neither source nor target node exists"
                        (Right False, _) -> sendError queue req $ "engram_link failed: source node does not exist: " <> src
                        (_, Right False) -> sendError queue req $ "engram_link failed: target node does not exist: " <> tgt
                        _ -> sendError queue req "engram_link failed: Redis error during existence check"
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

-- | Parse FT.SEARCH results into SearchResult objects
parseHybridResults :: Connection -> [Reply] -> IO [SearchResult]
parseHybridResults _ [] = return []
parseHybridResults conn replies = mapM (parseOne conn) (pairs replies)
  where
    pairs [] = []
    pairs (i:f:rs) = (i,f) : pairs rs
    pairs _ = []

    parseOne c (Bulk (Just key), MultiBulk (Just fields)) = do
        let cont  = lookupHGetAll "content" fields
            dom   = lookupHGetAll "domain" fields
            nType = lookupHGetAll "type" fields
            sText = lookupHGetAll "score" fields
            score = case reads (T.unpack sText) of
                [(val, "")] -> Just val
                _           -> Nothing
        edges <- fetchEdges c key
        return $ SearchResult (TE.decodeUtf8 key) dom nType cont score edges
    parseOne _ _ = return $ SearchResult "malformed" "" "" "" Nothing []

-- | Fetch all edges for a node
fetchEdges :: Connection -> ByteString -> IO [RelatedEdge]
fetchEdges conn key = do
    reply <- runRedis conn $ sendRequest ["SMEMBERS", "engram:edges:" <> key]
    case reply of
        Right (MultiBulk (Just ms)) -> do
            let targets = [tid | Bulk (Just tid) <- ms]
            mapM (fetchEdge conn key) targets
        _ -> return []

-- | Fetch relationship type for a specific edge
fetchEdge :: Connection -> ByteString -> ByteString -> IO RelatedEdge
fetchEdge conn src tgt = do
    reply <- runRedis conn $ sendRequest ["HGET", "engram:edge:data:" <> src <> ":" <> tgt, "type"]
    let relType = case reply of
            Right (Bulk (Just t)) -> TE.decodeUtf8 t
            _                     -> "RELATED"
    return $ RelatedEdge (TE.decodeUtf8 tgt) relType

-- | Generic HSET/FT.SEARCH field lookup
lookupHGetAll :: ByteString -> [Reply] -> Text
lookupHGetAll _ [] = ""
lookupHGetAll target (Bulk (Just f) : Bulk (Just v) : fs)
    | f == target = TE.decodeUtf8 v
    | otherwise   = lookupHGetAll target fs
lookupHGetAll t (_:fs) = lookupHGetAll t fs

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
