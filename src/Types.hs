{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Concurrent.STM (TVar, TQueue)
import Database.Redis (Connection)
import Network.Wai.EventSource (ServerEvent)
import Servant
import Control.Monad.Reader (ReaderT)

-- | Unique identifier for an SSE session
type SessionId = Text

-- | JSON-RPC 2.0 Request structure
data RpcRequest = RpcRequest
    { jsonrpc :: !Text
    , method  :: !Text
    , params  :: !(Maybe Value)
    , rpcId   :: !(Maybe Value)
    } deriving (Generic, Show)

instance FromJSON RpcRequest
instance ToJSON RpcRequest

-- | JSON-RPC 2.0 Response structure
data RpcResponse = RpcResponse
    { resJsonrpc :: !Text
    , result     :: !(Maybe Value)
    , resError   :: !(Maybe Value)
    , resId      :: !(Maybe Value)
    } deriving (Generic, Show)

instance FromJSON RpcResponse
instance ToJSON RpcResponse

-- | Server State record (shared across all handlers)
data ServerState = ServerState
    { redisConn :: !Connection
    , sessions  :: !(TVar (Map SessionId (TQueue ServerEvent)))
    }

-- | Application Monad: Reader over ServerState, lifts into Servant Handler
type AppM = ReaderT ServerState Handler

-- | Servant API — only /messages here; /sse is a raw WAI handler
type MessagesAPI =
    "messages"
        :> QueryParam' '[Required, Strict] "session_id" SessionId
        :> ReqBody '[JSON] RpcRequest
        :> PostAccepted '[JSON] NoContent

messagesProxy :: Proxy MessagesAPI
messagesProxy = Proxy
