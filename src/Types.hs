module Types 
    ( User(..)
    , Message(..)
    , MessageType(..)
    , Topic
    ) where

import Control.Concurrent.STM (TQueue, TVar)
import Data.Set (Set)
import Data.Map (Map)

-- | Represents a simulated user in the network.
data User = User
    { userName              :: String
    , userInboxUrgent       :: TQueue Message      -- ^ Priority 1: Friend Requests
    , userInboxNormal       :: TQueue Message      -- ^ Priority 2: General Chat
    , userMessagesSent      :: TVar Int            -- ^ Metric: Total messages sent
    , userCount             :: TVar Int            -- ^ Metric: Total messages received
    , userFriends           :: TVar (Set String)   -- ^ Dynamic Friend Graph
    , userSecrets           :: TVar (Set String)   -- ^ Known gossip secrets
    , userRatingsReceived   :: TVar [Int]          -- ^ Trust ratings (0-10) from others
    }

-- | Type of message sent between users.
data MessageType 
    = Regular           -- ^ Standard chat/gossip
    | FriendRequest     -- ^ Urgent request to connect
    | FriendAccept      -- ^ Confirmation of connection
    deriving (Show, Eq)

type Topic = String

-- | The core Message payload.
data Message = Message
    { sender     :: String
    , content    :: String
    , msgType    :: MessageType
    , msgTopic   :: Topic
    , msgSecrets :: [String] -- ^ Piggybacked gossip
    } deriving (Show, Eq)
