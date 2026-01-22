module Types
  ( User (..),
    Message (..),
    MessageType (..),
    Topic,
    Personality (..),
  )
where

import Control.Concurrent.STM (TQueue, TVar)
import Data.Set (Set)

-- import Data.Map (Map) -- Will use later for Env

type Topic = String

-- | Defines the behavior profile of a user.
data Personality
  = -- | Posts rarely, reads more
    Introvert
  | -- | Posts frequently, connects often
    Extrovert
  | -- | High frequency automated poster
    Bot
  deriving (Show, Eq)

-- | Represents a simulated user in the network.
data User = User
  { userName :: String,
    -- | Behavioral profile
    userPersonality :: Personality,
    -- | Priority 1: Friend Requests
    userInboxUrgent :: TQueue Message,
    -- | Priority 2: General Chat
    userInboxNormal :: TQueue Message,
    -- | Metric: Total messages sent
    userMessagesSent :: TVar Int,
    -- | Metric: Total messages received
    userCount :: TVar Int,
    -- | Dynamic Friend Graph
    userFriends :: TVar (Set String),
    -- | Known gossip secrets
    userSecrets :: TVar (Set String),
    -- | Trust ratings (0-10) from others
    userRatingsReceived :: TVar [Int]
  }

-- | Type of message sent between users.
data MessageType
  = -- | Standard chat/gossip
    Regular
  | -- | Urgent request to connect
    FriendRequest
  | -- | Confirmation of connection
    FriendAccept
  deriving (Show, Eq)

-- | The core Message payload.
data Message = Message
  { sender :: String,
    content :: String,
    msgType :: MessageType,
    msgTopic :: Topic,
    -- | Piggybacked gossip
    msgSecrets :: [String]
  }
  deriving (Show, Eq)
