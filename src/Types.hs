-- This module defines the fundamental data structures used throughout the application,
-- including 'User', 'Message', and helper types.
module Types 
    ( User(..)
    , Message(..)
    , MessageType(..)
    , Topic
    , Personality(..)
    ) where

import Control.Concurrent.STM (TVar, TQueue)
import Data.Set (Set)

-- | Defines the behavioral archetype of a user.
data Personality 
    = Introvert   -- ^ Prefers not to initiate, longer delays.
    | Extrovert   -- ^ Highly active, initiates often.
    | Bot         -- ^ Automated behavior, spams messages.
    deriving (Show, Eq)

-- | Represents a User in the social network.
data User = User
    { userName            :: String           -- ^ The unique name of the user.
    , userInboxUrgent     :: TQueue Message   -- ^ High-priority messages (Friend Requests).
    , userInboxNormal     :: TQueue Message   -- ^ Standard messages (Chat).
    , userMessagesSent    :: TVar Int         -- ^ Counter for total messages sent.
    , userCount           :: TVar Int         -- ^ Counter for total messages received/processed.
    , userFriends         :: TVar (Set String)-- ^ Dynamic Graph: Set of friend usernames.
    , userSecrets         :: TVar (Set String)-- ^ Gossip State: Set of known secrets.
    , userRatingsReceived :: TVar [Int]       -- ^ Trust System: History of ratings received from others.
    , userPersonality     :: Personality      -- ^ The behavioral profile of this user.
    }

-- | Type alias for Message Topics (e.g., "Sports", "Haskell").
type Topic = String

-- | Different categories of messages exchanged between users.
data MessageType 
    = Regular       -- ^ Standard chat message.
    | FriendRequest -- ^ Request to form a connection.
    | FriendAccept  -- ^ Confirmation of connection.
    deriving (Show, Eq)

-- | Represents a message sent from one user to another.
data Message = Message
    { sender     :: String        -- ^ The username of the sender.
    , content    :: String        -- ^ The textual content.
    , msgType    :: MessageType   -- ^ Type of interaction.
    , msgTopic   :: Topic         -- ^ Topic of the message (used for viral analysis).
    , msgSecrets :: [String]      -- ^ Piggybacked gossip secrets (if any).
    } deriving (Show, Eq)
