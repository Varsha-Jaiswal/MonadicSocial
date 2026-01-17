module Types 
    ( Message(..)
    , MessageType(..)
    , Topic
    ) where

type Topic = String

-- | Type of message sent between users.
data MessageType 
    = Regular           -- ^ Standard chat/gossip
    | FriendRequest     -- ^ Urgent request to connect
    | FriendAccept      -- ^ Confirmation of connection
    deriving (Show, Eq)

-- | The core Message payload.
data Message = Message
    { sender     :: String
    , content    :: String
    , msgType    :: MessageType
    , msgTopic   :: Topic
    , msgSecrets :: [String] -- ^ Piggybacked gossip
    } deriving (Show, Eq)
