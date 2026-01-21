module UserBehaviour
  ( userThread,
  )
where

import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', readTQueue, readTVar, tryReadTQueue, writeTQueue)
import Control.Monad (forever, when)
import Data.Set qualified as Set
import Env (Env (..))
import Logger (logMsg)
import System.Random (randomRIO)
import Text.Printf (printf)
import Types (Message (..), MessageType (..), User (..))

-- | The main loop for a simulated user.
userThread :: Env -> User -> [User] -> IO ()
userThread env user allUsers = do
  let name = userName user
  let others = filter (\u -> userName u /= name) allUsers
  logMsg $ printf "Thread started for %s" name

  forever $ do
    -- 1. Randomly decide to send a message
    chance <- randomRIO (1, 100) :: IO Int
    when (chance <= 20) $ do
      -- 20% chance to send per tick
      target <- randomElement others
      sendMessage user target "Hello!"

    -- 2. Process Inbox
    processInbox user

    -- 3. Sleep
    threadDelay 1000000 -- 1 second sleep

-- | Pick a random element from a list
randomElement :: [a] -> IO a
randomElement xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)

-- | Send a message to another user's appropriate queue
sendMessage :: User -> User -> String -> IO ()
sendMessage sender receiver content = do
  let msg =
        Message
          { sender = userName sender,
            content = content,
            msgType = Regular,
            msgTopic = "General",
            msgSecrets = []
          }

  atomically $ do
    -- For now, everything goes to Normal inbox.
    -- Later we will split based on msgType.
    writeTQueue (userInboxNormal receiver) msg
    modifyTVar' (userMessagesSent sender) (+ 1)
    modifyTVar' (userCount receiver) (+ 1)

-- | Process pending messages in the inbox
processInbox :: User -> IO ()
processInbox user = atomically $ do
  -- Check Urgent first (Friend Requests) - Placeholder for now
  -- mUrgent <- tryReadTQueue (userInboxUrgent user)

  -- Check Normal
  mNormal <- tryReadTQueue (userInboxNormal user)
  case mNormal of
    Nothing -> return ()
    Just msg -> do
      -- In a real app we'd log this, but purely in STM we can't print.
      -- So we just extract it. The run loop or logger would handle display
      -- if we returned actions, but here we just consume it.
      return ()
