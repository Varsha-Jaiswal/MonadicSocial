module UserBehaviour
  ( userThread,
  )
where

-- import qualified Data.Set as Set

-- import Config (Config(..))

import Capabilities (MonadAtom (..), MonadLog (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (modifyTVar', readTQueue, readTVar, tryReadTQueue, writeTQueue)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Env (Env (..))
import System.Random (randomRIO)
import Text.Printf (printf)
import Types (Message (..), MessageType (..), User (..))

-- | The main loop for a simulated user.
-- Refactored to use Capabilities (MonadAtom, MonadLog) instead of raw IO/STM.
userThread :: (MonadIO m, MonadAtom m, MonadLog m) => Env -> User -> [User] -> m ()
userThread _ user allUsers = do
  let name = userName user
  let others = filter (\u -> userName u /= name) allUsers
  logInfo $ printf "Thread started for %s" name

  forever $ do
    -- 1. Randomly decide to send a message
    chance <- liftIO $ randomRIO (1, 100) :: (MonadIO m) => m Int
    when (chance <= 20) $ do
      -- 20% chance to send per tick
      target <- liftIO $ randomElement others
      sendMessage user target "Hello!"

    -- 2. Process Inbox
    processInbox user

    -- 3. Sleep
    liftIO $ threadDelay 1000000 -- 1 second sleep

-- | Pick a random element from a list
randomElement :: [a] -> IO a
randomElement xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)

-- | Send a message to another user's appropriate queue
sendMessage :: (MonadAtom m) => User -> User -> String -> m ()
sendMessage sender receiver content = do
  let msg =
        Message
          { sender = userName sender,
            content = content,
            msgType = Regular,
            msgTopic = "General",
            msgSecrets = []
          }

  liftAtom $ do
    -- For now, everything goes to Normal inbox.
    writeTQueue (userInboxNormal receiver) msg
    modifyTVar' (userMessagesSent sender) (+ 1)
    modifyTVar' (userCount receiver) (+ 1)

-- | Process pending messages in the inbox
processInbox :: (MonadAtom m) => User -> m ()
processInbox user = liftAtom $ do
  -- Check Urgent first (Friend Requests) - Placeholder for now
  -- mUrgent <- tryReadTQueue (userInboxUrgent user)

  -- Check Normal
  mNormal <- tryReadTQueue (userInboxNormal user)
  case mNormal of
    Nothing -> return ()
    Just _ -> do
      -- In a real app we'd log this, but purely in STM we can't print.
      return ()
