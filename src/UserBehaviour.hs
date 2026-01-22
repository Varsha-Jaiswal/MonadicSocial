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
import Types (Message (..), MessageType (..), Personality (..), User (..))

-- | The main loop for a simulated user.
-- Refactored to use Capabilities (MonadAtom, MonadLog) instead of raw IO/STM.
userThread :: (MonadIO m, MonadAtom m, MonadLog m) => Env -> User -> [User] -> m ()
userThread env user allUsers = do
  let name = userName user
  let personality = userPersonality user
  let others = filter (\u -> userName u /= name) allUsers
  logInfo $ printf "Thread started for %s (%s)" name (show personality)

  forever $ do
    -- 1. Randomly decide to send a message based on personality
    chance <- liftIO $ randomRIO (1, 100) :: (MonadIO m) => m Int
    let threshold = case personality of
          Introvert -> 10 -- 10%
          Extrovert -> 40 -- 40%
          Bot -> 90 -- 90%
    when (chance <= threshold) $ do
      target <- liftIO $ randomElement others
      content <- liftIO $ generateContent personality
      sendMessage env user target content

    -- 2. Process Inbox
    processInbox user

    -- 3. Sleep based on personality
    liftIO $ simulatedDelay personality

-- | Sleep duration based on personality
simulatedDelay :: Personality -> IO ()
simulatedDelay p = do
  let baseDelay = case p of
        Introvert -> 3000000 -- 3 seconds
        Extrovert -> 1000000 -- 1 second
        Bot -> 100000 -- 0.1 second

  -- Add 20% jitter
  jitter <- randomRIO (-(baseDelay `div` 5), baseDelay `div` 5)
  threadDelay (baseDelay + jitter)

-- | Generate random content based on personality
generateContent :: Personality -> IO String
generateContent p = do
  let topics = case p of
        Introvert -> ["Haskell", "Philosophy", "Reading", "Chess"]
        Extrovert -> ["Party", "Travel", "Music", "Sports"]
        Bot -> ["SPAM", "BUY NOW", "CLICK HERE", "FREE"]

  t <- randomElement topics
  return $ "Thinking about " ++ t

-- | Pick a random element from a list
randomElement :: [a] -> IO a
randomElement xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)

-- | Send a message to another user's appropriate queue
sendMessage :: (MonadAtom m) => Env -> User -> User -> String -> m ()
sendMessage env sender receiver content = do
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

    -- Update Global Count
    modifyTVar' (envGlobalCount env) (+ 1)

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
