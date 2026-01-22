module Run
  ( runSimulation,
  )
where

import App (runAppM)
import Capabilities (MonadAtom (..), MonadLog (..))
import Config (Config (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTQueueIO, newTVarIO, readTVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Env (Env (..))
import Logger (logMsg) -- Still used for setup logging in IO
import Text.Printf (printf)
import Types (User (..))
import UserBehaviour (userThread)

-- | Main entry point for the simulation orchestration
runSimulation :: Config -> IO ()
runSimulation config = do
  logMsg "Run module: Setting up simulation..."

  -- 1. Setup Environment
  globalCnt <- newTVarIO 0
  trendMap <- newTVarIO Map.empty
  let env = Env config globalCnt trendMap

  -- 2. Create Users
  users <- createUsers config
  logMsg $ printf "Created %d users." (length users)

  -- 3. Spawn User Threads
  logMsg "Run module: Spawning user threads..."
  forM_ users $ \user -> do
    forkIO $ runAppM env (userThread env user users)

  logMsg "Run module: Setup complete. Starting Monitor..."

  -- 4. Start Monitor Loop (Blocking)
  -- We run this in AppM to demonstrate capability usage
  runAppM env (monitorLoop env users)

-- | Helper for forM_ since we didn't import Data.Foldable or Control.Monad specifically for it
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

-- | Create users and their channels
createUsers :: Config -> IO [User]
createUsers _ = do
  let userNames = ["User" ++ show i | i <- [1 .. 10]]
  forM userNames $ \name -> do
    inboxU <- newTQueueIO
    inboxN <- newTQueueIO
    msgSent <- newTVarIO 0
    cnt <- newTVarIO 0
    friends <- newTVarIO Set.empty
    secrets <- newTVarIO Set.empty
    ratings <- newTVarIO []

    return $
      User
        { userName = name,
          userInboxUrgent = inboxU,
          userInboxNormal = inboxN,
          userMessagesSent = msgSent,
          userCount = cnt,
          userFriends = friends,
          userSecrets = secrets,
          userRatingsReceived = ratings
        }

-- | Loop to monitor simulation progress
monitorLoop :: (MonadIO m, MonadAtom m, MonadLog m) => Env -> [User] -> m ()
monitorLoop env users = do
  let limit = maxMessages (envConfig env)

  -- Check global count
  count <- liftAtom $ readTVar (envGlobalCount env)

  if count >= limit
    then do
      logInfo $ printf "Simulation Limit Reached (%d messages). Stopping." count
      printStats env users
    else do
      -- Wait a bit (e.g., 1 second)
      liftIO $ threadDelay 1000000 -- 1 second
      printStats env users
      monitorLoop env users

-- | Print periodic statistics
printStats :: (MonadAtom m, MonadLog m) => Env -> [User] -> m ()
printStats env _ = do
  count <- liftAtom $ readTVar (envGlobalCount env)
  logInfo $ printf "--- Stats: Global Messages: %d ---" count
