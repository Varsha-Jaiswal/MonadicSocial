module Run 
    ( runSimulation
    ) where

import Control.Monad (forM, unless, forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (newTQueueIO, newTVarIO, atomically, readTVar)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf (printf)

import Config (Config(..))
import Env (Env(..))
import Logger (logMsg)
import Types (User(..))

-- | Main entry point for the simulation orchestration
-- Updated to include the monitor loop.
runSimulation :: Config -> IO ()
runSimulation config = do
    logMsg "Run module: Setting up simulation..."
    
    -- 1. Setup Environment
    globalCnt <- newTVarIO 0
    trendMap  <- newTVarIO Map.empty
    let env = Env config globalCnt trendMap

    -- 2. Create Users
    users <- createUsers config
    logMsg $ printf "Created %d users." (length users)
    
    logMsg "Run module: Setup complete. Starting Monitor..."
    
    -- 3. Start Monitor Loop (Blocking)
    monitorLoop env users

-- | Create users and their channels
createUsers :: Config -> IO [User]
createUsers _ = do
    let userNames = ["User" ++ show i | i <- [1..10]]
    forM userNames $ \name -> do
        inboxU  <- newTQueueIO
        inboxN  <- newTQueueIO
        msgSent <- newTVarIO 0
        cnt     <- newTVarIO 0
        friends <- newTVarIO Set.empty
        secrets <- newTVarIO Set.empty
        ratings <- newTVarIO []
        
        return $ User
            { userName            = name
            , userInboxUrgent     = inboxU
            , userInboxNormal     = inboxN
            , userMessagesSent    = msgSent
            , userCount           = cnt
            , userFriends         = friends
            , userSecrets         = secrets
            , userRatingsReceived = ratings
            }

-- | Loop to monitor simulation progress
monitorLoop :: Env -> [User] -> IO ()
monitorLoop env users = do
    let limit = maxMessages (envConfig env)
    
    -- Check global count
    count <- atomically $ readTVar (envGlobalCount env)
    
    if count >= limit
        then do
            logMsg $ printf "Simulation Limit Reached (%d messages). Stopping." count
            printStats env users
        else do
            -- Wait a bit (e.g., 1 second)
            threadDelay 1000000 -- 1 second
            printStats env users
            monitorLoop env users

-- | Print periodic statistics
printStats :: Env -> [User] -> IO ()
printStats env users = do
    count <- atomically $ readTVar (envGlobalCount env)
    logMsg $ printf "--- Stats: Global Messages: %d ---" count
