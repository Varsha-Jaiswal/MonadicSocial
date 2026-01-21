module Run
    ( runSimulation
    ) where

import Control.Monad (forM)
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
runSimulation :: Config -> IO ()
runSimulation config = do
    logMsg "Run module: Setting up simulation..."
    
    -- 1. Create Users
    users <- createUsers config
    logMsg $ printf "Created %d users." (length users)
    
    logMsg "Run module: Setup complete."

-- | Create users and their channels
-- Hardcoded to 10 users for Day 4.
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
monitorLoop :: Env -> IO ()
monitorLoop _ = return ()

-- | Print periodic statistics
printStats :: Env -> [User] -> IO ()
printStats _ _ = return ()
