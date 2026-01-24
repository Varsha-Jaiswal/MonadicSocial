-- This module sets up the simulation environment, spawns user threads,
-- and monitors the global state for termination conditions.
module Run 
    ( runSimulation
    ) where

import Control.Concurrent.STM (newTVarIO, newTQueueIO, atomically, readTVar, modifyTVar)
import Control.Concurrent.Async (async, cancel, Async)
import Control.Monad (forM_, forM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import System.Random (randomRIO)
import qualified Control.Concurrent (threadDelay)
import Text.Printf (printf)

import Types
import Env
import Config
import Logger
import AppM
import UserBehaviour

import Reputation (calculateScore)

-- | Entry point for the simulation.
-- Initializes the environment, creates users, spawns threads, and blocks until completion.
runSimulation :: IO ()
runSimulation = do
    -- 1. Load Configuration
    config <- loadConfig "config.json"
    
    -- 2. Initialize Global State
    globalCount <- newTVarIO 0
    trendMap    <- newTVarIO Map.empty
    
    let env = Env config globalCount trendMap
    
    logInfo ">>> Initializing Hybrid Social Network Simulation..."
    logInfo ">>> Features: Dynamic Friends + Gossip + Viral Trends"
    logInfo ">>> Enhancements: Trust System + Priority Queues"
    
    -- 3. Create Users
    let names = [ "Nishant", "Harsh", "Katie", "Adam", "Besmur"
                , "Shreya", "Varsha", "Paulo", "Edmund", "Mahesha"
                , "Kavita", "Abhishek", "Elli" 
                ]
    users <- createUsers names
    
    -- 4. Seed Initial Secrets (Gossip)
    let secret = initialSecret config
    atomically $ do
        modifyTVar (userSecrets (head users)) (Set.insert secret)
        -- Make Nishant and Harsh friends initially to bootstrap gossip
        modifyTVar (userFriends (head users)) (Set.insert (userName (users !! 1)))
        modifyTVar (userFriends (users !! 1)) (Set.insert (userName (head users)))
    
    logInfo ">>> Spawning Threads..."
    threads <- mapM (\u -> async $ runAppM env (userThread u users)) users
    
    logInfo ">>> Simulation Running..."
    logInfo ">>> You can view it in the chat.log file"
    
    -- 5. Monitor
    monitorLoop env users threads

-- | Helper to create User structures with empty state and random personalities.
createUsers :: [String] -> IO [User]
createUsers names = do
    forM names $ \name -> do
        inboxU  <- newTQueueIO
        inboxN  <- newTQueueIO
        sentCnt <- newTVarIO 0
        rcvdCnt <- newTVarIO 0
        friends <- newTVarIO Set.empty
        secrets <- newTVarIO Set.empty
        ratings <- newTVarIO []
        
        -- Randomly assign personality
        pRoll <- randomRIO (1 :: Int, 100)
        let pers = case pRoll of
                _ | pRoll <= 20 -> Introvert  -- 20% Introverts
                _ | pRoll <= 80 -> Extrovert  -- 60% Extroverts
                _ -> Bot                      -- 20% Bots (Spammers)
                
        return $ User name inboxU inboxN sentCnt rcvdCnt friends secrets ratings pers

-- | Blocking loop that checks for termination conditions.
-- Terminates when 'envGlobalCount' exceeds 'maxMessages'.
monitorLoop :: Env -> [User] -> [Async ()] -> IO ()
monitorLoop env users threads = do
    done <- atomically $ do
        cnt <- readTVar (envGlobalCount env)
        return (cnt >= maxMessages (envConfig env))
    
    if done
        then do
            logInfo ">>> Simulation Limit Reached!"
            mapM_ cancel threads 
            printStats users
        else do
            -- Sleep briefly to avoid busy loop
            Control.Concurrent.threadDelay 500000 -- 0.5s
            monitorLoop env users threads

-- | Print final statistics for all users.
printStats :: [User] -> IO ()
printStats users = do
    putStrLn "\n--- Final Statistics ---"
    putStrLn "Name       | Type      | Sent | Rcvd | Friends | Influence Score | Activity"
    putStrLn "----------------------------------------------------------------------------------------------------"
    
    -- 1. Gather Data
    stats <- forM users $ \u -> do
        sCnt <- atomically $ readTVar (userMessagesSent u)
        rCnt <- atomically $ readTVar (userCount u)
        friends <- atomically $ readTVar (userFriends u)
        ratings <- atomically $ readTVar (userRatingsReceived u)
        let score = calculateScore ratings
        return (u, sCnt, rCnt, Set.size friends, score)
        
    -- 2. Sort by Score (Descending)
    let sortedStats = sortBy (comparing (\(_, _, _, _, score) -> Down score)) stats
    
    -- 3. Print
    forM_ sortedStats $ \(u, sCnt, rCnt, fCnt, score) -> do
        let notes = case userPersonality u of
                Bot -> "Viral: \"BUY CRYPTO NOW!!\""
                _   -> ""
        let output = printf "%-10s | %-9s | %-4d | %-4d | %-7d | %.2f            | %s" 
                        (userName u) 
                        (show (userPersonality u)) 
                        sCnt
                        rCnt 
                        fCnt 
                        score
                        notes
        putStrLn output
    
    putStrLn "\n--- Simulation Summary ---"
    printf "Total Nodes Created     : %d\n" (length users)
    printf "Total Threads Spawned   : %d\n" (length users)
    printf "Total Messages Sent     : %d\n" (sum $ map (\(_, s, _, _, _) -> s) stats)
    printf "Total Messages Received : %d\n" (sum $ map (\(_, _, r, _, _) -> r) stats)
    printf "Total Friends           : %d\n" (sum $ map (\(_, _, _, f, _) -> f) stats)
    printf "Total Influence Score   : %.2f\n" (sum $ map (\(_, _, _, _, score) -> score) stats)
