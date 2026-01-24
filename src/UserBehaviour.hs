{-# LANGUAGE FlexibleContexts #-} -- Required for MonadAtom instance.

-- This module contains the decision-making logic for users.
-- It uses abstract capabilities ('MonadAtom', 'MonadLog', etc.) to remain independent
-- of the concrete execution environment, facilitating testing and refactoring.
module UserBehaviour
    ( userThread
    ) where

import Control.Monad (forever, when, unless, forM_)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Capabilities

import Env (Env(..))
import Config (Config(..))
import Types (User(..), Message(..), MessageType(..), Topic, Personality(..))

import Reputation (rateInteraction)

-- | List of trending real-world topics.
topics :: [Topic]
topics = [ "AI_Revolution"
         , "Climate_Change"
         , "Bitcoin_Surge"
         , "SpaceX_Launch"
         , "Remote_Work"
         , "Election_Updates"
         , "Haskell"
         , "MSc_Thesis"
         , "Trump_Presidency"
         , "Phd_Application_Process"
         , "QMUL"
         , "Oxford_Phd_Interview"
         ]

-- | Generates a realistic message content based on the topic.
generateMessageContent :: Topic -> Int -> String
generateMessageContent topic seed = 
    let templates = case topic of
            -- Tech & AI
            "AI_Revolution" -> 
                [ "Honestly, ChatGPT is writing better code than me at this point, kinda clear."
                , "I'm worried about the alignment problem, nobody seems to take it seriously."
                , "Just saw a demo of the new model, it's actually insane how fast it's moving."
                ]
            "Bitcoin_Surge" -> 
                [ "Bro, did you see the charts this morning? It's actually pumping."
                , "I wish I bought more when it was low, huge regret right now."
                , "Everyone is talking about crypto again, feels like the bull run is back."
                ]
            "SpaceX_Launch" -> 
                [ "Staying up to watch the Starship launch, fingers crossed it doesn't explode."
                , "The engineering behind those boosters landing is just mind-blowing."
                , "I really hope we actually get to Mars in our lifetime, that would be sick."
                ]
            "Remote_Work" -> 
                [ "I honestly can't imagine going back to the office 5 days a week."
                , "My back is killing me, I need to invest in a proper ergonomic chair."
                , "The freedom of working from anywhere is just unmatched, love it."
                ]
            
            -- Academic & Thesis (QMUL/Oxford)
            "MSc_Thesis" -> 
                [ "I am so behind on my literature review, sending help immediately."
                , "My supervisor just completely tore apart my draft, back to square one."
                , "Just need to grind out these last few chapters and I'm free."
                ]
            "Phd_Application_Process" -> 
                [ "Writing these statements of purpose is draining my soul."
                , "Trying to find funding is harder than the actual research, it's a joke."
                , "Sent out 5 applications today, now the waiting game begins."
                ]
            "QMUL" -> 
                [ "The library at Mile End is absolutely packed today, can't find a seat."
                , "Walking down the canal after lectures is the only thing keeping me sane."
                , "Actually really enjoying the Functional Programming module, Haskell is kinda cool."
                ]
            "Oxford_Phd_Interview" -> 
                [ "I have my interview at Oxford next week and I'm absolutely terrified."
                , "Prepping for this interview is intense, they ask the wildest questions."
                , "Imagine actually getting into Oxford though, that would be life changing."
                ]
            "Haskell" -> 
                [ "Once you understand monads, everything else just feels messy."
                , "I spent 3 hours debugging a type error and it was a missing bracket."
                , "Functional programming breaks your brain at first, but then it makes total sense."
                ]

            -- Politics & Society
            "Climate_Change" -> 
                [ "The weather has been so weird lately, climate change is definitely real."
                , "I'm trying to cut down on meat, just doing my small part you know."
                , "It's scary thinking about what the world will look like in 50 years."
                ]
            "Trump_Presidency" -> 
                [ "Did you see the latest tweet? It's all over my feed right now."
                , "The polls are so close, it's going to be a stressful election night."
                , "Politics is just so polarizing these days, distinct vibe shift."
                ]
            "Election_Updates" -> 
                [ "I'm just glued to the news right now, waiting for the results."
                , "It's wild how much this election matters for the global economy."
                , "Make sure you go out and vote, it actually makes a difference."
                ]
            
            -- Default / Catch-all
            _ -> 
                [ "Hey, do you have a sec to chat?"
                , "It's been a long week, looking forward to the weekend."
                , "Just caught up on some sleep, feeling way better."
                ]
        idx = seed `mod` length templates
    in templates !! idx

-- | Helper: Atomically updates the global trend map found in 'Env'.
updateTrend :: (MonadAtom m, MonadReader Env m) => Topic -> m ()
updateTrend topic = do
    env <- ask
    atomically $ modifyTVar (envTrendMap env) (Map.insertWith (+) topic 1)

-- | The main loop for a User thread.
-- Continuously processes checking the inbox, waiting, and performing actions.
userThread :: (MonadIO m, MonadAtom m, MonadLog m, MonadRandom m, MonadReader Env m) => User -> [User] -> m ()
userThread me allUsers = forever $ do
    -- 1. Process Inbox (Priority)
    processInbox me allUsers
    
    -- 2. Wait
    -- Personality determines the delay duration.
    -- Delays are calibrated for a fast yet realistic simulation.
    let (minD, maxD) = case userPersonality me of
            Introvert -> (500000, 2000000)    -- 0.5s to 2s
            Extrovert -> (100000, 500000)     -- 0.1s to 0.5s
            Bot       -> (10000, 100000)      -- 0.01s to 0.1s (High Frequency)
            
    delay <- getRandomInt (minD, maxD)
    
    -- "Random Delay / AFK" Logic
    -- 5% chance to take a short break (2s) instead of 5s
    afkChance <- getRandomInt (1, 100)
    let finalDelay = if afkChance <= 5 
                     then delay + 2000000 -- Add 2 seconds
                     else delay
                     
    liftIO $ threadDelay finalDelay 
    
    -- 3. Action Strategy
    -- Decide whether to make new friends or talk to existing ones based on Personality.
    friends <- atomically $ readTVar (userFriends me)
    let myFriends = Set.toList friends
    
    dice <- getRandomInt (1, 100)
    
    case userPersonality me of
        Introvert -> do
            -- Introverts rarely initiate contact unless they have no friends.
            -- If connected, they prefer smaller circles (gossip).
            if null myFriends 
                then when (dice > 80) $ sendFriendRequest me allUsers -- Low chance to start
                else when (dice > 60) $ sendGossipMessage me allUsers myFriends
                
        Extrovert -> do
            -- Extroverts love making friends and talking.
            if dice > 40 -- 60% chance to request friend
                then sendFriendRequest me allUsers
                else sendGossipMessage me allUsers myFriends
                
        Bot -> do
            -- Bots spam friend requests and viral trends efficiently.
            -- They don't care about "relationships".
            if dice > 50
               then sendFriendRequest me allUsers
               else do
                   -- Bots force viral trends
                   let topic = "Bitcoin_Surge" -- Hardcoded bot spam
                   -- Filter out self to prevent self-spam
                   let potentialTargets = filter (\u -> userName u /= userName me) allUsers
                   unless (null potentialTargets) $ do
                       target <- getRandomElement potentialTargets
                       sendMessage me target Regular topic "BUY CRYPTO NOW!!" []
                       updateTrend topic

-- | Processes all messages currently in the user's inbox.
-- PRIORITY QUEUE LOGIC: Checks urgent inbox first, then normal inbox.
processInbox :: (MonadAtom m, MonadLog m, MonadRandom m, MonadReader Env m) => User -> [User] -> m ()
processInbox me allUsers = do
    -- Try Urgent first
    maybeUrgent <- atomically $ tryReadTQueue (userInboxUrgent me)
    case maybeUrgent of
        Just msg -> handleMessage me msg allUsers >> processInbox me allUsers -- Process all urgent first
        Nothing -> do
            -- If no urgent, try Normal
            maybeNormal <- atomically $ tryReadTQueue (userInboxNormal me)
            case maybeNormal of
                Just msg -> handleMessage me msg allUsers
                Nothing -> return ()

-- | Handle a single message and apply logic.
handleMessage :: (MonadAtom m, MonadLog m, MonadRandom m, MonadReader Env m) => User -> Message -> [User] -> m ()
handleMessage me msg allUsers = do
    atomically $ modifyTVar (userCount me) (+1)
    case msgType msg of
        FriendRequest -> do
            let senderName = sender msg
            -- Note: Logging suppressed for terminal clarity.
            atomically $ modifyTVar (userFriends me) (Set.insert senderName)
            
            case lookupUser senderName allUsers of
                Just target -> sendMessage me target FriendAccept "Connectivity" "Friend Request Accepted!" []
                Nothing -> return ()

        FriendAccept -> do
            -- Note: Logging suppressed for terminal clarity.
            atomically $ modifyTVar (userFriends me) (Set.insert (sender msg))
            logChat (userName me) (sender msg) "[System] Accepted Friend Request"

        Regular -> do
            learnSecrets me (msgSecrets msg)
            checkViralInfluence me msg allUsers
            
            -- REALTIME CHAT LOGGING
            logChat (sender msg) (userName me) (content msg)
            
            -- TRUST SYSTEM: Rate the interaction
            -- "I received a message from X. I rate X."
            case lookupUser (sender msg) allUsers of
                Just senderObj -> do
                    -- Pure calculation of rating using random seed
                    randSeed <- getRandomInt (1, 100)
                    let rating = rateInteraction randSeed 
                    atomically $ modifyTVar (userRatingsReceived senderObj) (rating :)
                Nothing -> return ()

-- | Selects a random non-friend user and sends a 'FriendRequest'.
sendFriendRequest :: (MonadAtom m, MonadLog m, MonadRandom m, MonadReader Env m) => User -> [User] -> m ()
sendFriendRequest me allUsers = do
    let others = filter (\u -> userName u /= userName me) allUsers
    target <- getRandomElement others
    
    friends <- atomically $ readTVar (userFriends me)
    if Set.member (userName target) friends
        then return ()
        else do
            sendMessage me target FriendRequest "Connectivity" "Let's be friends!" []
            -- Note: Logging suppressed for terminal clarity.
            logChat (userName me) (userName target) "[System] Sent Friend Request"

-- | Sends a regular message to a friend, potentially piggybacking secrets.
sendGossipMessage :: (MonadAtom m, MonadRandom m, MonadReader Env m) => User -> [User] -> [String] -> m ()
sendGossipMessage me allUsers friendNames = do
    targetName <- getRandomElement friendNames
    case lookupUser targetName allUsers of
        Nothing -> return ()
        Just target -> do
            topic <- getRandomElement topics
            randSeed <- getRandomInt (1, 1000)
            let msgBody = generateMessageContent topic randSeed
            
            mySecrets <- atomically $ readTVar (userSecrets me)
            let secretList = Set.toList mySecrets
            
            sendMessage me target Regular topic msgBody secretList
            updateTrend topic

-- | Integrates new secrets received from a message into the user's knowledge.
learnSecrets :: (MonadAtom m, MonadLog m) => User -> [String] -> m ()
learnSecrets _ [] = return ()
learnSecrets me incomingSecrets = do
    atomically $ do
        current <- readTVar (userSecrets me)
        let newSet = Set.union current (Set.fromList incomingSecrets)
        writeTVar (userSecrets me) newSet
        
    oldSecrets <- atomically $ readTVar (userSecrets me)
    let newSecrets = filter (\s -> not (Set.member s oldSecrets)) incomingSecrets
    forM_ newSecrets $ \s -> 
        logInfo $ ">>> GOSSIP: " ++ userName me ++ " learned secret: " ++ s

-- | Checks if the message's topic is viral and influences the user to add the sender as a friend.
checkViralInfluence :: (MonadAtom m, MonadLog m, MonadReader Env m) => User -> Message -> [User] -> m ()
checkViralInfluence me msg allUsers = do
    env <- ask
    trends <- atomically $ readTVar (envTrendMap env)
    let topic = msgTopic msg
    let count = Map.findWithDefault 0 topic trends
    
    when (count > viralThreshold (envConfig env)) $ do
        friends <- atomically $ readTVar (userFriends me)
        unless (Set.member (sender msg) friends) $ do
             -- Note: Logging suppressed for terminal clarity.
            case lookupUser (sender msg) allUsers of
                Just target -> do
                    sendMessage me target FriendRequest topic ("I love " ++ topic ++ " too! Let's connect.") []
                    logChat (userName me) (userName target) ("[System] Sent Friend Request (Viral: " ++ topic ++ ")")
                Nothing -> return ()

-- | Low-level helper to construct and place a message in the target's inbox.
-- Routes to URGENT or NORMAL inbox based on message type.
sendMessage :: (MonadAtom m, MonadReader Env m) => User -> User -> MessageType -> Topic -> String -> [String] -> m ()
sendMessage from to mType topic body secrets = do
    env <- ask
    atomically $ do
        cnt <- readTVar (envGlobalCount env)
        let limit = maxMessages (envConfig env)
        
        if cnt < limit
            then do
                let msg = Message
                        { sender = userName from
                        , content = body
                        , msgType = mType
                        , msgTopic = topic
                        , msgSecrets = secrets
                        }
                -- PRIORITY ROUTING
                case mType of
                   FriendRequest -> writeTQueue (userInboxUrgent to) msg
                   FriendAccept  -> writeTQueue (userInboxUrgent to) msg
                   Regular       -> writeTQueue (userInboxNormal to) msg
                   
                writeTVar (envGlobalCount env) (cnt + 1)
                modifyTVar (userMessagesSent from) (+1) -- Increment Sent Counter
            else return ()

-- | Utility to find a User object by name.
lookupUser :: String -> [User] -> Maybe User
lookupUser _ [] = Nothing
lookupUser name (u:us)
    | userName u == name = Just u
    | otherwise          = lookupUser name us
