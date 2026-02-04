-- Required for MonadAtom instance.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- This module contains the decision-making logic for users.
-- It uses abstract capabilities ('MonadAtom', 'MonadLog', etc.) to remain independent
-- of the concrete execution environment, facilitating testing and refactoring.
module UserBehaviour
  ( userThread,
  )
where

import Capabilities
import Config (Config (..))
-- \| List of trending real-world topics.
import ContentStrategy qualified as CS
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Env (Env (..))
import PersonalityStrategy (ActionDecision (..))
import PersonalityStrategy qualified as PS
import Reputation (rateInteraction)
import Types (Message (..), MessageType (..), Personality (..), Topic, User (..))

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
  -- New Strategy: Delegate delay calculation to PersonalityStrategy
  let (minD, maxD) = PS.getDelayRange (userPersonality me)

  delay <- getRandomInt (minD, maxD)

  -- "Random Delay / AFK" Logic
  isAFK <- PS.shouldGoAFK
  let finalDelay =
        if isAFK
          then delay + 2000000 -- Add 2 seconds
          else delay

  liftIO $ threadDelay finalDelay

  -- 3. Action Strategy
  -- New Strategy: Delegate decision naming to PersonalityStrategy
  decision <- PS.decideAction me

  friends <- atomically $ readTVar (userFriends me)
  let myFriends = Set.toList friends

  case decision of
    SendFriendRequest -> sendFriendRequest me allUsers
    SendGossip -> sendGossipMessage me allUsers myFriends
    BotSpam -> do
      -- Bots force viral trends
      let topic = "Bitcoin_Surge" -- Hardcoded bot spam
      -- Filter out self to prevent self-spam
      let potentialTargets = filter (\u -> userName u /= userName me) allUsers
      unless (null potentialTargets) $ do
        target <- getRandomElement potentialTargets
        sendMessage me target Regular topic "BUY CRYPTO NOW!!" []
        updateTrend topic
    DoNothing -> return ()

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
  atomically $ modifyTVar (userCount me) (+ 1)
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
      topic <- getRandomElement CS.topics
      randSeed <- getRandomInt (1, 1000)
      let msgBody = CS.generateMessageContent topic randSeed

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
        let msg =
              Message
                { sender = userName from,
                  content = body,
                  msgType = mType,
                  msgTopic = topic,
                  msgSecrets = secrets
                }
        -- PRIORITY ROUTING
        case mType of
          FriendRequest -> writeTQueue (userInboxUrgent to) msg
          FriendAccept -> writeTQueue (userInboxUrgent to) msg
          Regular -> writeTQueue (userInboxNormal to) msg

        writeTVar (envGlobalCount env) (cnt + 1)
        modifyTVar (userMessagesSent from) (+ 1) -- Increment Sent Counter
      else return ()

-- | Utility to find a User object by name.
lookupUser :: String -> [User] -> Maybe User
lookupUser _ [] = Nothing
lookupUser name (u : us)
  | userName u == name = Just u
  | otherwise = lookupUser name us
