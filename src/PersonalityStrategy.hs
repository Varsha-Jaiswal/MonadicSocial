{-# LANGUAGE ImportQualifiedPost #-}

module PersonalityStrategy
  ( getDelayRange,
    decideAction,
    shouldGoAFK,
    ActionDecision (..),
  )
where

import Capabilities (MonadAtom (..), MonadRandom (..), STM, readTVar)
import Data.Set qualified as Set
import Types (Personality (..), User (..))

-- | Determines the wait time range (in microseconds) based on personality.
getDelayRange :: Personality -> (Int, Int)
getDelayRange Introvert = (500000, 2000000) -- 0.5s to 2s
getDelayRange Extrovert = (100000, 500000) -- 0.1s to 0.5s
getDelayRange Bot = (10000, 100000) -- 0.01s to 0.1s (High Frequency)

-- | Decides whether the user should take a short break.
shouldGoAFK :: (MonadRandom m) => m Bool
shouldGoAFK = do
  chance <- getRandomInt (1, 100)
  return (chance <= 5)

-- | Represents the decision made by the user implementation.
data ActionDecision
  = SendFriendRequest
  | SendGossip
  | BotSpam
  | DoNothing
  deriving (Show, Eq)

-- | Decides the next action for a user based on their personality and current state.
decideAction :: (MonadRandom m, MonadAtom m) => User -> m ActionDecision
decideAction me = do
  friends <- atomically $ readTVar (userFriends me)
  let friendCount = Set.size friends

  dice <- getRandomInt (1, 100)

  case userPersonality me of
    Introvert -> do
      -- Introverts rarely initiate contact unless they have no friends.
      -- If connected, they prefer smaller circles (gossip).
      if friendCount == 0
        then return $ if dice > 80 then SendFriendRequest else DoNothing
        else return $ if dice > 60 then SendGossip else DoNothing
    Extrovert -> do
      -- Extroverts love making friends and talking.
      return $
        if dice > 40 -- 60% chance to request friend
          then SendFriendRequest
          else SendGossip
    Bot -> do
      -- Bots spam friend requests and viral trends efficiently.
      -- They don't care about "relationships".
      return $
        if dice > 50
          then SendFriendRequest
          else BotSpam
