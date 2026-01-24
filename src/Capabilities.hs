{-# LANGUAGE FlexibleContexts #-} -- Required for MonadAtom instance.

-- This module defines the abstract interface for the application's side effects.
-- By programming against these typeclasses instead of concrete IO, we achieve
-- loose coupling and enable easier testing and refactoring.
module Capabilities 
    ( -- * Core Capabilities
      MonadAtom(..)
    , MonadLog(..)
    , MonadRandom(..)
      -- * STM Re-exports
      -- | These are re-exported to allow consumers to use STM primitives
      -- without importing 'Control.Concurrent.STM' directly.
    , STM
    , TVar
    , TQueue
    , readTVar
    , writeTVar
    , modifyTVar
    , readTQueue
    , writeTQueue
    , tryReadTQueue
    , newTVar
    , newTQueue
    ) where

import Control.Concurrent.STM (STM, TVar, TQueue)
import qualified Control.Concurrent.STM as STM

-- | 'MonadAtom' abstracts over atomic transactions (STM).
-- This allows us to perform thread-safe operations on mutable variables.
class Monad m => MonadAtom m where
    -- | Atomically executes an STM action within the monad @m@.
    atomically :: STM a -> m a

-- Re-export STM primitives for convenience

-- | Read a 'TVar'.
readTVar :: TVar a -> STM a
readTVar = STM.readTVar

-- | Write to a 'TVar'.
writeTVar :: TVar a -> a -> STM ()
writeTVar = STM.writeTVar

-- | Modify a 'TVar' using a pure function.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar = STM.modifyTVar

-- | Read from a 'TQueue'. Retries if empty.
readTQueue :: TQueue a -> STM a
readTQueue = STM.readTQueue

-- | Write to a 'TQueue'.
writeTQueue :: TQueue a -> a -> STM ()
writeTQueue = STM.writeTQueue

-- | Non-blocking read from a 'TQueue'. Returns 'Nothing' if empty.
tryReadTQueue :: TQueue a -> STM (Maybe a)
tryReadTQueue = STM.tryReadTQueue

-- | Create a new 'TVar'.
newTVar :: a -> STM (TVar a)
newTVar = STM.newTVar

-- | Create a new 'TQueue'.
newTQueue :: STM (TQueue a)
newTQueue = STM.newTQueue

-- | 'MonadLog' abstracts over logging functionality.
class Monad m => MonadLog m where
    -- | Log an informational message.
    logInfo  :: String -> m ()
    -- | Log a debug message.
    logDebug :: String -> m ()
    -- | Log a chat message with sender, recipient, and content.
    logChat  :: String -> String -> String -> m ()
    -- | Log an error message.
    logError :: String -> m ()

-- | 'MonadRandom' abstracts over random value generation.
class Monad m => MonadRandom m where
    -- | Generate a random integer within a range (inclusive).
    getRandomInt :: (Int, Int) -> m Int
    -- | Pick a random element from a list.
    getRandomElement :: [a] -> m a
