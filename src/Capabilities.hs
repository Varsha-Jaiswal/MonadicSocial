module Capabilities
  ( MonadLog (..),
    MonadAtom (..),
  )
where

import Control.Concurrent.STM (STM)

-- | Capability for logging messages.
-- Allows mocking or different implementations (Stdout vs File).
class (Monad m) => MonadLog m where
  logInfo :: String -> m ()
  logChat :: String -> String -> String -> m ()

-- | Capability for atomic STM operations.
-- Allows abstraction over raw 'atomically'.
class (Monad m) => MonadAtom m where
  liftAtom :: STM a -> m a
