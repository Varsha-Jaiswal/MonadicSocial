module Capabilities
  ( MonadLog (..),
  )
where

-- | Capability for logging messages.
-- Allows mocking or different implementations (Stdout vs File).
class (Monad m) => MonadLog m where
  logInfo :: String -> m ()
