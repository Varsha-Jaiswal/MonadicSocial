
-- This module defines the 'Env' data type, which holds the shared state
-- and configuration accessible to all parts of the application via 'AppM'.
module Env 
    ( Env(..)
    ) where

import Control.Concurrent.STM (TVar)
import qualified Data.Map as Map

import Config (Config)
import Types (Topic)

-- | The read-only environment containing global mutable state references.
data Env = Env
    { envConfig      :: Config                 -- ^ Static configuration loaded at startup.
    , envGlobalCount :: TVar Int               -- ^ Global counter for total messages exchanged.
    , envTrendMap    :: TVar (Map.Map Topic Int) -- ^ Tracks popularity of topics for viral trend analysis.
    }
