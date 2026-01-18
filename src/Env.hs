module Env 
    ( Env(..)
    ) where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Config (Config)
import Types (Topic)

-- | Global Environment shared across threads.
data Env = Env
    { envConfig      :: Config
    , envGlobalCount :: TVar Int             -- ^ Global message counter for termination
    , envTrendMap    :: TVar (Map Topic Int) -- ^ Global tracker for viral trends
    }
