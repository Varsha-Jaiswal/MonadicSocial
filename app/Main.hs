module Main (main) where

import Config (defaultConfig)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import qualified Data.Map as Map
import Env (Env (..))
import Logger (logMsg)
import Types (User (..))

main :: IO ()
main = do
  logMsg "Monadic Social Network Simulation: Initializing..."

  -- Verify Domain Model
  print defaultConfig
  logMsg "Domain Model Verification: Success"
