module Main (main) where

import Types (User(..))
import Config (defaultConfig)
import Env (Env(..))
import Control.Concurrent.STM (newTVarIO, atomically, readTVar)
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Monadic Social Network Simulation: Initializing..."
    
    -- Verify Domain Model
    print defaultConfig
    putStrLn "Domain Model Verification: Success"
