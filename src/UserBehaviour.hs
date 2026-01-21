module UserBehaviour
  ( userThread,
  )
where

import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Env (Env (..))
import Logger (logMsg)
import Text.Printf (printf)
import Types (User (..))

-- | The main loop for a simulated user.
-- Currently just a placeholder for Day 5.
userThread :: Env -> User -> IO ()
userThread env user = do
  let name = userName user
  logMsg $ printf "Thread started for %s" name

  -- Simulation loop (simplified for now)
  forever $ do
    threadDelay 1000000 -- Sleep 1 sec
    -- logMsg $ printf "%s is active..." name
