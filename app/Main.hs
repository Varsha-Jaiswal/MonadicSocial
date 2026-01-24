module Main (main) where

import Config (loadConfig)
import Logger (logMsg)
import Run (runSimulation)

main :: IO ()
main = do
  logMsg "Monadic Social Network Simulation: Initializing..."
  config <- loadConfig "config.json"
  runSimulation config
  logMsg "Simulation Finished."
