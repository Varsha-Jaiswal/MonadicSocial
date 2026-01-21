module Main (main) where

import Config (defaultConfig)
import Logger (logMsg)
import Run (runSimulation)

main :: IO ()
main = do
  logMsg "Monadic Social Network Simulation: Initializing..."
  runSimulation defaultConfig
  logMsg "Simulation Finished."
