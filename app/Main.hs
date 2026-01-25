module Main (main) where

import Logger (logInfo)
import Run (runSimulation)

main :: IO ()
main = do
  logInfo "Monadic Social Network Simulation: Initializing..."
  runSimulation
  logInfo "Simulation Finished."
