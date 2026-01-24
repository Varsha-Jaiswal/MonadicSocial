module Lib
    ( simulate
    ) where

import Run (runSimulation)

simulate :: IO ()
simulate = runSimulation
