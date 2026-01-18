module Config 
    ( Config(..)
    , defaultConfig
    ) where

-- | Simuation Configuration Parameters.
data Config = Config
    { minMessages    :: Int
    , maxMessages    :: Int
    , minDelay       :: Int
    , maxDelay       :: Int
    , initialSecret  :: String
    , viralThreshold :: Int
    } deriving (Show)

-- | Default values for testing before JSON loading is ready.
defaultConfig :: Config
defaultConfig = Config
    { minMessages    = 10
    , maxMessages    = 50
    , minDelay       = 100000
    , maxDelay       = 500000
    , initialSecret  = "Haskell is fun"
    , viralThreshold = 3
    }
