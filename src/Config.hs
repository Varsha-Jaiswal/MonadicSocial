{-# LANGUAGE DeriveGeneric #-} -- Required for Generic instance.

-- This module defines the 'Config' data type and handles loading it from
-- a JSON configuration file. It uses 'Aeson' for robust parsing.
module Config 
    ( -- * Configuration Type
      Config(..)
    , defaultConfig
      -- * Loading Configuration
    , loadConfig
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import Control.Exception (try, IOException)

-- | Holds all tunable parameters for the simulation.
data Config = Config
    { maxMessages    :: Int      -- ^ The simulation terminates after this many global messages.
    , minDelay       :: Int      -- ^ Minimum thread delay in microseconds.
    , maxDelay       :: Int      -- ^ Maximum thread delay in microseconds.
    , initialSecret  :: String   -- ^ The initial secret string seeded to users.
    , viralThreshold :: Int      -- ^ Number of occurrences required for a topic to trigger viral behavior.
    } deriving (Show, Eq, Generic)

-- | Automatic JSON decoding instance.
instance FromJSON Config

-- | Default configuration values used as a fallback.
defaultConfig :: Config
defaultConfig = Config
    { maxMessages    = 100
    , minDelay       = 100000 -- 0.1s
    , maxDelay       = 500000 -- 0.5s
    , initialSecret  = "FP is Awesome"
    , viralThreshold = 5
    }

-- | Load (or create) the configuration from a JSON file.
-- Tries to read the file. If missing or invalid, falls back to 'defaultConfig'.
-- Catches 'IOException' to ensure the simulation always starts.
loadConfig :: FilePath -> IO Config
loadConfig path = do
    result <- try (B.readFile path) :: IO (Either IOException B.ByteString)
    case result of
        Left ex -> do
            putStrLn $ ">>> Config file not found or unreadable (" ++ show ex ++ "). Using defaults."
            return defaultConfig
        Right content -> do
            case decode content of
                Just cfg -> do
                    putStrLn ">>> Loaded config.json successfully."
                    return cfg
                Nothing -> do
                    putStrLn ">>> Failed to parse config.json (Invalid JSON). Using defaults."
                    return defaultConfig
