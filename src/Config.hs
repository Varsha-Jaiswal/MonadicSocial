{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config (..),
    defaultConfig,
    loadConfig,
  )
where

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.ByteString.Lazy qualified as B
import GHC.Generics (Generic)

-- | Simuation Configuration Parameters.
data Config = Config
  { minMessages :: Int,
    maxMessages :: Int,
    minDelay :: Int,
    maxDelay :: Int,
    initialSecret :: String,
    viralThreshold :: Int
  }
  deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

-- | Default values for testing before JSON loading is ready.
defaultConfig :: Config
defaultConfig =
  Config
    { minMessages = 10,
      maxMessages = 50,
      minDelay = 100000,
      maxDelay = 500000,
      initialSecret = "Haskell is fun",
      viralThreshold = 3
    }

-- | Load configuration from a JSON file. Returns defaultConfig on failure.
loadConfig :: FilePath -> IO Config
loadConfig path = do
  content <- B.readFile path
  case decode content of
    Just config -> return config
    Nothing -> do
      putStrLn "Failed to parse config.json, using defaults."
      return defaultConfig
