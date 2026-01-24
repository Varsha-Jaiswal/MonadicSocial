{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-} -- Required for deriving MonadReader and implementing capability instances.

-- This module defines the 'AppM' monad, which is the concrete execution context
-- for the social network simulation. It implements the abstract capabilities
-- defined in "Capabilities" by leveraging "Env" and standard IO/STM operations.
module AppM
    ( -- * The Application Monad
      AppM(..)
    , runAppM
      -- * Helper Functions
    , getEnv
    , getConfig
    ) where

import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, MonadReader, MonadIO, liftIO)
import System.Random (randomRIO)
import qualified Control.Concurrent.STM as STM

import Env (Env(..))
import Config (Config)
import Capabilities (MonadAtom(..), MonadLog(..), MonadRandom(..))
import qualified Logger as L

-- | 'AppM' is the core monad stack for the application.
-- It wraps a 'ReaderT' carrying the global 'Env' over the 'IO' monad.
newtype AppM a = AppM { unAppM :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- | Executes an 'AppM' computation given an initial 'Env'.
runAppM :: Env -> AppM a -> IO a
runAppM env app = runReaderT (unAppM app) env

-- | Retrieves the global environment.
getEnv :: AppM Env
getEnv = ask

-- | Retrieves the configuration from the environment.
getConfig :: AppM Config
getConfig = asks envConfig

-- | Instance for Atomic Operations (STM)
instance MonadAtom AppM where
    atomically stmAct = liftIO $ STM.atomically stmAct

-- | Instance for Logging via the 'Logger' module
instance MonadLog AppM where
    logInfo  = L.logInfo
    logDebug = L.logDebug
    logChat  = L.logChat
    logError = L.logError

-- | Instance for Randomness using 'System.Random'
instance MonadRandom AppM where
    getRandomInt range = liftIO $ randomRIO range
    getRandomElement xs = liftIO $ do
        idx <- randomRIO (0, length xs - 1)
        return (xs !! idx)
