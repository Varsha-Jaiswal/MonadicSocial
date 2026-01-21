{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( AppM,
    runAppM,
  )
where

import Capabilities (MonadLog (..))
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, liftIO, runReaderT)
import Env (Env)
import Logger qualified

-- | The application monad stack.
-- Uses ReaderT to pass the Environment implicitly.
newtype AppM a = AppM {unAppM :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- | Execute the application monad.
runAppM :: Env -> AppM a -> IO a
runAppM env app = runReaderT (unAppM app) env

instance MonadLog AppM where
  logInfo = liftIO . Logger.logMsg
