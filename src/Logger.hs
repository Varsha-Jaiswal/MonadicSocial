
-- This module provides functions for printing timestamped log messages.
-- It is used by the 'MonadLog' instance in "AppM".
module Logger 
    ( logInfo
    , logDebug
    , logError
    , logChat  -- ^ New function for tracking conversations
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr, appendFile)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, IOException) -- Added for safe logging

-- | Writes a log message to standard error with a timestamp/level.
logMsg :: MonadIO m => String -> String -> m ()
logMsg level msg = liftIO $ do
    time <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "[%H:%M:%S]" time
    hPutStrLn stderr $ timestamp ++ " [" ++ level ++ "] " ++ msg

-- | Log information (green/standard).
logInfo :: MonadIO m => String -> m ()
logInfo = logMsg "Info"

-- | Log debug traces (often hidden in production).
logDebug :: MonadIO m => String -> m ()
logDebug = logMsg "Debug"

-- | Log error messages.
logError :: MonadIO m => String -> m ()
logError = logMsg "Error"

-- | Appends a chat message to a persistent log file.
-- Format: [Time] Sender -> Recipient: Message
-- Catches IOExceptions (e.g., file locked) to prevent crashing the simulation.
logChat :: MonadIO m => String -> String -> String -> m ()
logChat sender recipient content = liftIO $ do
    time <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "[%H:%M:%S]" time
    let entry = timestamp ++ " " ++ sender ++ " -> " ++ recipient ++ ": " ++ content ++ "\n"
    
    -- Exception Handling: Ignore log write failures
    result <- try (appendFile "chat.log" entry) :: IO (Either IOException ())
    case result of
        Left _  -> return () -- Silently fail if logging fails
        Right _ -> return ()
