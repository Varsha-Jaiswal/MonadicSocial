module Logger
  ( logMsg,
    logChat,
  )
where

import Control.Exception (IOException, try)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (appendFile, hPutStrLn, stderr)

-- | Logs a message to stdout with a timestamp.
logMsg :: String -> IO ()
logMsg msg = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "[%H:%M:%S]" now
  putStrLn $ timestamp ++ " " ++ msg

-- | Appends a chat message to a persistent log file.
-- Format: [Time] Sender -> Recipient: Message
-- Catches IOExceptions (e.g., file locked) to prevent crashing the simulation.
logChat :: String -> String -> String -> IO ()
logChat sender recipient content = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "[%H:%M:%S]" now
  let entry = timestamp ++ " " ++ sender ++ " -> " ++ recipient ++ ": " ++ content ++ "\n"

  -- Exception Handling: Ignore log write failures
  result <- try (appendFile "chat.log" entry) :: IO (Either IOException ())
  case result of
    Left _ -> return () -- Silently fail if logging fails
    Right _ -> return ()
