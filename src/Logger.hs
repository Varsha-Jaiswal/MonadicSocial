module Logger
  ( logMsg,
  )
where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

-- | Logs a message to stdout with a timestamp.
logMsg :: String -> IO ()
logMsg msg = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "[%H:%M:%S]" now
  putStrLn $ timestamp ++ " " ++ msg
