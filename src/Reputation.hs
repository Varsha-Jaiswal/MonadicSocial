module Reputation
  ( calculateScore,
    rateInteraction,
  )
where

-- | Calculate a normalized reputation score (0.0 to 10.0) from a list of ratings.
-- Ratings are integers from 1 to 5.
-- Returns 5.0 if no ratings exist (Neutral start).
calculateScore :: [Int] -> Double
calculateScore [] = 5.0
calculateScore ratings =
  let total = sum ratings
      count = length ratings
      avg = fromIntegral total / fromIntegral count
   in avg * 2.0 -- Scale 1-5 to 2-10 roughly (or just keep it simple)

-- | Pure function to determine a rating for an interaction.
-- In a real system, this might depend on content sentiment.
-- Here, we just return a valid rating.
rateInteraction :: String -> Int
rateInteraction content
  | length content > 20 = 5 -- "High effort" message
  | length content > 10 = 4
  | otherwise = 3 -- "Low effort" message
