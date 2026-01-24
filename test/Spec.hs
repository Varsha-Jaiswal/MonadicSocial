import Reputation (calculateScore, rateInteraction)
import Test.QuickCheck

-- | Property: Score should always be between 0.0 and 10.0
prop_scoreInRange :: [Int] -> Property
prop_scoreInRange ratings =
  let score = calculateScore ratings
   in label "Score Range" $ score >= 0.0 && score <= 10.0

-- | Property: Rating should always be between 1 and 5
prop_ratingInRange :: String -> Bool
prop_ratingInRange content =
  let rating = rateInteraction content
   in rating >= 1 && rating <= 5

main :: IO ()
main = do
  putStrLn "Running QuickCheck properties..."
  quickCheck prop_scoreInRange
  quickCheck prop_ratingInRange
