import Reputation (calculateScore, rateInteraction)
import Test.QuickCheck

-- | Property: Score should always be between 0.0 and 10.0
-- | Property: Score should always be between 0.0 and 10.0
prop_scoreInRange :: Property
prop_scoreInRange =
  forAll (listOf (elements [1 .. 5])) $ \ratings ->
    let score = calculateScore ratings
     in label "Score Range" $ score >= 0.0 && score <= 10.0

-- | Property: Rating should always be between 1 and 5
prop_ratingInRange :: Int -> Bool
prop_ratingInRange seed =
  let rating = rateInteraction seed
   in rating >= 1 && rating <= 5

main :: IO ()
main = do
  putStrLn "Running QuickCheck properties..."
  quickCheck prop_scoreInRange
  quickCheck prop_ratingInRange
