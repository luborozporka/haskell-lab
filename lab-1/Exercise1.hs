module Exercise1 where

import Test.QuickCheck

-- Time spent: 30 mins

factorial :: Integer -> Integer
factorial n 
  | n < 0 = error "In mathematics, the factorial of a non-negative integer n -Source: https://en.wikipedia.org/wiki/Factorial"
  | n == 0 = 1
  | n > 0 = n * factorial (n-1)

-- Property: next step factorial matches
prop_nextStepMatches :: Positive Integer -> Bool
prop_nextStepMatches (Positive n) = factorial (n + 1) == factorial n * (n + 1)

-- Property: factorial is increasing with larger n
prop_factorialIncreasing :: Property
prop_factorialIncreasing =
  forAll (chooseInteger (0, 10)) (\n -> factorial (n + 1) >= factorial n)

main :: IO ()
main = do
  putStrLn "Exercise 1:"
  putStrLn "Property: for positive ints next step factorial matches:"
  quickCheck prop_nextStepMatches
  putStrLn "Property: factorial is increasing with larger n:"
  quickCheck prop_factorialIncreasing

