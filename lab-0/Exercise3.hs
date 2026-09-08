module Exercise3 where

import Data.List
import Test.QuickCheck
import Lab0 hiding (reversibleStream)

-- Time spent: ~45 minutes

-- 'primes' and 'prime' are from Lab0
reversibleStream :: [Integer]
reversibleStream = [p | p <- takeWhile (< 10000) primes, prime (reversal p)]

-- TESTS --

-- Property: after reversing primes twice we always get the original prime
prop_streamReversingTwice :: Bool
prop_streamReversingTwice =
  all (\p -> reversal (reversal p) == p) reversibleStream

-- Property: every reverse of a num from reversibleStream is a prime
prop_reversedElemIsPrime :: Bool
prop_reversedElemIsPrime =
  all (\p -> prime (reversal p)) reversibleStream

-- Property: all nums in reversibleStream are prime
prop_allPrime :: Bool
prop_allPrime =
  all (\p -> prime p) reversibleStream

-- Property: if num is in reversibleStream, then its reverse shall be as well
prop_ReverseIsElemOfStream :: Bool
prop_ReverseIsElemOfStream =
  all (\p -> reversal p `elem` reversibleStream) reversibleStream

-- Property: unique values
prop_uniqueValues :: Bool
prop_uniqueValues =
  nub reversibleStream == reversibleStream

-- Property: max value <10k
prop_checkMaxValue :: Bool
prop_checkMaxValue =
  all (\p -> p > 1 && p < 10000) reversibleStream

main :: IO ()
main = do
  putStrLn "Exercise 3:"
  putStrLn $ "- Property: reversing twice: " ++ show prop_streamReversingTwice
  putStrLn $ "- Property: reverse of an element in our stream is prime: " ++ show prop_reversedElemIsPrime
  putStrLn $ "- Property: all prime: " ++ show prop_allPrime
  putStrLn $ "- Property: reverse is an element of our stream: " ++ show prop_ReverseIsElemOfStream
  putStrLn $ "- Property: unique values: " ++ show prop_uniqueValues
  putStrLn $ "- Property: max value <10k: " ++ show prop_checkMaxValue

-- Exercise 3:
-- - Property: reversing twice: True
-- - Property: reverse of an element in our stream is prime: True
-- - Property: all prime: True
-- - Property: reverse is an element of our stream: True
-- - Property: unique values: True
-- - Property: max value <10k: True
