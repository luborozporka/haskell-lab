module Exercise1 where

import Test.QuickCheck
import Lab0

isIn1stQuartile :: [Float] -> [Float]
isIn1stQuartile = filter (<0.25)

isIn2ndQuartile :: [Float] -> [Float]
isIn2ndQuartile = filter (\x -> x>=0.25 && x<0.5)

isIn3rdQuartile :: [Float] -> [Float]
isIn3rdQuartile = filter (\x -> x>=0.5 && x<0.75)

isIn4thQuartile :: [Float] -> [Float]
isIn4thQuartile = filter (\x -> x>=0.75 && x<1)

-- Time spent: ~120-180 hour, due to additional self study
-- According to https://en.wikipedia.org/wiki/Statistical_significance,
-- "The significance level for a study is chosen before data collection, 
-- and is typically set to 5% or much lower - depending on the field of study."
-- Considering that the null hypothesis is that 10,000 randomly generated values are
-- unifromly distributed to these quartiles - (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1),
-- and each quartile shall contain roughly 2500 nums; we can define roughly as and interval
-- (95% of 2500 .. 105% of 2500), which corresponds to (2375..2625).
areInInterval :: [Int] -> Bool
areInInterval xs =
  let expected = fromIntegral (sum xs) / fromIntegral (length xs)
      lower = 0.95 * expected
      upper = 1.05 * expected
  in all (\x -> fromIntegral x > lower && fromIntegral x < upper) xs

main :: IO ()
main = do
  randomVals <- probs 10000
  let x1 = length (isIn1stQuartile randomVals)
  let x2 = length (isIn2ndQuartile randomVals)
  let x3 = length (isIn3rdQuartile randomVals)
  let x4 = length (isIn4thQuartile randomVals)
  putStrLn "Exercise 1:"
  putStrLn $ "Quartile counts [Q1, Q2, Q3, Q4]: " ++ show [x1, x2, x3, x4]
  putStrLn $ "Quartiles uniformly distributed? " ++ show (areInInterval [x1, x2, x3, x4])

-- Exercise 1:
-- Quartile counts [Q1, Q2, Q3, Q4]: [2479,2559,2510,2452]
-- Quartiles uniformly distributed? True