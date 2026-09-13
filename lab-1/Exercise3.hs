module Exercise3 where

import Lecture2
import Data.List (sortBy)

-- Time spent: 70mins
-- Dependencies: Lecture2, Data.List (sortBy)

data Strength
  = Stronger
  | Weaker
  | Equal
  deriving (Show, Eq)

-- Workshop 2 Exercise 3 properties
p1 :: Int -> Bool
p1 x = even x && x > 3

p2 :: Int -> Bool
p2 x = even x || x > 3

p3 :: Int -> Bool
p3 x = (even x && x > 3) || even x

p4 :: Int -> Bool
p4 = even

domain :: [Int]
domain = [(-10)..10]

props :: [Int -> Bool]
props = [p1, p2, p3, p4]

compareStrength :: (Int -> Bool) -> (Int -> Bool) -> Strength
compareStrength p q
  | stronger domain p q && stronger domain q p = Equal
  | stronger domain p q = Stronger
  | otherwise = Weaker

getStrengthComparisons :: [Int -> Bool] -> [[Strength]]
getStrengthComparisons xs = map (\x -> map (\y -> compareStrength x y) xs) xs

main :: IO ()
main = do
  putStrLn "Exercise 3:"

  let comparisons = getStrengthComparisons props
  let scores = map (\row -> length (filter (== Stronger) row)) comparisons
  putStrLn $ "Scores (number of properties each is stronger than the other for [p1,p2,p3,p4]): " ++ show scores
  let names = ["p1", "p2", "p3", "p4"]
  let ranked = map fst (sortBy (\(_, s1) (_, s2) -> compare s2 s1) (zip names scores))
  putStrLn $ "Sorted order by score: " ++ show ranked

-- === Output ===
-- Exercise 3:
-- Scores (number of properties each is stronger than the other for [p1,p2,p3,p4]): [3,0,1,1]
-- Sorted order by score: ["p1","p3","p4","p2"]