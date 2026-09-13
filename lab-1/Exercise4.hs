module Exercise4 where

import Test.QuickCheck
import Data.List (nub)

-- Time spent: 45mins
-- Dependencies: Test.QuickCheck, Data.List (nub)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (`elem` ys) xs && length xs == length ys

-- Property: every list is a permutation of itself
prop_isPermutationItself :: [Int] -> Bool
prop_isPermutationItself xs = isPermutation clean clean
  where clean = nub xs

-- Property: reversing a list is a permutation of original
prop_isPermutationReverse :: [Int] -> Bool
prop_isPermutationReverse xs = isPermutation clean (reverse clean)
  where clean = nub xs

-- Property: symmetry (if xs is perm of ys, then ys is perm of xs)
prop_isPermutationSymmetry :: [Int] -> [Int] -> Bool
prop_isPermutationSymmetry xs ys =
  isPermutation (nub xs) (nub ys) == isPermutation (nub ys) (nub xs)

main :: IO ()
main = do
  putStrLn "Exercise 4:"
  putStrLn $ "Is [1,2,3] permutation of [3,1,2]?: " ++ show (isPermutation [1,2,3] [3,1,2])
  putStrLn $ "Is [1,2] permutation of [1,2,3]?: " ++ show (isPermutation [1,2] [1,2,3])
  putStrLn $ "Is [1,2,3] permutation of [1,2,4]?: " ++ show (isPermutation [1,2,3] [1,2,4])

  putStrLn "\nQuickCheck tests:"
  putStrLn "Property: list is permutation of itself:"
  quickCheck prop_isPermutationItself
  putStrLn "Property: reverse of list is permutation:"
  quickCheck prop_isPermutationReverse
  putStrLn "Property: symmetry:"
  quickCheck prop_isPermutationSymmetry

-- === Output ===
-- Exercise 4:
-- Is [1,2,3] permutation of [3,1,2]?: True
-- Is [1,2] permutation of [1,2,3]?: False
-- Is [1,2,3] permutation of [1,2,4]?: False

-- QuickCheck tests:
-- Property: list is permutation of itself:
-- +++ OK, passed 100 tests.
-- Property: reverse of list is permutation:
-- +++ OK, passed 100 tests.
-- Property: symmetry:
-- +++ OK, passed 100 tests.


-- Q1.) You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?

-- QuickCheck generates lists randomly, so they may contain duplicates, however our isPermutation function does not account for it.
-- Hence, we are using `nub` to get rid of potential duplicates.

-- Q2.) Can you automate the test process? Also, use QuickCheck.

-- Yes, we can, using QuickCheck like in the previous exercises, we check whether the list is a permutation of itself, 
-- whether the reverse of the list is a permutation, or whether list A is a permutation of list B, then B should be a permutation of A.