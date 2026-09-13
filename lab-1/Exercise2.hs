module Exercise2 where

import Test.QuickCheck

-- Time spent: 150mins
-- Dependencies: Test.QuickCheck

powerset :: [a] -> [[a]]
powerset xs = helper xs [[]]
  where
    helper [] curr = curr
    helper (head:rest) curr = helper rest (curr ++ [temp ++ [head] | temp <- curr])

prop_powersetCardinality :: Property
prop_powersetCardinality = forAll (chooseInt (0, 10)) (\n -> length (powerset [1..n]) == 2 ^ n)

main :: IO ()
main = do
  putStrLn "Exercise 2:"
  putStrLn $ "Powerset [1..3]: " ++ show (powerset [1..3])
  putStrLn "Cardinality tests:"
  quickCheck prop_powersetCardinality

-- === Output ===
-- Exercise 2:
-- Powerset [1..3]: [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]
-- Cardinality tests:
-- +++ OK, passed 100 tests.

-- Proof by induction that if A is a finite set with |A| = n then |P(A)| = 2^n

-- Base case (n = 0):
--   Let A = {}. The only subset of {} is {} itself, so P({}) = {{}}.
--   Hence, |P({})| = 1 = 2^0. The base case holds.
-- Inductive hypothesis:
--   Assume that for any set A of size k, |P(A)| = 2^k.
-- Inductive step (n = k + 1):
--   When adding a new element x:
--     Every existing subset can either exclude, or include x.
--     This creates two copies of each subset doubling the total:
--   2 * 2^k = 2^(k + 1)
-- Hence, by induction, |P(A)| = 2^n holds for all n >= 0

-- Q1.) Is the cardinality property of the powerset hard to test? If you find that it is, can you give a reason why?

-- Yes, testing it is computationally expensive, hence hard, since the size of the powerset grows with O(2^n).
-- So, we constrained the testing domain to small values of [0..10] to prevent OOM kills.

-- Q2.) Give your thoughts on the following issue: When you perform these tests, what are you testing actually?
-- Are you checking a mathematical fact? Or are you testing whether powerset satisfies a part of its specification?
-- Or are you testing something else still?

-- We are not testing a mathematical fact, since we proved it by induction earlier. Rather, we are actually testing our haskell code,
-- and specifically, its most basic property - cardinality. But to be fair, this test is not completely sufficient/unerring, since we don't check
-- what the generated subsets actually contain, we are solely checking the number of subsets.
