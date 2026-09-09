module Exercise2 where

import Test.QuickCheck

-- Time spent: 150min

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
  quickCheck prop_powersetCardinality

