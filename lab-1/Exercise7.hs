module Exercise7 where

import Lecture3

-- Time spent: 120mins
-- Dependencies: Lecture3

-- helper that distributes OR over AND
-- (A ∧ B) ∨ C  ==>  (A ∨ C) ∧ (B ∨ C)
dist :: Form -> Form -> Form
dist (Cnj fs) f = Cnj [dist x f | x <- fs]
dist f (Cnj fs) = Cnj [dist f x | x <- fs]
dist f1 f2 = Dsj [f1, f2]

-- recursively converts a NNF formula into CNF
cnf' :: Form -> Form
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj [f]) = cnf' f
cnf' (Dsj (f:fs)) = dist (cnf' f) (cnf' (Dsj fs))
cnf' f = f

-- arrowfree from Lecture3 removes ==> and <=>
-- nnf from Lecture3 pushes negations inwards + removes a double-negation
-- cnf' distributes OR over AND to ensure compliance with CNF
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree

-- 2 formulas are logically equivalent when they have identical truth values
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals (Equiv f1 f2))

-- Property checking that CNF remains equivalent
prop_cnfEquiv :: Form -> Bool
prop_cnfEquiv f = equiv f (cnf f)

-- test formulas
testForm1, testForm2, testForm3, testForm4 :: Form
testForm1 = Impl (Prop 1) (Prop 2)
testForm2 = Dsj [Prop 1, Cnj [Prop 2, Prop 3]]
testForm3 = Equiv (Prop 1) (Prop 2)
testForm4 = Dsj [Cnj [Prop 1, Prop 2], Cnj [Prop 3, Prop 4]]

main :: IO ()
main = do
  putStrLn "Exercise 7:"
  putStrLn "Converting test formulas to CNF:"
  putStrLn $ "1. p ==> q:"
  putStrLn $ "Original: " ++ show testForm1
  putStrLn $ "CNF: " ++ show (cnf testForm1)
  putStrLn $ "Equivalent?: " ++ show (prop_cnfEquiv testForm1)

  putStrLn $ "\n2. p || (q && r):"
  putStrLn $ "Original: " ++ show testForm2
  putStrLn $ "CNF: " ++ show (cnf testForm2)
  putStrLn $ "Equivalent?: " ++ show (prop_cnfEquiv testForm2)

  putStrLn $ "\n3. p <=> q:"
  putStrLn $ "Original: " ++ show testForm3
  putStrLn $ "CNF: " ++ show (cnf testForm3)
  putStrLn $ "Equivalent?: " ++ show (prop_cnfEquiv testForm3)

  putStrLn $ "\n4. (p && q) || (r && s):"
  putStrLn $ "Original: " ++ show testForm4
  putStrLn $ "CNF: " ++ show (cnf testForm4)
  putStrLn $ "Equivalent?: " ++ show (prop_cnfEquiv testForm4)

-- === Output ===
-- Exercise 7:
-- Converting test formulas to CNF:
-- 1. p ==> q:
-- Original: (1==>2)
-- CNF: +(-1 2)
-- Equivalent?: True

-- 2. p || (q && r):
-- Original: +(1 *(2 3))
-- CNF: *(+(1 2) +(1 3))
-- Equivalent?: True

-- 3. p <=> q:
-- Original: (1<=>2)
-- CNF: *(*(+(1 -1) +(1 -2)) *(+(2 -1) +(2 -2)))
-- Equivalent?: True

-- 4. (p && q) || (r && s):
-- Original: +(*(1 2) *(3 4))
-- CNF: *(*(+(1 3) +(1 4)) *(+(2 3) +(2 4)))
-- Equivalent?: True