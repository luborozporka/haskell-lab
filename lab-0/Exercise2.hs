module Exercise2 where

import Test.QuickCheck
import Lab0

-- Time spent: ~60 min
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

-- Triangle must have all sides positive, and it cannot have any side larger than or equal to the sum of the other 2.
-- An equilateral triangle is a triangle that has all sides of equal length.
-- Isosceles has to have two sides equal. It is intentionally placed in pattern-match after equilateral, so that scenario is taken care of.
-- Rectangular - Pythagorean theorem.
triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
  | x <= 0 || y <= 0 || z <= 0 = NoTriangle
  | x + y <= z || x + z <= y || y + z <= x = NoTriangle
  | x == y && y == z = Equilateral
  | x == y || y == z = Isosceles
  | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || z^2 + y^2 == x^2 = Rectangular
  | otherwise = Other

-- TESTS --

-- If all three sides are equal the triangle is equilateral
prop_equilateral :: Positive Integer -> Bool
prop_equilateral (Positive a) = triangle a a a == Equilateral

-- If triangle has non-positive sides, it's not a triangle
prop_notTriangle :: Integer -> Integer -> Integer -> Property
prop_notTriangle a b c =
  (a <= 0 || b <= 0 || c <= 0) ==> triangle a b c == NoTriangle

-- If the sum of any 2 sides is <= than the 3rd, it cannot be a triangle
prop_2sidesSmallerOrEqualThanThe3rd :: Positive Integer -> Positive Integer -> Positive Integer -> Property
prop_2sidesSmallerOrEqualThanThe3rd (Positive a) (Positive b) (Positive c) =
  (a + b <= c || a + c <= b || b + c <= a) ==> triangle a b c == NoTriangle

-- Simple 3,4,5 rectangular check
prop_rectangular :: Positive Integer -> Bool
prop_rectangular (Positive k) = triangle (3 * k) (4 * k) (5 * k) == Rectangular

-- 2 sides equal -> isosceles; 3 sides equal -> equilateral; else no triangle
prop_isosceles :: Positive Integer -> Positive Integer -> Bool
prop_isosceles (Positive a) (Positive b)
  | a == b = triangle a a b == Equilateral
  | 2 * a > b = triangle a a b == Isosceles
  | otherwise = triangle a a b == NoTriangle

main :: IO ()
main = do
  putStrLn "Exercise 2:"
  putStrLn "- Equilateral property:"
  quickCheck prop_equilateral
  putStrLn "- Negative sides != triangle property:"
  quickCheck prop_notTriangle
  putStrLn "- 2 sides <= that the 3rd property:"
  quickCheck prop_2sidesSmallerOrEqualThanThe3rd
  putStrLn "- 3, 4, 5 rectangular property:"
  quickCheck prop_rectangular
  putStrLn "- 2 sides equal -> isosceles; 3 sides equal -> equilateral; else no triangle property:"
  quickCheck prop_isosceles
  
-- Exercise 2:
-- - Equilateral property:
-- +++ OK, passed 100 tests.
-- - Negative sides != triangle property:
-- +++ OK, passed 100 tests; 7 discarded.
-- - 2 sides <= that the 3rd property:
-- +++ OK, passed 100 tests; 81 discarded.
-- - 3, 4, 5 rectangular property:
-- +++ OK, passed 100 tests.
-- - 2 sides equal -> isosceles; 3 sides equal -> equilateral; else no triangle property:
-- +++ OK, passed 100 tests.
