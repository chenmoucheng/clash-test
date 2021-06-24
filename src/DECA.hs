import Prelude
import Test.QuickCheck

xgcd :: Integer -> Integer -> (Integer, Integer, Integer)
xgcd x y
  | x < 0 = let (a, b, c) = xgcd (-x) y in (-a, b, c)
  | y < 0 = let (a, b, c) = xgcd x (-y) in (a, -b, c)
  | x < y = let (a, b, c) = xgcd y x in (b, a, c)
  | y == 0 = (1, 0, x)
  | otherwise = let
    (q, r) = x `divMod` y
    (a', b', c') = xgcd y r
  in (b', a' - q*b', c')

propXgcd :: Integer -> Integer -> Bool
propXgcd x y = a*x + b*y == c where
  (a, b, c) = xgcd x y

r = 2^3
n = 5
r_inverse = a `mod` n where
  (a, b, c) = xgcd r n

toMontgomery :: Integer -> Integer
toMontgomery x = x*r `mod` n

fromMontgomery :: Integer -> Integer
fromMontgomery y = y*r_inverse `mod` n

propMontgomeryIso1 :: Integer -> Bool
propMontgomeryIso1 x = fromMontgomery (toMontgomery (x `mod` n)) == x `mod` n

propMontgomeryIso2 :: Integer -> Integer -> Bool
propMontgomeryIso2 x y = (toMontgomery x + toMontgomery y) `mod` n == toMontgomery ((x + y) `mod` n)

propMontgomeryIso3 :: Integer -> Integer -> Bool
propMontgomeryIso3 x y = (toMontgomery x * toMontgomery y) `mod` n == toMontgomery ((x*y) `mod` n) -- should fail

montMult :: Integer -> Integer -> Integer
montMult x y = x*y*r_inverse `mod` n

propMontgomeryIso4 :: Integer -> Integer -> Bool
propMontgomeryIso4 x y = (toMontgomery x `montMult` toMontgomery y) `mod` n == toMontgomery ((x*y) `mod` n) -- should work
