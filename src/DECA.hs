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

inverse :: Integer -> Integer -> Integer
inverse x y = a `mod` y where
  (a, b, c) = xgcd x y

propInverse :: Integer -> Integer -> Bool
propInverse 0 _ = True
propInverse _ 0 = True
propInverse 1 _ = True
propInverse _ 1 = True
propInverse x y = if y < 0 then True else if gcd x y == 1 then ((inverse x y)*x `mod` y) == 1 else True where
  gcd x y = c where
    (a, b, c) = xgcd x y

r = 2^3
n = 5
r_inverse = inverse r n
minus_n_inverse = inverse (-n) r

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
montMult 0 _ = 0
montMult _ 0 = 0
montMult a b = if s < n then s else s - n where
  t = a*b
  x = (t `mod` r)*minus_n_inverse `mod` r
  s = (t + x*n) `div` r

propMontgomeryIso4 :: Integer -> Integer -> Bool
propMontgomeryIso4 x y = (toMontgomery x `montMult` toMontgomery y) `mod` n == toMontgomery ((x*y) `mod` n) -- should work
