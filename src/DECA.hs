module DECA
  ( fromMontgomery
  , toMontgomery
  , fromRadix
  , toRadix
  , multMontgomery
  , multMontgomeryRadix
  , topEntity
  ) where

--

import qualified Prelude as P
import Clash.Prelude

--

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

inverseMod :: Integer -> Integer -> Integer
x `inverseMod` n = if a < 0 then a + n else a where
  (a, _, _) = xgcd x n

--

toRadix :: Integer -> (Integer, Integer) -> [Word]
x `toRadix` (m, r)
  | x < 0 = error "undefined"
  | r < 2 = error "undefined"
  | otherwise = case m of
    1 -> []
    _ -> fromInteger y : (q `toRadix` (m `div` r, r)) where (q, y) = x `divMod` r

fromRadix :: [Word] -> Integer -> Integer
xs `fromRadix` r
  | r < 2 = error "undefined"
  | otherwise = case xs of
    [] -> 0
    (x:xs') -> toInteger x + (xs' `fromRadix` r) * r

--

toMontgomery :: Integer -> (Integer, Integer) -> Integer
x `toMontgomery` (m, n) = x * m `mod` n

fromMontgomery :: Integer -> (Integer, Integer) -> Integer
fromMontgomery = reduce

reduce :: Integer -> (Integer, Integer) -> Integer
y `reduce` (m, n) = x `normalizeMod` n where
  minus_n_inverse = (-n) `inverseMod` m
  q = y `mod` m * minus_n_inverse `mod` m
  x = (y + q * n) `div` m

normalizeMod :: Integer -> Integer -> Integer
x `normalizeMod` n = if x < n then x else x - n

--

multMontgomery :: (Integer, Integer) -> Integer -> Integer -> Integer
multMontgomery (m, n) = flip reduce (m, n) `compose2` (*) where compose2 = (.) . (.)

multMontgomeryRadix :: ((Integer, Integer), Integer) -> [Word] -> [Word] -> [Word]
multMontgomeryRadix ((m, r), n) xs ys = P.foldl f 0 ys `toRadix` (m, r) where
  f s y = flip reduce (r, n) $ (xs `fromRadix` r) * (toInteger y) + s

--

type WordType = Signed 4

toRadix' :: Integer -> Vec 7 WordType
toRadix' x = unfoldrI f x where
  f i = (fromInteger r, q)
    where (q, r) = i `divMod` 2

fromRadix' :: Vec 7 WordType -> Integer
fromRadix' = foldr f 0 where
  f x z = toInteger x + z*2

multMontgomeryRadix' :: Integer -> Vec 7 WordType -> Vec 7 WordType -> Vec 7 WordType
multMontgomeryRadix' n xs ys = toRadix' $ foldl f 0 ys where
  f s y = flip reduce (2, n) $ (fromRadix' xs) * (toInteger y) + s

--

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Vec 7 WordType)
    -> Signal System (Vec 7 WordType)
    -> Signal System (Vec 7 WordType)
topEntity = exposeClockResetEnable $ liftA2 (|*|) where
    (|*|) = multMontgomeryRadix' 107
