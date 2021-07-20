module Tests.DECA where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import DECA

prop_fromToRadix :: H.Property
prop_fromToRadix =
  H.property $ do
    x <- H.forAll . Gen.integral $ Range.linear 0 127
    let xs = x `toRadix` (128, 2)
    H.assert $ length xs == 7 && xs `fromRadix` 2 == x

prop_fromToMontgomery :: H.Property
prop_fromToMontgomery =
  H.property $ do
    x <- H.forAll . Gen.integral $ Range.linear 0 112
    x === (flip fromMontgomery (128, 113) . flip toMontgomery (128, 113)) x

prop_multMontgomery :: H.Property
prop_multMontgomery =
  H.property $ do
    x <- H.forAll . Gen.integral $ Range.linear 0 112
    y <- H.forAll . Gen.integral $ Range.linear 0 112
    let x' = x `toMontgomery` (128, 113)
    let y' = y `toMontgomery` (128, 113)
    let (|*|) = multMontgomery (128, 113)
    x * y `mod` 113 === (x' |*| y') `fromMontgomery` (128, 113)

prop_multMontgomeryRadix :: H.Property
prop_multMontgomeryRadix =
  H.property $ do
    x <- H.forAll . Gen.integral $ Range.linear 0 112
    y <- H.forAll . Gen.integral $ Range.linear 0 112
    let x' = x `toMontgomery` (128, 113) `toRadix` (128, 2)
    let y' = y `toMontgomery` (128, 113) `toRadix` (128, 2)
    let (|*|) = multMontgomeryRadix ((128, 2), 113)
    x * y `mod` 113 === (x' |*| y') `fromRadix` 2 `fromMontgomery` (128, 113)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
