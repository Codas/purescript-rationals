module Data.Rational
  ( Rational(..)
  , rational
  , (%)
  , toNumber
  , fromInt
  ) where

import Prelude
import Data.Function.Uncurried (Fn2 (), runFn2)
import Data.BigInt (BigInt ())
import Data.BigInt as BigInt
import Data.Ratio (Ratio(Ratio))

newtype Rational = Rational (Ratio BigInt)

instance showRational :: Show Rational where
  show (Rational (Ratio a b)) = show a <> " % " <> show b

instance eqRational :: Eq Rational where
  eq x y = eq' (reduce x) (reduce y)
    where
    eq' (Rational (Ratio a' b')) (Rational (Ratio c' d')) = a' == c' && b' == d'

instance ordRational :: Ord Rational where
  compare (Rational (Ratio a b)) (Rational (Ratio c d)) | a * d < b * c = LT
  compare (Rational (Ratio a b)) (Rational (Ratio c d)) | a * d == b * c = EQ
  compare _ _ = GT

instance semiringRational :: Semiring Rational where
  one = Rational one
  mul (Rational a) (Rational b) = reduce $ Rational $ a `mul` b
  zero = Rational zero
  add (Rational a) (Rational b) = reduce $ Rational $ a `add` b

instance ringRational :: Ring Rational where
  sub (Rational a) (Rational b) = reduce $ Rational $ a `sub` b

instance commutativeRingRational :: CommutativeRing Rational

instance euclideanRingRational :: EuclideanRing Rational where
  degree (Rational a) = degree a
  div (Rational a) (Rational b) = Rational $ a `div` b
  mod _ _ = Rational zero

instance fieldRational :: Field Rational


rational :: Int -> Int -> Rational
rational x y = reduce $ Rational $ Ratio (BigInt.fromInt x) (BigInt.fromInt y)
infixl 7 rational as %

toNumber :: Rational -> Number
toNumber (Rational (Ratio a b)) = BigInt.toNumber a / BigInt.toNumber b

fromInt :: Int -> Rational
fromInt i = Rational $ Ratio (BigInt.fromInt i) (BigInt.fromInt 1)

fromBigInt :: BigInt -> Rational
fromBigInt i = Rational $ Ratio i (BigInt.fromInt 1)

reduce :: Rational -> Rational
reduce (Rational (Ratio a b)) =
  let x = a / g
      y = b / g
      g = gcd a b
  in Rational $ Ratio (x * signum y) (BigInt.abs y)

gcd :: BigInt -> BigInt -> BigInt
gcd a b = runFn2 js_gcd a b

foreign import signum :: BigInt -> BigInt
foreign import js_gcd :: Fn2 BigInt BigInt BigInt
