module Data.Rational
  ( Rational(..)
  , (%)
  , toNumber
  , fromInt
  ) where

import Prelude
import qualified Data.Int as Int
import Data.Ratio

newtype Rational = Rational (Ratio Int)

instance showRational :: Show Rational where
  show (Rational (Ratio a b)) = show a ++ " % " ++ show b

instance eqRational :: Eq Rational where
  eq x y = eq' (reduce x) (reduce y)
    where
    eq' (Rational (Ratio a' b')) (Rational (Ratio c' d')) = a' == c' && b' == d'

instance ordRational :: Ord Rational where
  compare (Rational (Ratio a b)) (Rational (Ratio c d)) | a * d < b * c = LT
  compare (Rational (Ratio a b)) (Rational (Ratio c d)) | a * d == b * c = EQ
  compare _ _ = GT

instance semiringRational :: Semiring Rational where
  one = Rational $ one
  mul (Rational a) (Rational b) = reduce $ Rational $ a `mul` b
  zero = Rational $ zero
  add (Rational a) (Rational b) = reduce $ Rational $ a `add` b

instance ringRational :: Ring Rational where
  sub (Rational a) (Rational b) = reduce $ Rational $ a `sub` b

instance moduloSemiringRational :: ModuloSemiring Rational where
  mod _ _ = zero
  div (Rational a) (Rational b) = reduce $ Rational $ a `div` b

instance divisionRingRational :: DivisionRing Rational

instance numRational :: Num Rational

infixl 7 %

(%) :: Int -> Int -> Rational
(%) x y = reduce $ Rational $ Ratio x y

toNumber :: Rational -> Number
toNumber (Rational (Ratio a b)) = Int.toNumber a / Int.toNumber b

fromInt :: Int -> Rational
fromInt i = Rational $ Ratio i 1

reduce :: Rational -> Rational
reduce (Rational (Ratio a b)) =
  let x = a / gcd a b
      y = b / gcd a b
  in Rational $ Ratio (x * signum y) (abs y)

gcd :: Int -> Int -> Int
gcd m n
  | n == 0 = m
  | otherwise = gcd n (m `mod` n)

signum :: Int -> Int
signum 0 = 0
signum x' | x' < 0 = -1
signum _ = 1

foreign import abs :: Int -> Int

