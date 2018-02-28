-- | This is Phil Freeman's, but I needed the contructor for Taylor. Sorry.

-- | Taylor series as lazy lists of coefficients.

module Data.Taylor
  ( Taylor(..)
  , coefficients
  , constant
  , x
  , d
  , integrate
  , eval0
  ) where

import Prelude

import Control.Lazy (class Lazy, defer)
import Data.List.Lazy (List, head, iterate, repeat, tail, uncons, zipWith, (:))
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)

-- | A Taylor series, with coefficients in the specified `Semiring`.
-- |
-- | By varying the base `Semiring`, we can do various interesting things:
-- |
-- | * Using `Number`, we can compute arbitrary higher order derivatives of functions.
-- | * Using `Complex`, we can compute path derivatives of functions.
-- | * Using the `Free` `Semiring`, we can implement symbolic differentiation.
newtype Taylor a = Taylor (List a)

-- | Extract the coefficients as a lazy list.
coefficients :: forall a. Taylor a -> List a
coefficients (Taylor cs) = cs

-- | A Taylor series representing a constant function.
constant :: forall a. Semiring a => a -> Taylor a
constant a = Taylor (a : repeat zero)

-- | Evaluate a Taylor series at zero.
eval0 :: forall a. Taylor a -> a
eval0 (Taylor cs) = unsafePartial fromJust (head cs)

-- | The derivative of a Taylor series.
d :: forall a. Semiring a => Taylor a -> Taylor a
d (Taylor cs) = Taylor (zipWith mul (unsafePartial fromJust (tail cs)) powers) where
  powers = iterate (add one) one

-- | The integral of a Taylor series.
integrate :: forall a. Field a => Taylor a -> Taylor a
integrate (Taylor cs) = Taylor (zero : zipWith div cs powers) where
  powers = iterate (add one) one

-- | A Taylor series which has a non-zero coefficient only for the x term.
x :: forall a. Semiring a => Taylor a
x = Taylor (zero : one : repeat zero)

derive newtype instance lazyTaylor :: Lazy (Taylor a)
derive newtype instance functorTaylor :: Functor Taylor

instance semiringTaylor :: Semiring a => Semiring (Taylor a) where
  zero = Taylor (repeat zero)
  add (Taylor cs1) (Taylor cs2) = Taylor (zipWith add cs1 cs2)
  one = Taylor (one : repeat zero)
  mul (Taylor cs1) t@(Taylor cs2) =
    unsafePartial
      case uncons cs1, uncons cs2 of
        Just o1, Just o2 -> Taylor ((o1.head * o2.head) : defer \_ ->
                              let t1 = Taylor (map (mul o1.head) o2.tail)
                                  t2 = Taylor o1.tail * t
                              in coefficients (t1 + t2))

instance ringTaylor :: Ring a => Ring (Taylor a) where
  sub (Taylor cs1) (Taylor cs2) = Taylor (zipWith sub cs1 cs2)

instance commutativeRingRing :: CommutativeRing a => CommutativeRing (Taylor a) where

instance euclideanRing :: (Eq a, Field a) => EuclideanRing (Taylor a) where
  degree _ = 1
  div (Taylor cs1) t@(Taylor cs2) =
    unsafePartial
      case uncons cs1, uncons cs2 of
        Just o1, Just o2
          | o1.head == zero && o2.head == zero -> Taylor o1.tail / Taylor o2.tail
          | otherwise -> Taylor ((o1.head / o2.head) : defer \_ ->
                           let t1 = Taylor o1.tail
                               t2 = Taylor (map (mul (o1.head / o2.head)) o2.tail)
                           in coefficients ((t1 - t2) / t))

  mod _ _ = zero

instance fieldRing :: (Eq a, Field a) => Field (Taylor a)