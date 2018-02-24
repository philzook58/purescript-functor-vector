module Data.DirectSum where

import Prelude
import Data.Dottable (class Dottable, dot)


data DSum f g a = DSum (f a) (g a)

dswap :: forall f g a. DSum f g a -> DSum g f a
dswap (DSum x y) = DSum y x

instance semiringDSum :: (Semiring (f a), Semiring (g a)) => Semiring (DSum f g a) where
   add (DSum x y) (DSum a b) = DSum (add x a) (add y b)
   zero = DSum zero zero
   mul (DSum x y) (DSum a b) = DSum (mul x a) (mul y b)
   one = DSum one one

instance ringDSum :: (Ring (f a), Ring (g a)) => Ring (DSum f g a) where
   sub (DSum x y) (DSum a b) = DSum (x - a) (y - b)

instance divisibleRingDSum :: (DivisionRing (f a), DivisionRing (g a)) => DivisionRing (DSum f g a) where -- Direct sum of matrices
   recip (DSum a b) = DSum (recip a) (recip b)

instance dottableDSum :: (Dottable (f a) (f' b) (f'' c), Dottable (g a) (g' b) (g'' c)) => Dottable (DSum f g a) (DSum f' g' b) (DSum f'' g'' c) where
   dot (DSum x y) (DSum a b) = DSum (dot x a) (dot y b)