module DirectSum where

import Prelude
import Semiring1

data DSum f g a = DSum (f a) (g a)

instance semiring1DSum :: (Semiring1 f, Semiring1 g) => Semiring1 (DSum f g) where
   add1 (DSum x y) (DSum a b) = DSum (add1 x a) (add1 y b)
   zero1 = DSum zero1 zero1
   mul1 (DSum x y) (DSum a b) = DSum (mul1 x a) (mul1 y b)
   one1 = DSum one1 one1

dswap :: forall f g a. DSum f g a -> DSum g f a
dswap (DSum x y) = DSum y x

instance semiringDSum :: (Semiring (f a), Semiring (g a)) => Semiring (DSum f g a) where
   add (DSum x y) (DSum a b) = DSum (add x a) (add y b)
   zero = DSum zero zero
   mul (DSum x y) (DSum a b) = DSum (mul x a) (mul y b)
   one = DSum one one

{-

data DSum a b = DSum a b
instance ringDSum :: (Ring a, Ring b) => Ring (DSum a b) where
   sub (DSum x y) (DSum a b) = DSum (x - a) (y - b)

instance divisibleDSum :: (DivisionRing a, DivisionRing b) => DivisionRing (DSum a b) where -- Direct sum of matrices
   recip (DSum a b) = DSum (recip a) (recip b)
   -}

-- Jeez. Do I need ring1 and divisionring1 too?