module Data.Complex where

import Prelude
import Data.DivisionRing
import Math

data Complex a = Complex a a
-- this is asking for trouble. i1 maybe?
i = Complex zero one
neg1 = negate one

clift x = Complex x zero
real (Complex x _) = x
imag (Complex _ y) = y

cexp (Complex x y) = Complex (ex*(cos y)) (ex*(sin y))  where ex = (exp x)

clog z@(Complex x y) = Complex ((log (mag2 z)) * 0.5) (atan2 y x)

cpow :: Complex Number -> Number -> Complex Number
cpow z n = cexp $ (clift n) * (clog z)

spow :: forall a. Semiring a => a -> Int -> a
spow x = go
  where
  go :: Int -> a
  go p
    | p <= 0         = one
    | p == 1         = x
    | p `mod` 2 == 0 = let x' = go (p/2) in x' * x'
    | otherwise      = let x' = go (p/2) in x' * x' * x


mag2 :: forall a. Semiring a => Complex a -> a
mag2 (Complex r i) = r * r + i * i

conj (Complex r i) = Complex r (negate i)

-- Complex numbers can be thought of as a compressed representation of matrices of a particular form.
-- same goes for quaternions and some other special guys
instance functorComplex :: Functor Complex where
  map f (Complex x y) = Complex (f x) (f y)

instance complexSemiRing :: Ring a => Semiring (Complex a) where
   add (Complex x y) (Complex a b) = Complex (x + a) (y + b)
   zero = Complex zero zero
   mul (Complex x y) (Complex a b) = Complex (x*a - y*b) (x*b + y*a)
   one = (Complex one zero)

instance ringComplex :: Ring a => Ring (Complex a) where
   sub (Complex x y) (Complex a b) = Complex (x - a) (y - b)

instance divisionRingComplex :: DivisionRing a => DivisionRing (Complex a) where
   recip z = map (leftDiv (mag2 z)) (conj z)

instance showComplex :: Show a => Show (Complex a) where
  show (Complex r i) = show r <> " + " <> show i <> "i"