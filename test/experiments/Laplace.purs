module Laplace where

import Prelude
import Data.BinMat
import Data.Functor.Representable
import Data.DivisionRing
--import Data.Proxy

-- little endian K matrix. Maybe
mK :: forall a. Ring a => M2 a    -- forall f. Functor f => Semiring (f Number) => M2 (f Number)
mK = M2 ntwo one one ntwo where
             				ntwo = zero - one - one -- map (_ * -2.0) one

mK'' :: Int -> Int -> Number
mK'' i j | i == j - 1 = -1.0
mK'' i j | i == j + 1 = -1.0
mK'' i j | i == j = 2.0
mK'' _ _ = 0.0

mK' :: M8 Number
mK' = fillFromZIndex mK''

fD' :: Int -> Int -> Number
fD' i j | i == j = -1.0
fD' i j | i == j + 1 = 1.0
fD' _ _ = 0.0

{-
fD :: Proxy2 f -> f Number
fD _ = fillFromZIndex fD'
-}
-- forward difference
fD :: M4 Number
fD = fillFromZIndex fD'
-- integration
fI = recip fD

bD' :: Int -> Int -> Number
bD' i j | i == j = 1.0
bD' i j | i == j - 1 = -1.0
bD' _ _ = 0.0

--bD = fillFromZIndex bD'

bD :: M4 Number
bD = fillFromZIndex bD'

i = Complex zero one
neg1 = negate one

mag2 :: forall a. Semiring a => Complex a -> a
mag2 (Complex r i) = r * r + i * i

conj (Complex r i) = Complex r (negate i)

-- Class Dualizable a b where
-- instance Dualiazble a b => Dualizable (Complex a) (Complex b)
-- dual (Complex x y) =  Complex (dual x) (negate (dual y))
-- instance  Dualizable Number Number where
-- dual = id
sigmax :: M2 (Complex Number)
sigmax = M2 zero one one zero

sigmaz :: M2 (Complex Number)
sigmaz = M2 one zero zero neg1

sigmay :: M2 (Complex Number)
sigmay = M2 zero (negate i) i zero

-- can put complex on the inside or outside of the stack.
data Complex a = Complex a a

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

