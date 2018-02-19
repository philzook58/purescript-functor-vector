module Semiring1 where

import Prelude
import Data.Functor.Compose

class Semiring1 f where
   add1 :: forall a. Semiring a => f a -> f a -> f a 
   zero1 :: forall a. Semiring a => f a
   mul1 :: forall a. Semiring a => f a -> f a -> f a 
   one1 :: forall a. Semiring a => f a


instance composeSemiring1 :: (Semiring1 g, Applicative f) => Semiring1 (Compose f g) where
  add1 (Compose x) (Compose y) = Compose (add1 <$> x <*> y)
  zero1 = Compose $ pure zero1
  mul1 (Compose x) (Compose y) = Compose (mul1 <$> x <*> y)
  one1 = Compose $ pure one1

{-
instance oneImpliesZero :: (Semiring1 f, Semiring a) => Semiring (f a) where
  add = add1
  zero = zero1
  mul = mul1
  one = one1
-}
{-
  --No
instance zeroImpliesOnes :: (Semiring (f a), Semiring a) => Semiring1 f where
  add = add1
  zero = zero1
  mul = mul1
  one = one1
-}
-- To Do: Product and Coproduct