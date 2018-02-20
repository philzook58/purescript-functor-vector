module Semiring1 where

import Prelude
import Data.Identity
import Control.Apply

class Semiring1 f where
   add1 :: forall a. Semiring a => f a -> f a -> f a 
   zero1 :: forall a. Semiring a => f a
   mul1 :: forall a. Semiring a => f a -> f a -> f a 
   one1 :: forall a. Semiring a => f a

add1Default :: forall a f. Semiring a => Semiring (f a) => f a -> f a -> f a 
add1Default = add


instance identitySemiring :: Semiring1 Identity where
  add1 = lift2 add
  one1 = Identity one
  mul1 = lift2 mul
  zero1 = Identity zero

class Semiring1 f <= Ring1 f where
   sub1 :: forall a. Ring a => f a -> f a -> f a 

class Ring1 f <= DivisionRing1 f where
   recip1 :: forall a. DivisionRing a => f a -> f a


-- direct extension of semiring. Internally use this, but probably externally can use ordinayr
-- semiring?
{-
class Semiring' a where
  add'  :: a -> a -> a
  zero' :: a
  mul'  :: a -> a -> a
  one'  :: a

instance semiringExtend :: Semiring a => Semiring' a where
  add' = add
  zero' = zero
  mul' = mul
  one' = one
  -}
