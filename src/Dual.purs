module Dual where

import Prelude
import Dottable
import Data.Identity

newtype Dual f a = Dual (f a -> a)
--newtype Dual' f c a = Dual' (f a -> c)
-- f a -> a
-- f b
-- dot :: a -> b -> c
-- f a -> b -> c
-- f (a->c)
-- f (b->c)->a 
instance dualDot :: Dottable (Dual f a) (f a) (Identity a) where
   dot (Dual f) x = Identity $ f x

instance semiringdual :: Semiring a => Semiring (Dual f a) where
  add (Dual x) (Dual y) = Dual $ add <$> x <*> y
  zero = Dual $ const zero
  mul (Dual x) (Dual y) = Dual $ mul <$> x <*> y
  one = Dual $ const one 


--ddot (Dual f) x = f x
