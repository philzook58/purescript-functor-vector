module Data.Dual where

import Prelude
import Data.Dottable (class Dottable)


newtype Dual f a = Dual (f a -> a)

instance dualDot :: Dottable (Dual f a) (f a) a where
   dot (Dual f) x = f x

instance semiringdual :: Semiring a => Semiring (Dual f a) where
  add (Dual x) (Dual y) = Dual $ add <$> x <*> y
  zero = Dual $ const zero
  mul (Dual x) (Dual y) = Dual $ mul <$> x <*> y
  one = Dual $ const one 

--newtype Dual' f c a = Dual' (f a -> c)
-- f a -> a
-- f b
-- dot :: a -> b -> c
-- f a -> b -> c
-- f (a->c)
-- f (b->c)->a 



{-
instance dualDot :: Dottable (f a) (Dual f a) (FKron f (Dual f) a) where
   dot (Dual f) x = Identity $ f x
-}
--ddot (Dual f) x = f x

-- Dual f (f a) = CKron (Dual f) f a
-- f f a -> f a 
-- which is signature of join
-- which comes from the indexing adjunction?
-- make index module and adjunction
-- the other way
-- f (f a -> a)
-- f a -> f a