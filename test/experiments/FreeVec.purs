module Data.FreeVec where

import Prelude
import Data.CatList
import Data.Tuple
--kind of a replication of FreeKron
-- also should we not use a Map data structure
newtype FreeVec b a = FreeVec (CatList (Tuple b a))


{-
instance semiringFreeVec :: Semiring a => Semiring (FreeVec b a) where
  add (FreeVec x) (FreeVec y) = FreeVec $ x <> y
  zero = FreeVec $ mempty
  mul 


Representable BoundedEnum b => 

-}