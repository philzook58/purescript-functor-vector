module FunVec where 

import Prelude
import Data.Functor.Representable (class Representable)

newtype FunVec a b = FunVec (a -> b)



instance funvecsemiring :: Semiring b => Semiring (FunVec a b) where
   add (FunVec f) (FunVec g) = FunVec $ f + g
   zero = FunVec $ const zero
   mul (FunVec f) (FunVec g) = FunVec $ f * g
   one = FunVec $ const one
{-
instance dottableV2 :: (Semiring c, Dottable a b c) => Dottable (FunVec r a) (FunVec r b) (Identity c) where
  dot (V2 x y) (V2 a b) = Identity $ (dot x a) + (dot y b) 
  -}
instance functorFunVec :: Functor (FunVec a) where
  map f (FunVec x) = FunVec $ map f x

instance repFunVec :: Representable (FunVec a) a where
  tabulate x = FunVec x 
  index (FunVec x) = x 