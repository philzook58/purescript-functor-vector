module Data.FunVec where 

import Prelude
import Data.Functor.Representable (class Representable, basis)
import Data.Foldable (sum)
import Data.Enum
import Control.Apply (lift2)
import Data.Dottable (class Dottable, dot)

newtype FunVec a b = FunVec (a -> b)



instance semiringFunVec :: Semiring b => Semiring (FunVec a b) where
   add (FunVec f) (FunVec g) = FunVec $ f + g
   zero = FunVec $ const zero
   mul (FunVec f) (FunVec g) = FunVec $ f * g
   one = FunVec $ const one

instance functorFunVec :: Functor (FunVec a) where
  map f (FunVec x) = FunVec $ map f x

instance representableFunVec :: Representable (FunVec a) a where
  tabulate x = FunVec x 
  index (FunVec x) = x 

instance dottableFunVecFunVec :: (BoundedEnum b, Semiring c) => Dottable (FunVec b c) (FunVec b c) c where
  dot (FunVec f) (FunVec g) = sum $ lift2 mul (map f basis) (map g basis)
