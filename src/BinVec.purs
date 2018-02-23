module BinVec where

import Prelude
import Representable
import Data.Functor.Compose
import DenseKron
import Semiring1 
import Data.Identity
import Dottable
import Data.Foldable
import Data.Monoid
import Control.Apply

data V2 a = V2 a a

{-
type V4 a = V2 (V2 a)
type V8 a = V2 (V4 a)
type V16 a = V2 (V8 a)
type V32 a = V2 (V16 a)
type V64 a = V2 (V32 a)
type V128 a = V2 (V64 a)
type V256 a = V2 (V128 a)
type V512 a = V2 (V256 a)
type V1024 a = V2 (V512 a)
type V2048 a = V2 (V1024 a)
-}

type V4 a = C2 V2 a
type V8 a = C3 V2 a
type V16 a = C4 V2 a
type V32 a = C5 V2 a
type V64 a = C6 V2 a
type V128 a = C7 V2 a
type V256 a = C8 V2 a
type V512 a = C9 V2 a
type V1024 a = C10 V2 a


--type V4' a = CKron V2 (CKron V2 V2) a
-- The thing that makes V4'' not so bad is the newtype deriving
newtype V4'' a = V4'' (CKron V2 V2 a)

--type V4''' a = C2 V2 a

instance functorV2 :: Functor V2 where
   map f (V2 x y) = V2 (f x) (f y)

instance foldableV2 :: Foldable V2 where
   foldMap f (V2 x y) = (f x) <> (f y)
   foldl f b (V2 x y) = f (f b x) y
   foldr f b (V2 x y) = f x (f y b)



instance applyV2 :: Apply V2 where
  apply (V2 f g) (V2 a b) = V2 (f a) (g b)

instance applicativeV2 :: Applicative  V2 where
  pure x = V2 x x

instance monadV2 :: Bind V2 where
  bind z f = join' $ map f z where
                          join' (V2 (V2 a _) (V2 _ b)) = V2 a b



toArray :: forall f a. Applicative f => Monoid (f a) => V2 a -> f a
toArray = foldMap pure 

instance v2Rep :: Representable V2 Boolean where
   tabulate f = V2 (f false) (f true)
   index (V2 x y) b = if b then y else x

instance showV2 :: Show a => Show (V2 a) where
   show (V2 x y) = "V2 " <> show x <> " " <> show y

instance v2Semiring :: (Semiring a) => Semiring (V2 a) where
   --add (V2 x y) (V2 a b) = V2 (add x a) (add y b)
   add = lift2 add
   zero = pure zero -- V2 zero zero
   --mul (V2 x y) (V2 a b) = V2 (x * a) (y * b)
   mul = lift2 mul
   one = pure one -- V2 one one

instance ringV2 :: (Ring a) => Ring (V2 a) where
   --sub (V2 x y) (V2 a b) = V2 (x-a) (y-b)
   sub = lift2 sub

{-
instance semiring1 :: Semiring1 V2 where
  add1 = add
  zero1 = zero
  mul1 = mul
  one1 = one
-}
{-
instance dottableV2 :: Dottable1 V2 V2 Identity where
  dot1 (V2 x y) (V2 a b) = Identity $ (dot x a) + (dot y b) 
-}
instance dottableV2 :: (Semiring c, Dottable a b c) => Dottable (V2 a) (V2 b) (Identity c) where
  dot (V2 x y) (V2 a b) = Identity $ (dot x a) + (dot y b) 
{-
instance metricV2 :: Metric V2 V2 Identity where
  mtabulate f = V2 x y where
  					Identity x = f (V2 one zero)
  					Identity y = f (V2 zero one)
-}
--buildfromRep :: forall a b f. Representable f a => (a -> b) -> 
--buildfromDot
