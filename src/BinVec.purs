module Data.BinVec where

import Prelude
import Data.Functor.Representable (class Representable)
import Data.DenseKron
import Data.Identity (Identity(..))
import Data.Dottable (class Dottable, dot)
import Data.Foldable (class Foldable, foldMap)
import Data.Monoid (class Monoid)
import Control.Apply (lift2)

data V2 a = V2 a a

type V4 a = C2 V2 a
type V8 a = C3 V2 a
type V16 a = C4 V2 a
type V32 a = C5 V2 a
type V64 a = C6 V2 a
type V128 a = C7 V2 a
type V256 a = C8 V2 a
type V512 a = C9 V2 a
type V1024 a = C10 V2 a


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

instance bindV2 :: Bind V2 where
  bind z f = join' $ map f z where
                          join' (V2 (V2 a _) (V2 _ b)) = V2 a b


toArray :: forall f a. Applicative f => Monoid (f a) => V2 a -> f a
toArray = foldMap pure 

instance representableV2 :: Representable V2 Boolean where
   tabulate f = V2 (f false) (f true)
   index (V2 x y) b = if b then y else x

instance showV2 :: Show a => Show (V2 a) where
   show (V2 x y) = "V2 " <> show x <> " " <> show y

instance semiringV2 :: (Semiring a) => Semiring (V2 a) where   
   add = lift2 add --add (V2 x y) (V2 a b) = V2 (add x a) (add y b)
   zero = pure zero -- V2 zero zero
   mul = lift2 mul --mul (V2 x y) (V2 a b) = V2 (x * a) (y * b)
   one = pure one

instance ringV2 :: (Ring a) => Ring (V2 a) where
   --sub (V2 x y) (V2 a b) = V2 (x-a) (y-b)
   sub = lift2 sub

instance dottableV2 :: (Semiring c, Dottable a b c) => Dottable (V2 a) (V2 b) (Identity c) where
  dot (V2 x y) (V2 a b) = Identity $ (dot x a) + (dot y b) 

