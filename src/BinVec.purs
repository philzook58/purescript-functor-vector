module BinVec where

import Prelude
import Representable
import Data.Functor.Compose

data V2 a = V2 a a

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

instance functorV2 :: Functor V2 where
   map f (V2 x y) = V2 (f x) (f y)

instance v2Rep :: Representable V2 Boolean where
   tabulate f = V2 (f false) (f true)
   index (V2 x y) b = if b then y else x

instance showV2 :: Show a => Show (V2 a) where
   show (V2 x y) = "V2 " <> show x <> " " <> show y

instance v2Semiring :: (Semiring a) => Semiring (V2 a) where
   add (V2 x y) (V2 a b) = V2 (add x a) (add y b)
   zero = V2 zero zero
   mul (V2 x y) (V2 a b) = V2 (x * a) (y * b)
   one = V2 one one

instance ringV2 :: (Ring a) => Ring (V2 a) where
   sub (V2 x y) (V2 a b) = V2 (x-a) (y-b)
