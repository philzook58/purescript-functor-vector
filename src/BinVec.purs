module BinVec where

import Prelude
import Representable
import Data.Functor.Compose

data V2 a = V2 a a

-- Produced via
-- foldMap (\n -> "type V" <> show (2*n) <> " a = (Compose V2 V" <> show n <> ") a ") $ map (\n -> pow 2 n) (1 .. 10)
--foldMap (\n -> "type V" <> show (2*n) <> " a = V2 (V" <> show n <> ") a ") $ map (\n -> pow 2 n) (1 .. 10)

-- two problems:
-- 1. Partially applied V4 is unnacceptable
-- 2. Compose
--solution A:
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

-- do everything in compose-less form
-- solution B:
--newtype V4 a = V8 (Compose V2 V2) a
--derive instance v8Newtype :: Newtype V4 _
-- include newtype unwrapping in all pertinent typeclasses
-- except semiring. which we can't get at. God damn is that annoying.
-- do everything in terms of semiring1?
-- do everything without composition. fully expanded.
{-
type V4 a = (Compose V2 V2) a
type V8 a = (Compose V2 V4) a 
type V16 a = (Compose V2 V8) a 
type V32 a = (Compose V2 V16) a 
type V64 a = (Compose V2 V32) a 
type V128 a = (Compose V2 V64) a 
type V256 a = (Compose V2 V128) a 
type V512 a = (Compose V2 V256) a 
type V1024 a = (Compose V2 V512) a 
type V2048 a = (Compose V2 V1024) a 
-}

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
