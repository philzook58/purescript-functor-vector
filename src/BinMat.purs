module BinMat where

import Prelude
import Representable
import BinVec
import Data.Functor.Compose
import Dottable
import Data.Tuple


-- foldMap (\n -> "type M" <> show (2*n) <> " a = (Compose M2 M" <> show n <> ") a ") $ map (\n -> pow 2 n) (1 .. 10)
data M2 a = M2 a a a a

type M4 a = M2 (M2 a)
{-
type M4 a = (Compose M2 M2) a 
type M8 a = (Compose M2 M4) a 
type M16 a = (Compose M2 M8) a 
type M32 a = (Compose M2 M16) a 
type M64 a = (Compose M2 M32) a 
type M128 a = (Compose M2 M64) a 
type M256 a = (Compose M2 M128) a 
type M512 a = (Compose M2 M256) a 
type M1024 a = (Compose M2 M512) a 
type M2048 a = (Compose M2 M1024) a
-}

type M2' a = M2 a
type M4' a = M2 (M2' a)
type M8' a = M2 (M4' a)

instance functorM2 :: Functor M2 where
   map f (M2 x y z w) = M2 (f x) (f y) (f z) (f w)

instance representableM2 :: Representable M2 (Tuple Boolean Boolean) where
   tabulate f = M2 (f' false false) (f' false true) (f' true false) (f' true true) where f' = curry f
   index (M2 x y z w) (Tuple row col) = if row then
                                                   if col then w else z
                                               else
                                                   if col then y else x

instance semiringM2 :: (Semiring a) => Semiring (M2 a) where
   add (M2 x y z w) (M2 a b c d) = M2 (x+a) (y+b) (z+c) (w+d)
   zero = M2 zero zero zero zero
   mul (M2 x y z w) (M2 a b c d) = M2 (x*a+y*c) (x*b+y*d) (z*a+w*c) (z*b+w*d)  -- Could use strassen
   one = M2 one zero zero one

instance showM2 :: Show a => Show (M2 a) where
   show (M2 a b c d) = "M2 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d

instance ringM2 :: (Ring a) => Ring (M2 a) where
   sub (M2 x y z w) (M2 a b c d) = M2 (x-a) (y-b) (z-c) (w-d)

instance divisionRingM2 :: (DivisionRing a) => DivisionRing (M2 a) where
   recip (M2 a b c d) = M2 m               (zero - m * b * dinv)
                           (zero - dinv * c * m)  (dinv * c * m * b * dinv + dinv) where
                                                                            m = recip (a - b * dinv * c)
                                                                            dinv = recip d



instance dottableM2V2 :: Dottable M2 V2 V2 where
  dot (M2 a b c d) (V2 x y) = V2 (a*x + b*y) (c*x + d*y)

instance dot0ma :: (Semiring a , Dottable0 m a a) => Dottable0 (M2 m) (V2 a) (V2 a) where
  dot0 (M2 a b c d) (V2 x y) = V2 ((dot0 a x) + (dot0 b y)) ((dot0 c x) + (dot0 d y))
