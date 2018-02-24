module Data.BinMat where

import Prelude
import Data.Functor.Representable (class Representable)
import Data.BinVec (V2(..))
import Data.Dottable (class Dottable, dot)
import Data.Tuple (Tuple(..), curry)
import Data.DenseKron

data M2 a = M2 a a a a

type M4 a = C2 M2 a
type M8 a = C3 M2 a
type M16 a = C4 M2 a
type M32 a = C5 M2 a
type M64 a = C6 M2 a
type M128 a = C7 M2 a
type M256 a = C8 M2 a
type M512 a = C9 M2 a
type M1024 a = C10 M2 a

instance functorM2 :: Functor M2 where
   map f (M2 x y z w) = M2 (f x) (f y) (f z) (f w)
{-
instance foldableM2 :: Foldable M2 where
   foldMap f (M2 a b c d) = (f a) <> (f b) <> (f c) <> (f d)
   foldl = foldlDefault
   foldr = foldrDefault
-}
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

--https://en.wikipedia.org/wiki/Schur_complement
instance divisionRingM2 :: (DivisionRing a) => DivisionRing (M2 a) where
   recip (M2 a b c d) = M2 m                      (zero - m * b * dinv)
                           (zero - dinv * c * m)  (dinv * c * m * b * dinv + dinv) where
                                                                            m = recip (a - b * dinv * c)
                                                                            dinv = recip d
{-
instance dottableM2V2 :: (Semiring a, Semiring m,  Dottable m a a) => Dottable (M2 m) (V2 a) (V2 a) where
  dot (M2 a b c d) (V2 x y) = V2 ((dot a x) + (dot b y)) ((dot c x) + (dot d y))
-}

instance dottableM2V2 :: (Semiring c, Dottable a b c) => Dottable (M2 a) (V2 b) (V2 c) where
  dot (M2 a b c d) (V2 x y) = V2 ((dot a x) + (dot b y)) ((dot c x) + (dot d y))

instance dottableM2M2 :: (Semiring c, Dottable a b c) => Dottable (M2 a) (M2 b) (M2 c) where
  dot (M2 x y z w) (M2 a b c d) = M2 ((dot x a) + (dot y c)) ((dot x b) + (dot y d)) ((dot z a) + (dot w c)) ((dot z b) + (dot w d))





