module Data.LinOp where

import Prelude
import Data.Dottable

newtype LinOp g f a = LinOp (g a -> f a)
-- newtype LinOp a b = LinOp (a -> b)


instance semiringLinOp :: Semiring (f a) => Semiring (LinOp f f a) where
   add (LinOp f) (LinOp g) = LinOp \x -> (f x) + (g x)
   zero = LinOp (const zero)
   mul (LinOp f) (LinOp g) = LinOp $ (f <<< g)
   one = LinOp id

--InvFunctor
--InvRepresentable
-- Dual f a ~ LinOp f Identity a

instance dottableLinOpFunctor :: Dottable (LinOp g f a) (g a) (f a) where
  dot (LinOp f) x = f x
{- I think this is covered by the combo of the next two and the previous
instance dottableLinOp :: Dottable (LinOp g f a) (LinOp f h a) (LinOp g h a) where
  dot (LinOp f) (LinOp g) = LinOp (f <<< g)
-}
instance dottableLinOpRight :: Dottable (r a) (f a) (h a) => Dottable (LinOp g f a) (r a) (LinOp g h a) where
  dot (LinOp f) x = LinOp ((\y -> dot x y) <<< f)

instance dottableLinOpLeft :: Dottable (r a) (j a) (g a) => Dottable (r a) (LinOp g f a) (LinOp j f a) where
  dot x (LinOp f) = LinOp (f <<< (\y -> dot x y))

{-
newtype LinOp' g f a b = LinOp' (g a -> f b)
newtype LinOp'' g f a  = LinOp'' (LinOp' g f a a)


instance dottableLinOpLong :: Dottable (LinOp' g f a b) (LinOp' f h b c) (LinOp' g h a c) where
  dot (LinOp f) (LinOp g) = LinOp (f <<< g)
  -}
{-
instance dottableLinOp' :: Dottable b c d => Dottable (LinOp' g f a b) (LinOp' f h c d) (LinOp' g h a c) where
  dot (LinOp f) (LinOp g) = LinOp (f <<< g)
  -}