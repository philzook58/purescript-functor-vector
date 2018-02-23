module Dottable where

import Prelude
import Data.Functor.Compose
import Semiring1
import Data.Newtype

class Dottable p g f | p g -> f where
  dot :: p -> g -> f

instance numDot :: Dottable Number Number Number where
  dot = mul
instance intDot :: Dottable Int Int Int where
  dot = mul

instance newtypeDot :: (Newtype p a, Newtype g b, Newtype f c, Dottable a b c) => Dottable p g f where
  dot x y = wrap $ dot x' y' where
								x' :: a
								x' = (unwrap x)
								y' :: b
								y' = (unwrap y)




{-
instance dottableSemiring :: Semiring a => Dottable a a a where
  dot = mul
-}
{-
class Dottable1 p g f | p g -> f where
  --dot1 :: forall a. Semiring a => p a -> (g a -> f a)
  dot1 :: forall a b c. Dottable a b c => p a -> (g b -> f c)

instance compDot :: (Dottable1 p g f, Dottable p' g' f') => Dottable (p p') (g g') (f f') where
  dot = dot1
  -}

  -- dot1 :: forall a b c. Additive c => Dottable a b c => p a -> (g b -> f c)
{-

-- I might really need an additive class. Basically Monoid I gueuss.
-- get rid of semiring instance of vector
-- and dottable is an extended notion of multiplication

	-- Not okay unforturnay because of Semiring instance of Vector.
instance dottableSemiring1 :: Semiring1 a => Dottable1 a a a where
  dot1 = mul1
-}
{-
mindex = dot1

class Dottable1 p g f <= Metric p g f | p g -> f where
  mtabulate :: forall a. Semiring a => (g a -> f a) -> p a
-}

--instance Dottable1 p g f, Dottable p' g' f' => Dottable 

--can encode all arrow into p.
{-
class Metric0 p g f where
  mtabulate0 :: (g -> f) -> p
  -}