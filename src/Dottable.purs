module Dottable where

import Prelude
import Data.Functor.Compose

class Dottable p g f | p g -> f where
  dot :: forall a. Semiring a => p a -> (g a -> f a)

class Dottable0 p g f | p g -> f where
  dot0 :: p -> g -> f

instance dottableSemiring :: Semiring a => Dottable0 a a a where
  dot0 = mul

mindex = dot

class Dottable p g f <= Metric p g f | p g -> f where
  mtabulate :: forall a. Semiring a => (g a -> f a) -> p a
  
  {-
instance Dottable0 a b c, Dottable p g f => Dottable (p a) (g b) (f c) where
  dot0 =
  -} 
-- p (p' a)
{-
instance dotCompose :: (Dottable p g f, Dottable p' g' f') => Dottable (Compose p p') (Compose g g') (Compose f f') where
  dot (Compose ppa) (Compose gga) = Compose (dot ppa gga)
-}