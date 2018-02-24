module Data.Dottable where

import Prelude
import Data.Exists

class Dottable p g f | p g -> f where
  dot :: p -> g -> f

instance dotttableNumber :: Dottable Number Number Number where
  dot = mul
instance dottableInt :: Dottable Int Int Int where
  dot = mul

{- This may preclude custom instances
instance dottableSemiring :: Semiring a => Dottable a a a where
  dot = mul
-}

{- this may preclude custom dot instances
instance newtypeDot :: (Newtype p a, Newtype g b, Newtype f c, Dottable a b c) => Dottable p g f where
  dot x y = wrap $ dot x' y' where
								x' :: a
								x' = (unwrap x)
								y' :: b
								y' = (unwrap y)
-}

instance dottableArrow :: Dottable (b -> c) b c where
  dot f x = f x

