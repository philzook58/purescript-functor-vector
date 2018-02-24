module Data.Dottable where

import Prelude
import Data.Exists
import Data.Functor.Representable (basis)
import Data.Foldable (sum)
import Data.Enum
import Control.Apply (lift2)

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

instance dottableArrowApply :: Dottable (b -> c) b c where
  dot f x = f x

instance dottableArrowArrow :: (BoundedEnum b, Semiring c) => Dottable (b -> c) (b -> c) c where
  dot f g = sum $ lift2 mul (map f basis) (map g basis)

