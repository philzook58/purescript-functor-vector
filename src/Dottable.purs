module Dottable where

import Prelude
import Data.Functor.Compose


class Dottable p g f | p g -> f where
  dot :: p -> g -> f

instance dottableSemiring :: Semiring a => Dottable a a a where
  dot = mul
