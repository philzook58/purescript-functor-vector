module Representable where

import Prelude
import Data.Functor.Compose
--import Data.Functor.Product
import Data.Tuple

class Functor f <= Representable f a | f -> a where
   tabulate :: forall b. (a -> b) -> f b
   index :: forall b. f b -> (a -> b)


instance arrowRep :: Representable ((->) a) a where
   tabulate = id
   index = id

{-
instance arrowRep :: Representable f a => Representable ((->) (f b)) (Tuple a b) where
   tabulate = id
   index = id
-}
instance repCompose :: (Representable f a, Representable g b) => Representable (Compose f g) (Tuple a b) where
   tabulate = Compose <<< tabulate <<< map tabulate <<< curry
   index (Compose fg) (Tuple i j) = index (index fg i) j