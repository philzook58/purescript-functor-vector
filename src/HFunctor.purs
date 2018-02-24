module Data.HFunctor where

import Prelude
import Data.Functor.Compose (Compose(..))

-- useful for changing out the functor. Augmenting it with monads, FreeSemiring etc.
class HFunctor p where
  hmap :: forall f g a. Functor f => Functor g => (f ~> g) -> (p f a) -> (p g a) 

instance hfunctorcompsoe :: Functor f => HFunctor (Compose f) where
  hmap f (Compose x) = Compose $ map f x

{- Not sure why this doesn't compile
newtype Zerosy f a = Zerosy a

instance serozyHfunctor :: HFunctor Zerosy where
  hmap _ (Zerosy x) = Zerosy $ x
-}

newtype Onesy f a = Onesy (f a)

instance hfunctortonesy :: HFunctor Onesy where
  hmap f (Onesy x) = Onesy $ (f x)

newtype Twosy f a = Twosy (f (f a))

instance hfunctortwosy :: HFunctor Twosy where
  hmap f (Twosy x) = Twosy $ map f (f x)

newtype Threesy f a = Threesy (f (f (f a)))

instance hfunctorthreesy :: HFunctor Threesy where
  hmap f (Threesy x) = Threesy $ (map <<< map) f (map f (f x))