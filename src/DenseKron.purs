module Data.DenseKron where

import Prelude
import Data.Functor.Representable (class Representable, index, tabulate)
import Data.Tuple (Tuple(..), curry)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Dottable (class Dottable, dot)
import Data.HFunctor (class HFunctor)


-- This module is largely identical to Compose, but due to orphan instance restrictions, we 
-- have to fork off our own version

--alternative approach: Use newtype Compose' which directly extends Compose

newtype DKron f g a = DKron (f (g a))

type C0 f a = a -- Identity a? -- DKron Ident
type C1 f a = f a -- DKron Identity f a
type C2 f a = (DKron f f) a
type C3 f a = (DKron f (DKron f f)) a
type C4 f a = (DKron f (DKron f (DKron f f))) a
type C5 f a = (DKron f (DKron f (DKron f (DKron f f)))) a
type C6 f a = (DKron f (DKron f (DKron f (DKron f (DKron f f))))) a
type C7 f a = (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f f)))))) a
type C8 f a = (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f f))))))) a
type C9 f a = (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f f)))))))) a
type C10 f a = (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f f))))))))) a
type C11 f a = (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f (DKron f f)))))))))) a


newtype C2' f a = C2' (C2 f a)

instance hfunctorC2' :: HFunctor C2' where
   hmap f (C2' (DKron x)) = C2' $ DKron $ map f (f x)

instance semiringDKron :: Semiring (f (g a)) => Semiring (DKron f g a) where
   add (DKron x) (DKron y) = DKron $ add x y
   zero = DKron zero
   mul (DKron x) (DKron y) = DKron $ mul x y
   one = DKron one

instance ringDKron :: Ring (f (g a)) => Ring (DKron f g a) where
   sub (DKron x) (DKron y) = DKron $ sub x y

instance divisionRingDKron :: DivisionRing (f (g a)) => DivisionRing (DKron f g a) where
   recip (DKron x) = DKron $ recip x

instance representableKron :: (Representable f a, Representable g b) => Representable (DKron f g) (Tuple a b) where
   tabulate = DKron <<< tabulate <<< map tabulate <<< curry
   index (DKron fg) (Tuple i j) = index (index fg i) j 

instance functorDKron :: (Functor f, Functor g) => Functor (DKron f g) where
  map f (DKron fga) = DKron $ map f <$> fga

instance applyDKron :: (Apply f, Apply g) => Apply (DKron f g) where
  apply (DKron f) (DKron x) = DKron $ apply <$> f <*> x

instance applicativeDKron :: (Applicative f, Applicative g) => Applicative (DKron f g) where
  pure = DKron <<< pure <<< pure

instance dottableDKron :: Dottable (p (p' a)) (g (g' b)) (f (f' c)) => Dottable (DKron p p' a) (DKron g g' b) (DKron f f' c) where
  dot (DKron x) (DKron y) = DKron $ dot x y


instance foldableDKron :: (Foldable f, Foldable g) => Foldable (DKron f g) where
  foldr f i (DKron fga) = foldr (flip (foldr f)) i fga
  foldl f i (DKron fga) = foldl (foldl f) i fga
  foldMap f (DKron fga) = foldMap (foldMap f) fga

instance showDKron :: Show (f (g a)) => Show (DKron f g a) where
  show (DKron fga) = "(DKron " <> show fga <> ")"

