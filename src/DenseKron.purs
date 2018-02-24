module DenseKron where

import Prelude
import Representable
import Data.Tuple
import Semiring1
import Data.Identity
import Data.Foldable
import Dottable
import HFunctor
--import Data.Functor.Compose

-- This module is largely identical to Compose, but due to orphan instance restrictions, we 
-- have to fork off our own version

--alternative approahc. Use Compose' which directly extends Compose
newtype CKron f g a = CKron (f (g a))

type C0 f a = a -- Identity a? -- CKron Ident
type C1 f a = f a -- CKron Identity f a
type C2 f a = (CKron f f) a
type C3 f a = (CKron f (CKron f f)) a
type C4 f a = (CKron f (CKron f (CKron f f))) a
type C5 f a = (CKron f (CKron f (CKron f (CKron f f)))) a
type C6 f a = (CKron f (CKron f (CKron f (CKron f (CKron f f))))) a
type C7 f a = (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f f)))))) a
type C8 f a = (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f f))))))) a
type C9 f a = (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f f)))))))) a
type C10 f a = (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f f))))))))) a
type C11 f a = (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f (CKron f f)))))))))) a


newtype C2' f a = C2' (C2 f a)

instance c2hfunctor :: HFunctor C2' where
   hmap f (C2' (CKron x)) = C2' $ CKron $ map f (f x)

-- is this totally pointless with the next instance?
{-
instance kronSemiring :: (Semiring (g a), Semiring1 f) => Semiring (CKron f g a) where
   add (CKron x) (CKron y) = CKron $ add1 x y
   zero = CKron zero1
   mul (CKron x) (CKron y) = CKron $ mul1 x y
   one = CKron one1
-}
instance kronSemiring' :: Semiring (f (g a)) => Semiring (CKron f g a) where
   add (CKron x) (CKron y) = CKron $ add x y
   zero = CKron zero
   mul (CKron x) (CKron y) = CKron $ mul x y
   one = CKron one

instance kronRing :: Ring (f (g a)) => Ring (CKron f g a) where
   sub (CKron x) (CKron y) = CKron $ sub x y

instance kronDivisionRing :: DivisionRing (f (g a)) => DivisionRing (CKron f g a) where
   recip (CKron x) = CKron $ recip x


-- Similarly for Ring and DivisionRing


-- deOne x = CKron (map Identity x) 

instance kronRep :: (Representable f a, Representable g b) => Representable (CKron f g) (Tuple a b) where
   tabulate = CKron <<< tabulate <<< map tabulate <<< curry
   index (CKron fg) (Tuple i j) = index (index fg i) j 

instance functorCompose :: (Functor f, Functor g) => Functor (CKron f g) where
  map f (CKron fga) = CKron $ map f <$> fga

instance applyCKron :: (Apply f, Apply g) => Apply (CKron f g) where
  apply (CKron f) (CKron x) = CKron $ apply <$> f <*> x

instance applicativeCKron :: (Applicative f, Applicative g) => Applicative (CKron f g) where
  pure = CKron <<< pure <<< pure

-- quesitonable
{-
instance composeSemiring1 :: (Semiring1 g, Applicative f) => Semiring1 (CKron f g) where
  add1 (CKron x) (CKron y) = CKron (add1 <$> x <*> y)
  zero1 = CKron $ pure zero1
  mul1 (CKron x) (CKron y) = CKron (mul1 <$> x <*> y)
  one1 = CKron $ pure one1
-}




{-
instance dot1ma :: (Applicative p, Applicative g, Applicative f, Dottable1 p' g' f') => Dottable1 (CKron p p') (CKron g g') (CKron f f') where
  dot1 (CKron x) (CKron y) = CKron $ dot1 <$> x <*> y
-}

instance dotma :: Dottable (p (p' a)) (g (g' b)) (f (f' c)) => Dottable (CKron p p' a) (CKron g g' b) (CKron f f' c) where
  dot (CKron x) (CKron y) = CKron $ dot x y
  {-
instance dot1ma :: (Dottable1 p g f, Dottable1 p' g' f') => Dottable1 (CKron p p') (CKron g g') (CKron f f') where
  dot1 (CKron x) (CKron y) = CKron $ dot1 x y
-}



instance foldableCompose :: (Foldable f, Foldable g) => Foldable (CKron f g) where
  foldr f i (CKron fga) = foldr (flip (foldr f)) i fga
  foldl f i (CKron fga) = foldl (foldl f) i fga
  foldMap f (CKron fga) = foldMap (foldMap f) fga

instance showCompose :: Show (f (g a)) => Show (CKron f g a) where
  show (CKron fga) = "(CKron " <> show fga <> ")"
{-
instance foldableCKron :: Foldable f => Foldable (CKron V2 f) where
   foldMap f (CKron (V2 x y)) = (foldMap f x) <> (foldMap f y)
   foldl f b (V2 x y) = f (f b x) y
   foldr f b (V2 x y) = f x (f y b)
-}


{-
we are not allowed
Semiring1 f, Semiring a => Semiring (f a)
because purescript is a jerk

instance oneImpliesZero :: (Semiring1 f, Semiring a) => Semiring (f a) where
  add = add1
  zero = zero1
  mul = mul1
  one = one1

	-- applicative?

instance composeSemiring1 :: (Semiring1 g, Applicative f) => Semiring1 (CKron f g) where
  add1 (Compose x) (Compose y) = Compose (add1 <$> x <*> y)
  zero1 = Compose $ pure zero1
  mul1 (Compose x) (Compose y) = Compose (mul1 <$> x <*> y)
  one1 = Compose $ pure one1
   -}