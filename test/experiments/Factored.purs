module Factored where

data Factored a b = Factored a b



{-
instance semiringFactored :: (Dottable a b c, Dottable b a d, Dottable d a d, Dottable d a d, Dottable d a d, Dottable d a d ) => SemiRing (Factored a b)
-}


-- this is all rather silly. basically you might as well convert to the function format, since that is all Dottable gives you
{-
newtype DotCatF b c a = DotCatF (Dottable a b c => a)

instance dotcatfdottable :: Dottable (DotCatF a b c) b c where
  dot (DotCatF x) (DotCatF y) = DotCatF $ dot x y

newtype DotCat b c = DotCat (Exists (DotCatF b c))

instance dotCat :: Semigroupoid DotCat where
  compose (DotCat x) (DotCat y) = DotCat $  mkExists $ runExists (\x' -> runExists (\y' -> DotCatF (\b -> dot y' (dot x' b)) ) y)  x

-}

{-
 newtype Expansion f a = Expansion (FKron f (Dual f) a)
 
instance expansionsemiring :: (Semiring a, Semiring (f a)) => Semiring (Expansion f a) where
  add (Expansion x) (Expansion y) = Expansion (x + y)
  zero = Expansion $ zero 
  mul (Expansion (FKron x)) (Expansion (FKron y)) = Expansion $ FKron $ helper <$> x <*> y where
  											helper (DSum a b) (DSum c d) = DSum a (map (\x -> unwrap $ x * (dot b c)) d)
  one = FKron $ pure one -- Not correct
  -} 