module FreeKron where
import Prelude 
import Data.CatList
import DirectSum
import Semiring1 
import Data.Monoid (mempty)
import Dual
import Data.Newtype
import Dottable

newtype FKron f g a = FKron (CatList (DSum f g a))
{-
instance semiring1Kron :: (Semiring1 f, Semiring1 g) => Semiring1 (FKron f g) where
   add1 (FKron x) (FKron y) = FKron $ x <> y
   zero1 = FKron $ mempty
   mul1 (FKron x) (FKron y) = FKron $ mul1 <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one1 = FKron $ pure one1
-}
instance semiringKron :: (Semiring (f a), Semiring (g a)) => Semiring (FKron f g a) where
   add (FKron x) (FKron y) = FKron $ x <> y
   zero = FKron $ mempty
   mul (FKron x) (FKron y) = FKron $ mul <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one = FKron $ pure one

-- No instance for Division 
-- maybe an instance for 

newtype Expansion f a = Expansion (FKron f (Dual f) a)


{-

instance expansionsemiring :: (Semiring a, Semiring (f a)) => Semiring (Expansion f a) where
  add (Expansion x) (Expansion y) = Expansion (x + y)
  zero = Expansion $ zero 
  mul (Expansion (FKron x)) (Expansion (FKron y)) = Expansion $ FKron $ helper <$> x <*> y where
  											helper (DSum a b) (DSum c d) = DSum a (map (\x -> unwrap $ x * (dot b c)) d)
  one = FKron $ pure one -- Not correct
  -} 