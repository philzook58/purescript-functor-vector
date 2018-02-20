module FreeKron where
import Prelude 
import Data.CatList
import DirectSum
import Semiring1 
import Data.Monoid (mempty)

newtype FKron f g a = FKron (CatList (DSum f g a))

instance semiring1Kron :: (Semiring1 f, Semiring1 g) => Semiring1 (FKron f g) where
   add1 (FKron x) (FKron y) = FKron $ x <> y
   zero1 = FKron $ mempty
   mul1 (FKron x) (FKron y) = FKron $ mul1 <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one1 = FKron $ pure one1

instance semiringKron :: (Semiring (f a), Semiring (g a)) => Semiring (FKron f g a) where
   add (FKron x) (FKron y) = FKron $ x <> y
   zero = FKron $ mempty
   mul (FKron x) (FKron y) = FKron $ mul <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one = FKron $ pure one

