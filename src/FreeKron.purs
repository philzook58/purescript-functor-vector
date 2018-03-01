module Data.FreeKron where

import Prelude 
import Data.CatList (CatList)
import Data.DirectSum (DSum(..))
import Data.Monoid (mempty)
import Data.Dottable (class Dottable, dot)

newtype FKron f g a = FKron (CatList (DSum f g a))

instance semiringFKron :: (Semiring (f a), Semiring (g a)) => Semiring (FKron f g a) where
   add (FKron x) (FKron y) = FKron $ x <> y
   zero = FKron $ mempty
   mul (FKron x) (FKron y) = FKron $ mul <$> x <*> y  -- ? Can I do this. Or should be matrix multplication? This is remarkablt symmettric with resepct to c (The intended scalar).
   one = FKron $ pure one

-- No instance for Division 

instance dottableFKron :: (Dottable (f a) (f' b) (f'' c), Dottable (g a) (g' b) (g'' c)) => Dottable (FKron f g a) (FKron f' g' b) (FKron f'' g'' c) where
   dot (FKron x) (FKron y) = FKron $ dot <$> x <*> y  


fkron x y = FKron $ pure $ DSum x y

-- densify :: FKron f g a -> DKron f g a


