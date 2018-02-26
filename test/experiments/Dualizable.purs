module Dualizable where

--THis should go in Data.Dual
-- Dottable already gives us a notion of dual.
import Prelude
import Data.BinMat
import Data.Dual
import Data.H2
import Data.BinVec

--transposable?
class Dualizable a d | a -> d where
   dual :: a -> d

instance dualizableM2 :: (Dualizable a d) => Dualizable (M2 a) (M2 d) where
   dual (M2 a b c d) = M2 (dual a) (dual c) (dual b) (dual d)


instance dualizableH2 :: (Dualizable a d) => Dualizable (H2 a) (V2 d) where
   dual (H2 (V2 x y)) = V2 (dual x) (dual y)

instance dualizableV2 :: (Dualizable a d) => Dualizable (V2 a) (H2 d) where
   dual (V2 x y) = H2 $ V2 (dual x) (dual y)
