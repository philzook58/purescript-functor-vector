module Dualizable where

--THis should go in Data.Dual
-- Dottable already gives us a notion of dual.
import Prelude
import Data.BinMat
import Data.Dual

--transposable?
class Dualizable a d | a -> d where
   dual :: a -> d

instance dualizableM2 :: (Dualizable a d) => Dualizable (M2 a) (M2 d) where
   dual (M2 a b c d) = M2 (dual a) (dual c) (dual b) (dual d)


