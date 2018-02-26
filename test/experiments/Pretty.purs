module Data.Pretty where

import Prelude
import Text.Pretty
import Data.BinMat
import Data.BinVec
import Data.DenseKron
import Data.Complex

-- PShow using functor composition as horizontal
-- and matrix multplication as vertical.

space = empty 1 0

class PShow a where
  pshow :: a -> Doc

instance numberPShow :: PShow Number where
  pshow = text <<< show

instance intPShow :: PShow Int where
  pshow = text <<< show

instance pshowV2 :: PShow a => PShow (V2 a) where
  pshow (V2 a b) = hcat [pshow a, space, pshow b]

instance pshowM2 :: PShow a => PShow (M2 a) where
  pshow (M2 a b c d) = atop (hcat [pshow a, space, pshow b]) (hcat [pshow c, space, pshow d])

instance pshowDKron :: PShow (f (g a))=> PShow (DKron f g a) where
  pshow (DKron x) = pshow x

instance pshowComplex :: PShow a => PShow (Complex a) where
  pshow (Complex x y) =  hcat [pshow x, text "+", pshow y, text "i"]
