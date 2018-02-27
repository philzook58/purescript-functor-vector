module Data.Pretty where

import Prelude
import Text.Pretty
import Data.BinMat
import Data.BinVec
import Data.DenseKron
import Data.Complex
import Data.Dottable

-- PShow using functor composition as horizontal
-- and matrix multplication as vertical.

space = empty 1 0

class PShow a where
  pshow :: a -> Doc

instance numberPShow :: PShow Number where
  pshow = text <<< show

instance intPShow :: PShow Int where
  pshow = text <<< show

instance unitPShow :: PShow Unit where
  pshow _ = text ""

instance pshowV2 :: PShow a => PShow (V2 a) where
  pshow (V2 a b) = hcat [pshow a, space, pshow b]

instance pshowM2 :: PShow a => PShow (M2 a) where
  pshow (M2 a b c d) = atop (hcat [pshow a, space, pshow b]) (hcat [pshow c, space, pshow d])

instance pshowDKron :: PShow (f (g a))=> PShow (DKron f g a) where
  pshow (DKron x) = pshow x

instance pshowComplex :: PShow a => PShow (Complex a) where
  pshow (Complex x y) =  hcat [pshow x, text "+", pshow y, text "i"]


data Diagram a = Diagram Doc a
data DiagramOp a = DiagramOp Doc a

arrowdiagram = vcat [text " | ", 
					 text "/|\\", 
					 text " | " ]

boxdiagram p = vcat [      text " | ",
					 text ("[" <> p <> "]"),
					       text " | "
                     ]

v2diagram :: forall a. V2 a -> Diagram a
v2diagram (V2 x _) = Diagram arrowdiagram x

-- use HFunctor instance of C4

instance pshowDiagram :: PShow a => PShow (Diagram a) where
   pshow (Diagram x a) = beside x (pshow a)

instance functorDiagram :: Functor Diagram where
   map f (Diagram x a) = Diagram x (f a)

instance pshowDiagramOp :: PShow a => PShow (DiagramOp a) where
   pshow (DiagramOp x a) = beside x (pshow a)

instance dottableDiagram :: Dottable a b c => Dottable (DiagramOp a) (DiagramOp b) (DiagramOp c) where
   dot (DiagramOp x a) (DiagramOp y b) = DiagramOp (atop x y) (dot a b)

instance dottableDiagramOp :: Dottable a b c => Dottable (DiagramOp a) (Diagram b) (Diagram c) where
   dot (DiagramOp x a) (Diagram y b) = Diagram (atop x y) (dot a b)



