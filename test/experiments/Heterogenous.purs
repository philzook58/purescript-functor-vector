module HetMat where

import Prelude
import Data.Dottable
import Data.Additive

data HetMat a b c d = HetMat a b c d

instance dottableHetMat :: (Semiring a, Semiring d, Additive b, Additive c, Dottable a b b, Dottable b d b, Dottable c a c, Dottable d c c, Dottable b c a, Dottable c b d) => Semiring (HetMat a b c d) where 
   add (HetMat a b c d) (HetMat e f g h) = HetMat (a + e) (add' b f) (add' c g) (d + h)
   zero = HetMat zero zero' zero' zero
   mul (HetMat a b c d) (HetMat e f g h) = HetMat (a * e + (dot b g)) (add' (dot a f) (dot b h)) (add' (dot c e) (dot d g)) ((dot c f) + (d * h))
   one = HetMat one zero' zero' one
-- instance Dottable => Dottable (HetMat a b c d) (HetMat e f g h) (HetMat i j k l)

data HStack a b = HStack a b
data VStack a b = VStack a b

instance stackDot:: (Dottable a c e, Dottable b d e, Semiring e) => Dottable (HStack a b) (VStack c d) e where
   dot (HStack a b) (VStack c d) = (dot a c) + (dot b d) 

instance stackDot2 :: (Dottable a c e, Dottable a d f, Dottable b c g, Dottable b d h) => Dottable (VStack a b) (HStack c d) (HetMat e f g h) where
   dot (VStack a b) (HStack c d) = HetMat (dot a c) (dot a d) (dot b c) (dot b d) 