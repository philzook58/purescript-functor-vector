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
-- instance Dottable => Dottable (HetMat a b c d) (HetMat e f g h) (HetMat i j k l) -- yikes. this is gonna be not great to fill out
-- actually it is the saem as the above, exceppt a b c d go into the dottable in order and the semiring is only on the last.
-- That's not so bad.

data HStack a b = HStack a b
data VStack a b = VStack a b

type H2' a = HStack a a
type V2' a = VStack a a
type HetMat' a b c d = VStack (HStack a b) (HStack c d)
type HetMat'' a b c d = HStack (VStack a c) (VStack b d)

instance stackDot:: (Dottable a c e, Dottable b d e, Semiring e) => Dottable (HStack a b) (VStack c d) e where
   dot (HStack a b) (VStack c d) = (dot a c) + (dot b d) 

instance stackDot2 :: (Dottable a c e, Dottable a d f, Dottable b c g, Dottable b d h) => Dottable (VStack a b) (HStack c d) (HetMat e f g h) where
   dot (VStack a b) (HStack c d) = HetMat (dot a c) (dot a d) (dot b c) (dot b d) 


-- is stacking taken care of by
-- DSum on the output and input?\
-- instance Dottable m (DSum a b) (DSum c d)
-- instance  SemiRing e z , Semiring f z, Dottable a e e, Dottable b f e, Dottable c e f, Dottable d f f => (HetMat a b c d) (DSum e f z) (DSum e f z) where
--    dot (HetMat a b c d) (DSum x y) = DSum $ (dot a x + dot b y) (dot c x + dot d y)

-- does this imply an instance for 
-- instance Dottable (HetMat a b c d) z z => Dottable (HetMat a b c d)  (HetMat a b c d)  (HetMat a b c d) where
-- dot (HetMat a b c d) (HetMat x y z w) = 
-- nah, I don't think it works. Not without an UnDottable  
-- what I have above is good.

-- could call HetMat  = BlockMat. it is deserving of this name

