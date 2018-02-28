module Laplace where

import Prelude
import Data.BinMat
import Data.Functor.Representable
import Data.DivisionRing
import Data.Complex
import Data.DenseKron
import Data.BinVec
import Data.Pretty
--import Data.Proxy

-- little endian K matrix. Maybe
mK :: forall a. Ring a => M2 a    -- forall f. Functor f => Semiring (f Number) => M2 (f Number)
mK = M2 ntwo one one ntwo where
             				ntwo = zero - one - one -- map (_ * -2.0) one

mK'' :: Int -> Int -> Number
mK'' i j | i == j - 1 = -1.0
mK'' i j | i == j + 1 = -1.0
mK'' i j | i == j = 2.0
mK'' _ _ = 0.0

mK' :: M8 Number
mK' = fillFromZIndex mK''

fD' :: Int -> Int -> Number
fD' i j | i == j = -1.0
fD' i j | i == j + 1 = 1.0
fD' _ _ = 0.0

{-
fD :: Proxy2 f -> f Number
fD _ = fillFromZIndex fD'
-}
-- forward difference
fD :: M4 Number
fD = fillFromZIndex fD'
-- integration
fI = recip fD

bD' :: Int -> Int -> Number
bD' i j | i == j = 1.0
bD' i j | i == j - 1 = -1.0
bD' _ _ = 0.0

--bD = fillFromZIndex bD'

bD :: M4 Number
bD = fillFromZIndex bD'

-- 2d laplace matrix on sqaure grid
mK2 = dkron mK' mK'


middleone :: V8 Number
middleone = fillFromIndex f where f i = if i == 4 then 1.0 else 0.0

--pointcharge :: DKron (C3 V2) (C3 V2) Number
-- we need to put newtypes around Vs.

pointcharge = dkron middleone middleone



--iterated filling

-- this method retraverses the tree
--testD3 = (map <<< map) fillFromIndex $ map fillFromIndex $ (fillFromIndex f) :: V8 Number where f i j k = 0.0
-- this is more CPS-like
testD3 ::  DKron V2 (DKron V2 V2) Number
testD3 = DKron $ fillFromIndex \i -> DKron $ fillFromIndex \j -> fillFromIndex \k -> f i j k where f i j k = 0.0

-- may want to demarcate DKron as different
-- with a tag
newtype Dim a = Dim a

testMD2 ::  DKron M2 M2 Number
testMD2 = DKron $ fillFromZIndex \i j -> fillFromZIndex \k l -> f i j k l where f i j k l = 0.0

-- Class Dualizable a b where
-- instance Dualiazble a b => Dualizable (Complex a) (Complex b)
-- dual (Complex x y) =  Complex (dual x) (negate (dual y))
-- instance  Dualizable Number Number where
-- dual = id
sigmax :: M2 (Complex Number)
sigmax = M2 zero one one zero

sigmaz :: M2 (Complex Number)
sigmaz = M2 one zero zero neg1

sigmay :: M2 (Complex Number)
sigmay = M2 zero (negate i) i zero

-- can put complex on the inside or outside of the stack.


