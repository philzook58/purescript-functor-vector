module Data.H2 where

import Prelude
import Data.BinVec
import Data.Functor.Representable
import Data.Dottable
import Data.BinMat
import Data.DenseKron
import Data.Traversable
import Data.Tuple
import Data.Enum

newtype H2 a = H2 (V2 a)

type H4 a = C2 H2 a
type H8 a = C3 H2 a
type H16 a = C4 H2 a
type H32 a = C5 H2 a
type H64 a = C6 H2 a
type H128 a = C7 H2 a
type H256 a = C8 H2 a
type H512 a = C9 H2 a
type H1024 a = C10 H2 a

derive newtype instance semiringH2 :: Semiring a => Semiring (H2 a)
derive newtype instance ringH2 :: Ring a => Ring (H2 a)
derive newtype instance showH2 :: Show a => Show (H2 a)
derive newtype instance functorH2 :: Functor H2
derive newtype instance applicativeH2 :: Applicative H2
derive newtype instance traversableH2 :: Traversable H2
derive newtype instance applyH2 :: Apply H2
derive newtype instance bindH2 :: Bind H2

instance representableH2 :: Representable H2 Boolean where
   index (H2 x) = index x
   tabulate f = H2 $ tabulate f

instance dottableH2V2 :: (Semiring c, Dottable a b c) => Dottable (H2 a) (V2 b) c where
  dot (H2 (V2 x y)) (V2 a b) = (dot x a) + (dot y b) 

instance dottableV2H2 :: (Semiring c, Dottable a b c) => Dottable (V2 a) (H2 b) (M2 c) where
  dot (V2 a b) (H2 (V2 x y)) = M2 (dot a x) (dot a y) (dot b x) (dot b y)


isoM2 :: forall a. H2 (V2 a) -> M2 a
isoM2 (H2 (V2  (V2 a c) (V2 b d)    )) = M2 a b c d
isoM2' :: forall a. M2 a -> H2 (V2 a)
isoM2' (M2 a b c d) = H2 $ V2 (V2 a c) (V2 b d)

isoM2'' :: forall a. V2 (H2 a) -> M2 a
isoM2'' (V2 (H2 (V2 a b)) (H2 (V2 c d)) ) =  M2 a b c d
isoM2''' :: forall a.  M2 a -> V2 (H2 a)
isoM2''' (M2 a b c d) =  (V2 (H2 (V2 a b)) (H2 (V2 c d)) ) 


-- Consider using a Raw2
-- and then newtype wrappers for V2 H2.
-- Also M2 = H2 V2 and M2T = V2 H2

-- Intersepcred is Z-ordering
-- Column major vs Row major is all H to the left or right
-- Best I can figure is that 
example1 = zero :: H4 (V4 Number)
example2 = zero :: V4 (H4 Number)


colmajor :: Int -> Int -> Int -> Int -> Int
colmajor v h r c = r + v * c 

uncolmajor :: Int  -> Int -> Int -> (Tuple Int Int)
uncolmajor v h i = Tuple (mod i v) (div i v) 


rowmajor :: Int -> Int -> Int -> Int -> Int
rowmajor v h r c = c + h * r 

unrowmajor :: Int  -> Int -> Int -> (Tuple Int Int)
unrowmajor v h i = Tuple (mod i h) (div i h) 

{-
	-- Ugh. This is completely unsatisfactory
coltorow :: forall a f g. Representable f a => Representable g a => Int -> f a -> g a
coltorow vh x = fillFromRowMajorIndex vh (index x <<< unsafePartial <<< fromJust <<< toEnum)

fillFromRowMajorIndex :: forall a f b. BoundedEnum a => Representable f a => Int -> (Int -> Int -> b) -> f b  
fillFromRowMajorIndex vh f = tabulate (uncurry f <<< unrowmajor vh vh <<< fromEnum)

fillFromColMajorIndex :: forall a f b. BoundedEnum a => Representable f a => Int -> (Int -> Int -> b) -> f b  
fillFromColMajorIndex vh f = tabulate (uncurry f <<< uncolmajor vh vh <<< fromEnum)

If we encode as
DKron (C4 f) (C4 g) -> 
then  
tabulate (index x <<< swap) 
will do it.

-- Or. If we declare Traversable instances for (C4 f)
-- But I have a suspicion that the representable method with be more effiicent. It's basically grabbing the value right form where it needs to.

-}
rowcolorderingswap :: forall f g a b c. Representable f a => Representable g b => DKron f g c -> DKron g f c
rowcolorderingswap x = tabulate (index x <<< swap) 





