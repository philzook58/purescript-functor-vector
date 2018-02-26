module Data.H2 where

import Prelude
import Data.BinVec
import Data.Functor.Representable
import Data.Dottable
import Data.BinMat
import Data.DenseKron
import Data.Traversable

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






