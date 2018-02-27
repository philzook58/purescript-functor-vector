module QR where

import Data.Tuple
import Data.BinVec
import Data.H2
import Data.CatList
import Data.Dottable
import Prelude
import Data.Foldable
{-
class QR a where
   qr :: a -> Tuple a a

instance qrM2 :: QR a => QR (M2 a) where
   qr (M2 a b c d) = Tuple () () where
                                (Tuple q1 r1) = qr (V2 a c)
                                (V2 c' d') = dot q1 (V2 c d) 
                                (Tuple q2 r2) = qr d'

instance qrH2 :: QR a => QR (H2 a) where
   qr (H2 (V2 x y)) = Tuple () () where
                                (Tuple q1 r1) = qr x
                                y' = dot q1 y
                                (Tuple q2 r2) = qr y'

instance qrV2 :: QR a => QR (V2 a) where
   qr (V2 x y) = Tuple () () where
                                (Tuple q1 r1) = qr x
                                y' = dot q1 y
                                (Tuple q2 r2) = qr y'
-}
-- expect a stack of H2 then V2?
-- return house holder reflection matrices
-- https://www8.cs.umu.se/research/parallel/recursion/recursive-qr/all_in_one.html

-- Free H2 (C4 V2 a)
-- QR does not require a square matrix.
{-
qr H2 x y =  (house, r) = qr x
-}

-- it feels like we need the interpetation of a matrix as a set of side by side column vectors.

-- HouseHolder does not hold houses. It holds a sequence of vectors over which to perform sequential housolder transformations
newtype HouseHolder a = HouseHolder (CatList a) 

instance composeHouseholder :: Dottable (HouseHolder a) (HouseHolder a) (HouseHolder a) where
   dot (HouseHolder x) (HouseHolder y) = HouseHolder (x <> y) 
{-
instance applyHouseholder :: (Ring a, Dottable a a b, Semiring b) => Dottable (HouseHolder a) a a where
   dot (HouseHolder vs) x = foldl (\x' v -> let s = dot v x' in
                                            x' - (smult (s + s) v)) x vs 
-}
-- the inverse of a housholder transformation is itself.
recipH = id


-- QR does not feel like a Vector Space notion. It feels like a matrix notion. It is a basis dependent decomposition. Depends on the columns, which are the image of
-- the basis vector under the linear operation.
-- that is one easy wayo to extract the columns. Use matrix on basis list.