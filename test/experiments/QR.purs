module QR where

import Data.Tuple
import Data.BinVec
import Data.H2
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