module Laplace where

import Prelude
import Data.BinMat

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