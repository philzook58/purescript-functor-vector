module Data.MaxPlus where

import Prelude

data MaxPlus a = MaxPlus a | NegInfinity

instance maxplusEq :: Eq a => Eq (MaxPlus a) where
   eq NegInfinity NegInfinity = true
   eq NegInfinity _ = false
   eq _ NegInfinity = false
   eq (MaxPlus x) (MaxPlus y) = (eq x y)


instance maxplusOrd :: Ord a => Ord (MaxPlus a) where
   compare NegInfinity NegInfinity = EQ
   compare NegInfinity _ = LT
   compare _ NegInfinity = GT
   compare (MaxPlus x) (MaxPlus y) = (compare x y)


instance maxplusSemiRing :: (Ord a, Semiring a) => Semiring (MaxPlus a) where
   add = max
   zero = NegInfinity
   mul (MaxPlus x) (MaxPlus y) = MaxPlus $ add x y
   mul NegInfinity _ = NegInfinity
   mul _ NegInfinity = NegInfinity
   one = MaxPlus zero
