module SoftMaxPlus where

import Prelude
import Math

newtype SoftMaxPlus = SoftMaxPlus Number

instance smieirngSoftMaxPlus :: Semiring SoftMaxPlus where
   add (SoftMaxPlus x) (SoftMaxPlus y) = SoftMaxPlus $ log ((exp x) + (exp y))
   zero = SoftMaxPlus $	-1000000000.0 -- negate infinity
   mul (SoftMaxPlus x) (SoftMaxPlus y) = SoftMaxPlus $ x + y
   one = SoftMaxPlus zero

-- we hsould perhaps be using Complex Number, then negate is ok and we can subtract larger numbers from smaller.
instance ringSoftMaxPlus :: Ring SoftMaxPlus where
   sub (SoftMaxPlus x) (SoftMaxPlus y) = SoftMaxPlus $ log ((exp x) - (exp y))

instance divisionRingSoftMaxPlus :: DivisionRing SoftMaxPlus where
   recip (SoftMaxPlus x) = SoftMaxPlus $ negate x

-- Is softmax plus irrelevant since you can just elementwise esponentiate and then use regular semiring?