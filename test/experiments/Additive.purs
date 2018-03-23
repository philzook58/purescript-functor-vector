module Data.Additive where

import Prelude

class Additive a where
   add' :: a -> a -> a
   zero' :: a

-- Really is a duplication of Monoid. 
-- But I don't like the idea of requiring us to use (Additive Number) as the base case

instance semiringAdditive :: Semiring a => Additive a where
   add' = add
   zero' = zero

class Multiplicative a where
   mul' :: a -> a -> a
   one' :: a
-- not dottable, because dottable enables heterogenous mutlpilication

-- unfortunately an orphan instance
-- instance Multiplicative, Additive => Semiring
-- or do we want Semiring => Additive
-- Semiring => Multiplicative
-- to get automatic instances.