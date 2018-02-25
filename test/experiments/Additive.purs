module Data.Additive where


class Additive a where
   add' :: a -> a -> a
   zero' :: a

class Multiplicative a where
   mul' :: a -> a -> a
   one' :: a
-- not dottable, because dottable enables heterogenous mutlpilication

-- unfortunately an orphan instance
-- instance Multiplicative, Additive => Semiring