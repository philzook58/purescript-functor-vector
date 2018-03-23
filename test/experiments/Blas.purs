module BLAS where

import Prelude

class Blas1 f where
   saxpy :: forall a. Semiring a => a -> f a -> f a -> f a
   inner :: a -> a -> Number

norm2 x = inner x x

class Blas2 m v where
   mv :: forall a. Semiring a => m a -> v a -> v a
class Blas2Solve m v where
   solve :: a -> a -> Number

{-
class Blas3Square m where
   sqmatmul :: forall a. Semiring a => m a -> m a -> m a
-}
class Blas3 m m' m'' | m m' -> m'' where
   matmul :: forall a. Semiring a => m a -> m' a -> m'' a

class Blas3 m m' m'' where
   solvem :: forall a. Semiring a => m a -> m' a -> m'' a

