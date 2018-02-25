module Random where

import Prelude
import Data.CatList

{-
-- 
newtype RandomV a = Random (Eff (random :: RANDOM) a)
-- basis type b and number type a
data RandomWalker b a  = RandomWalker (Eff (random :: RANDOM) CatList (Tuple b a))

-- I should be using a stack.
instance semiringRandomWalker :: Semiring a => Semiring (RandomWalker b a) where
   add = 

newtype Ensemble a = Ensemble (Eff (random :: RANDOM) (CatList a))
-- ensemble adds via list appendning
-- multiplication makes little sense.

instance monoidEnsemble :: Monoid Ensemble where
  mappend (Ensemble x) (Ensemble y)
--explicit ensemble or history based  -- use WriterT for logging?
-- finidnig explciit stationary distro is eigenvalue power

-- writerT to keep log
newtype RandomM a b = RandomM (a -> RandomV b)

newtype RandomM a b = RandomM (a -> RandomV b)

newtype ArrayWriter a = ArrayWriter Eff (state :: ST Array a) a

newtype  a -> Eff (random :: RANDOM) (ArrayWriter a)

-- a walker carrying a value
type RandomWalker a = RandomV (Tuple a Number)



instance semiringRandomM :: Semiring a => RandomM a a where
  -- add = lift2 add

   add (RandomM f) (RandomM g) = \a -> do x <- f a
   										  y <- f a
   										  return (x + y) -- equally chooce from f or g?
  
   zero = const (pure zero)
   one = const (pure one)
-}

{-
instance semiringRandomM :: RandomM a a where
   add (RandomM f) (RandomM g) = (RandomM f) -- equally chooce from f or g?
   zero = 
-}






-- dottableRandomM

{-
instance semiringkliesli :: (Monad m, Semiring a) => Semiring (a -> m a) where
   add 
   mul x y = 
   -}