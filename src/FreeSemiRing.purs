module Data.FreeSemiring where

import Prelude
import Control.Monad.Free (Free)
import Data.Functor.Representable 
import Data.DenseKron

data SemiringF a = AddF a a | ZeroF | MulF a a | OneF

newtype FSR a = FSR (Free SemiringF a)
{-
instance semiringFree :: Semiring (FSR a) where
   add (FSR x) (FSR y) = FSR $ liftF (AddF x y)
   zero = FSR $ liftF ZeroF
   mul (FSR x) (FSR y) = FSR $ liftF (MulF x y)
   one = FSR $ liftF OneF
-}

data ZeroOne a = ZeroZO | OneZO | PureZO a
--data ZeroOne f a = ZeroZO | OneZO | PureZO (f a)
--data Improve f a = ZeroI | OneI | ScalarI a | PureI (f a) 
-- instance representable Improve ... gonna require a semiring contraint
data Improve f a = ScalarI a | PureI (f a)
-- Now it doesn't. Passes right on through. Kind of collects?
-- Coyoneda another optin?


instance semirRingFree :: Semiring a => Semiring (ZeroOne a) where
	add ZeroZO x = x
	add x ZeroZO = x
	add (PureZO x) OneZO = PureZO (x + one)
	add OneZO (PureZO x) = PureZO (x + one)
	add OneZO OneZO = PureZO (one + one)
	add (PureZO x) (PureZO y) = PureZO (x + y)

	zero = ZeroZO

	mul OneZO x = x
	mul x OneZO = x
	mul _ ZeroZO = ZeroZO
	mul ZeroZO _ = ZeroZO
	mul (PureZO x) (PureZO y) = PureZO (x * y)

	one = OneZO


-- instance Repsentable Void for ZeroOne
-- ExampleStack C4 (CKron ZeroOne V2) Number
-- C4 (Improve V2) Number
{-
instance representableZeroOne :: (Semiring a, Representable f a) => Representable (DKron ZeroOne f) a where
  tabulate f = PureZO $ tabulate f 
  index (ZeroZO) = const zero
  index (OneZO) = const one
  index (PureZO x) = index x

  -} 
