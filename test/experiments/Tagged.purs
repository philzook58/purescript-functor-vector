module Tagged where

import Prelude
import Data.Int.Bits
import Data.Int
import Data.Enum
import Representable
import Data.Newtype
import Data.Maybe
import Partial.Unsafe
import Data.Either
import Data.Tuple

newtype Tagged tag a = Tagged a

data Tag

type Exampletag = { littleEndian :: Tag, fourier :: Tag , triangular :: Tag }

{-
littleBig :: forall f a b. Representable f a => BoundedEnum a => f b -> f b
littleBig x =  tabulate $ (index x <<< unsafePartial <<< fromJust <<< toEnum <<< reversebits card <<< fromEnum) where
                                                      card = ceil $ log 2 $ unwrap (cardinality :: Cardinality a)
-}
reversebits :: Int -> Int -> Int
reversebits n x = go n x 0 where
                         go 0 _ acc = acc
                         go n x' acc = go (n-1) (shr x 1) ((mod x 2) * (pow 2 n) + acc)


-- I would like to reverse a list of tuples?
--reversing index?

class Reversable a r | a -> r where
  reverse :: a -> r


instance tuplerverse :: (Reversable a a', Reversable b b') => Reversable (Tuple a b) (Tuple b' a') where
  reverse (Tuple x y) = Tuple (reverse y) (reverse x)

instance booleanrevrse :: Reversable Boolean Boolean where
  reverse = not -- id?

instance unitreverse :: Reversable Unit Unit where
  reverse = id

instance eitherreverse :: (Reversable a a', Reversable b b') => Reversable (Either a b) (Either b' a') where
  reverse (Left x) = (Right (reverse x))
  reverse (Right x) = (Left (reverse x))



{-
	for fft
twiddle = map (\i -> Polar 2 * pi * i / cardiality   1.0)  fillRange
-}


