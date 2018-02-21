module Tagged where

import Prelude
import Data.Int.Bits
import Data.Int
import Data.Enum
import Representable
import Data.Newtype
import Data.Maybe
import Partial.Unsafe

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







