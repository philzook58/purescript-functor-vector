module Data.Functor.Representable where

import Prelude
import Data.Functor.Compose (Compose(..))
import Data.Tuple (Tuple(..), curry, uncurry)
import Data.Enum (class BoundedEnum, Cardinality, cardinality, fromEnum, toEnum)
import Data.Int.Bits (shr)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust, Maybe)
import Data.Array (range, fromFoldable, (!!))
import Data.Newtype (unwrap)
import Data.Foldable

class Functor f <= Representable f a | f -> a where
   tabulate :: forall b. (a -> b) -> f b
   index :: forall b. f b -> (a -> b)


instance arrowRep :: Representable ((->) a) a where
   tabulate = id
   index = id

instance repCompose :: (Representable f a, Representable g b) => Representable (Compose f g) (Tuple a b) where
   tabulate = Compose <<< tabulate <<< map tabulate <<< curry
   index (Compose fg) (Tuple i j) = index (index fg i) j



-- Similarly for Product -> Either a b

fillRange :: forall f a. BoundedEnum a => Representable f a => f Int
fillRange = fillFromIndex id -- tabulate (\x -> (fromEnum x))

fillFromIndex :: forall a f b. BoundedEnum a => Representable f a => (Int -> b) -> f b  
fillFromIndex f = tabulate (f <<< fromEnum)

basis :: forall a. BoundedEnum a => Array a
basis = unsafePartial $ map (fromJust <<< toEnum) (range 0 $ unwrap (cardinality :: Cardinality a))

-- Use this to fill iterated M2 instances
fillFromZIndex :: forall a f b. BoundedEnum a => Representable f a => (Int -> Int -> b) -> f b  
fillFromZIndex f = tabulate (uncurry f <<< unzorder <<< fromEnum)


fillFromRowMajorIndex :: forall a f b. BoundedEnum a => Representable f a => Int -> (Int -> Int -> b) -> f b  
fillFromRowMajorIndex vh f = tabulate (uncurry f <<< unrowmajor vh vh <<< fromEnum)

fillFromColMajorIndex :: forall a f b. BoundedEnum a => Representable f a => Int -> (Int -> Int -> b) -> f b  
fillFromColMajorIndex vh f = tabulate (uncurry f <<< uncolmajor vh vh <<< fromEnum)


zorder :: Int -> Int -> Int
zorder x y | x == 0 && y == 0 = 0 
zorder x y  = (mod x 2) + 2 * (mod y 2) + 4 * (zorder (shr x 1) (shr y 1))

unzorder :: Int -> Tuple Int Int
unzorder 0 = Tuple 0 0
unzorder z = Tuple x y where
		                   Tuple x' y' = unzorder (shr z 2)
		                   x = (mod z 2) + x' * 2
		                   y = (mod (shr z 1) 2) + y' * 2

colmajor :: Int -> Int -> Int -> Int -> Int
colmajor v h r c = r + v * c 

uncolmajor :: Int  -> Int -> Int -> (Tuple Int Int)
uncolmajor v h i = Tuple (mod i v) (div i v) 


rowmajor :: Int -> Int -> Int -> Int -> Int
rowmajor v h r c = c + h * r 

unrowmajor :: Int  -> Int -> Int -> (Tuple Int Int)
unrowmajor v h i = Tuple (mod i h) (div i h) 

fromArray :: forall f b a. Representable f b => BoundedEnum b => Array a -> f (Maybe a)
fromArray xs = fillFromIndex (\m -> xs !! m)

toArray :: forall f a. Foldable f => f a -> Array a 
toArray = fromFoldable

flatten = toArray



