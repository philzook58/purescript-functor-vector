module QR where

import Data.Tuple
import Data.BinVec
import Data.H2
import Data.CatList
import Data.Dottable
import Prelude
import Data.Foldable
import Math
import Data.List
import Data.Complex
{-
class QR a where
   qr :: a -> Tuple a a

instance qrM2 :: QR a => QR (M2 a) where
   qr (M2 a b c d) = Tuple () () where
                                (Tuple q1 r1) = qr (V2 a c)
                                (V2 c' d') = dot q1 (V2 c d) 
                                (Tuple q2 r2) = qr d'

instance qrH2 :: QR a => QR (H2 a) where
   qr (H2 (V2 x y)) = Tuple () () where
                                (Tuple q1 r1) = qr x
                                y' = dot q1 y
                                (Tuple q2 r2) = qr y'

instance qrV2 :: QR a => QR (V2 a) where
   qr (V2 x y) = Tuple () () where
                                (Tuple q1 r1) = qr x
                                y' = dot q1 y
                                (Tuple q2 r2) = qr y'
-}
-- expect a stack of H2 then V2?
-- return house holder reflection matrices
-- https://www8.cs.umu.se/research/parallel/recursion/recursive-qr/all_in_one.html

-- Free H2 (C4 V2 a)
-- QR does not require a square matrix.
{-
qr H2 x y =  (house, r) = qr x
-}

-- it feels like we need the interpetation of a matrix as a set of side by side column vectors.

-- HouseHolder does not hold houses. It holds a sequence of vectors over which to perform sequential housolder transformations
newtype HouseHolder a = HouseHolder (CatList a) 

instance composeHouseholder :: Dottable (HouseHolder a) (HouseHolder a) (HouseHolder a) where
   dot (HouseHolder x) (HouseHolder y) = HouseHolder (x <> y) 
{-
instance applyHouseholder :: (Ring a, Dottable a a b, Semiring b) => Dottable (HouseHolder a) a a where
   dot (HouseHolder vs) x = foldl (\x' v -> let s = dot v x' in
                                            x' - (smult (s + s) v)) x vs 
-}
-- the inverse of a housholder transformation is itself.
recipH = id


-- QR does not feel like a Vector Space notion. It feels like a matrix notion. It is a basis dependent decomposition. Depends on the columns, which are the image of
-- the basis vector under the linear operation.
-- that is one easy wayo to extract the columns. Use matrix on basis list.

-- Householder relies on the current basis. It has an implciit subspace inclusion relation
norm :: forall a. Dottable a a Number => a -> Number
norm x = sqrt (dot x x)
normalize :: forall f. Dottable (f Number) (f Number) Number => Functor f => f Number -> f Number
normalize x = smult (recip (norm x)) x

-- does not store R.
-- Dottable (f Number) (f Number) Number => Func 
orthogonalize :: forall f. Dottable (f Number) (f Number) Number => Functor f => Semiring (f Number) => List (f Number) -> List (f Number)  -- List (V2 Number) -> List (V2 Number)
orthogonalize Nil = Nil
orthogonalize (x : xs) = u : orthogonalize xs' where
                         u = normalize x
                         xs' = map (\v -> dot (Project u) v) xs



-- Type classes to generate implicit matrices from vectors
newtype Diag a = Diag a -- Diagonal of vector
newtype Project a = Project a -- I - u uT

-- Special Operators
newtype Reflect a = Reflect a -- I - 2 u uT
newtype Circulant a = Circulant (V2 a) -- via the fft -- circulant is either built as the top row, or as the fourier transform of that row
newtype Toeplitz a = Toeplitz a -- The opposite diagonal filled out in a toeplitz manner
newtype Fourier a = Fourier a -- Designates that a is in the fourier domain

--instance dottablecirculant :: Dottable a b c => Dottable (Circulant a) (Circulant b) (Circulant c) where
--   dot (Circulant x) (Circulant y) = x * y

-- class (Functor f, Semiring a) <= SMult (f a)

project :: forall f a. Functor f => Semiring (f a) => Ring a => Dottable (f a) (f a) a => f a -> f a -> f a 
project x y = y + (smult (negate (dot x y)) x)

diag x y = x * y

instance projectDottable :: (Ring a, Semiring (f a), Functor f, Dottable (f a) (f a) a) => Dottable (Project (f a)) (f a) (f a) where
   dot (Project x) y = project x y  -- y + (smult (negate (dot x y)) x)

instance diagDottable :: (Semiring a) => Dottable (Diag a) a a where
   dot (Diag x) y = diag x y  -- y + (smult (negate (dot x y)) x)



class Haar a where
   haar :: a -> a
-- guarantees perfect balancing
-- assumes the little endian ordering
instance haarV2 :: (Haar a, Ring a) => Haar (V2 a) where   
   haar (V2 a b) = V2 (a - b) (haar (a + b)) 

instance haarNumber :: Haar Number where
   haar = id
{-
  --The matrix that represents the haar transform. Could be useful if one uses the free semiring.
  -- can be built by tabulating? de dotting. 
instance haarM2 :: (Haar a, Semiring a) => HaarM (M2 a) where
   haar = M2 one (zero - one) z z where
                                  z = haar
-}

class FFT a where
   fft :: a -> a 
-- Complex a -> Complex a
-- -- Complex (f a) -> Complex (f a)
-- fft :: a -> Fourier a
-- ifft :: a -> Fourier a
-- twiddle :: a -> a
-- twiddle :: a

-- Haar and FFT both fall under Similarity Transformations.
-- Similarity (Fourier a) a

-- Does this have any reason to be a class?
{-
class Similarity m a b where
  to :: Dottable m a b
  from :: Dottable m b a
-}
-- Lenslike Iso.


   -- or perhaps (a, a) can push up the twiddles. twiddle' = V2 twiddle (smult exp (i pi / N)  twiddle)
   -- with an external convenince function that dumps the twiddle.
   -- or even a function that accepts the twiddle.
   -- is making twiddle a convenince function bad? I feel like it might be exactly awful.
{-
instance fftV2 :: (FFT a, Semiring a, BoundedEnum b, Representable a b) => FFT (V2 (f a)) where
   fft (V2 a b) = V2 (a' + twiddle * b') (a' - twiddle * b') where
                                                            a' = fft a
                                                            b' = fft b
                                                            Cardinality n = cardinality :: Cardinality b 
                                                            twiddle :: (f a)
                                                            twiddle = map (\m -> cexp (i * 3.1459 *  m  / n ) fillRange 


   twiddle = (V2 (smult a twiddle) (smult (negate a) twiddle)) where a = cexp (i * cpi / (2 * n))
class Twiddle where
   twiddle
-}
instance fftComplex :: FFT (Complex Number) where
   fft = id
{-
instance fftComplex :: FFT (Complex Number) where
   fft x = (Tuple x 1.0)
   twiddle = 1.0
-}
