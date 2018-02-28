module Algos where

import Prelude
import Data.Taylor
import Data.List.Lazy (iterate)


-- geometric series for 1 / (1-x)
-- basically the main thing.
-- Should go into the Taylor package itself.
geo :: forall a. Semiring a => a -> Taylor a
geo x = Taylor $ iterate (mul x) one


-- AA^-1 = I
-- AA^-1 + P = I - P
-- PA^-1= I - ()

-- 1/A = 1/ (A + P - P) = 1 / P(I - P^-1(P - A)) = ( 1 + P^-1(P-A) + ...   ) P^-1
-- attempt to choose P such that eigenvalues of (P-A) are small and P is easily invertible. 
-- P = Diagonal = Jacobi
-- P = Triangular = Gauss-Seidel
-- and others. 
iterativeinverse p a = (geo (rp * (p - a) )) * (constant rp) where rp = recip p

--jacobi a = iterativeinverse (projectdiagonal a) a



-- Hmm. I may require a taylor series that uses dottable. That's fine
--- iterativeinverse p a = (geo (dot rp (p - a)))  `dot` (constant rp) where rp = recip p

-- Define Dottable instance for geo
-- Dottable a b c => Dottable (Taylor a) (Taylor b) (Taylor c)

-- Also term by term dottability? (for example two functions get multiplied into a new function)
-- well taylor holds a single type.
-- you might need the Free equivlant of taylor.
-- in any case I guess this would still work.


-- -P x_n+1 = b - (A-P)x_n
-- This is the same thing?