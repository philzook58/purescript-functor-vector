module Typelevel where

--Replacing the Cn hierarchy with typelevel nonsense.
{-


import Data.Typelevel.Nat

-- class NComp n r | n -> r
class NComp n f r | n f -> r

instance ncompD2 :: NComp D2 f (DKron f f) 
instance ncompSucc :: (Succ a b, NComp a f r) => NComp b f (DKron f r) 


--example = forall r. (NComp D8 V2 r) => r Number
--example = zero

-- could possibly use reflection to push values up into the type level.
-- 

-}