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


-- Do not export the constructor of Finite

-- newtype Finite n = Finite Int
data Finite n = Finite n Int

instance Num n => Enum (Finite n) where
   succ (Finite d x) = if x < (toInt d) then (Just $ Finite d (x+1)) else Nothing


-- or
data Mod n = Mod n Int

-- I shouldn't have to store n in the type? Where is the function that gives me that value?
-- Mod is the OG example for reficying/reflecting

instance (Num n) => Semiring (Mod n) where
   add (Mod n x) (Mod _ y) = Mod $ n  (mod (x + y) (toInt n))



-}