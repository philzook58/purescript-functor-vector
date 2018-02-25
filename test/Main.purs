module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BinVec
import Data.BinMat
import Data.Dottable
import Data.Foldable
import Data.Int
import Data.Array
import Laplace
import Data.Functor.Representable
import Data.DenseKron


test1 :: M4 Number
test1 = one

test2 :: M2 Number
test2 = M2 1.23 23.3 2.2 3.4

test3 = M2 (test2 + test2) (test2 * test2) test2 (test2 * test2+test2)

top n = "type C" <> show (n+1)  <> " f a = " <> go n <> " a\n"
go 0 = "f"
go n = "(CKron f " <> go (n-1) <> ")"

logs = log <<< show

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
  log $ show test1
  log $ show $ recip test2
  log $ show $ (recip test2) * test2
  log $ show $ (recip test3) * test3
  log $ foldMap (\n -> "type V" <> show (2*n) <> " a = V2 (V" <> show n <> " a)") $ map (\n -> pow 2 n) (1 .. 10)
  log $ foldMap (\n -> "type M" <> show (2*n) <> " a = M2 (M" <> show n <> " a)") $ map (\n -> pow 2 n) (1 .. 10)
  log $ foldMap top $ (1 .. 10)
  log $ foldMap (\n -> "type V" <> show ((pow 2 n)) <> " a = C" <> show n <> " V2 a\n") $ (1 .. 10)
  log $ foldMap (\n -> "type M" <> show ((pow 2 n)) <> " a = C" <> show n <> " M2 a\n") $ (1 .. 10)
  log $ show $ mK' * mK'
  log $ show $ recip mK'
  log $ show $ (fillRange :: V8 Int) 
  log $ show $ (fillRange :: V8 Int) * (fillRange :: V8 Int)
  log $ show $ map (\x -> x * x) (fillRange :: V8 Int)
  log $ show $ fD
  log $ show $ fD * bD
  log $ show $ bD * fD
  log $ show $ recip fD
  log $ show $ recip bD
  logs $ sigmax * sigmay - map (i * _) sigmaz 
  logs $ dkron sigmaz sigmaz
  


