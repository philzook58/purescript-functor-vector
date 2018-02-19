module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import BinVec
import BinMat
import Dottable

test1 :: M4' Number
test1 = one

test2 :: M2' Number
test2 = M2 1.23 23.3 2.2 3.4

test3 = M2 (test2 + test2) (test2 * test2) test2 (test2 * test2+test2)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
  log $ show test1
  log $ show $ recip test2
  log $ show $ (recip test2) * test2
  log $ show $ (recip test3) * test3

