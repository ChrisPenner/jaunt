module Test.Main where

import Prelude
import Test.Assert

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)


main :: forall e. Eff (assert :: ASSERT | e) Unit
main = do
  assert true
