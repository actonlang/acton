-- SPDX-License-Identifier: BSD-3-Clause

module Main(main) where

import qualified Acton.Env as Env
import qualified ReachabilityRowsTests
import qualified ReachabilitySelectionTests
import qualified ReachabilityTests
import qualified SelectiveBackTests
import Test.Syd
import qualified WitnessForwardingTests


main :: IO ()
main = do
    builtinEnv <- Env.initEnv "" True
    reachabilityRows <- ReachabilityRowsTests.buildFixture
    sydTest $ do
      ReachabilityTests.tests builtinEnv
      ReachabilityRowsTests.tests reachabilityRows
      ReachabilitySelectionTests.tests
      SelectiveBackTests.tests
      WitnessForwardingTests.tests
