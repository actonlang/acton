-- SPDX-License-Identifier: BSD-3-Clause

module Main(main) where

import qualified Acton.Env as Env
import qualified ReachabilityRowsTests
import qualified ReachabilityTests
import qualified SelectiveBackTests
import qualified SelectiveWorklistTests
import Test.Syd
import qualified WitnessForwardingTests


main :: IO ()
main = do
    builtinEnv <- Env.initEnv "" True
    reachabilityRows <- ReachabilityRowsTests.buildFixture
    sydTest $ do
      ReachabilityTests.tests builtinEnv
      ReachabilityRowsTests.tests reachabilityRows
      SelectiveWorklistTests.tests
      SelectiveBackTests.tests
      WitnessForwardingTests.tests
