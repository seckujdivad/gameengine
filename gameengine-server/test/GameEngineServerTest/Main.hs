module GameEngineServerTest.Main (tests) where

import Test.HUnit (Test (TestList))

import qualified GameEngineServerTest.State.Main (tests)


tests :: Test
tests = TestList [GameEngineServerTest.State.Main.tests]