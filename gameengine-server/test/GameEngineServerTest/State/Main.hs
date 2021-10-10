module GameEngineServerTest.State.Main (tests) where

import Test.HUnit (Test (TestList))

import qualified GameEngineServerTest.State.Scene.Main (tests)


tests :: Test
tests = TestList [GameEngineServerTest.State.Scene.Main.tests]