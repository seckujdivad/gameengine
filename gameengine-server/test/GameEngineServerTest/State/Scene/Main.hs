module GameEngineServerTest.State.Scene.Main (tests) where

import Test.HUnit (Test (TestList))

import qualified GameEngineServerTest.State.Scene.Model.Main (tests)


tests :: Test
tests = TestList [GameEngineServerTest.State.Scene.Model.Main.tests]