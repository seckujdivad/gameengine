module GameEngineServerTest.State.Scene.Model.Main (tests) where

import Test.HUnit (Test (TestList))

import qualified GameEngineServerTest.State.Scene.Model.GeometryIntersection (tests)


tests :: Test
tests = TestList [GameEngineServerTest.State.Scene.Model.GeometryIntersection.tests]