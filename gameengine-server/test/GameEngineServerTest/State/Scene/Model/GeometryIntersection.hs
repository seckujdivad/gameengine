module GameEngineServerTest.State.Scene.Model.GeometryIntersection (tests) where

import Test.HUnit (Test (TestList, TestCase), assertEqual)


tests :: Test
tests = TestList [TestCase (assertEqual "1 is 1" "1" "1")]