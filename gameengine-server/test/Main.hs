module Main (main) where

import Test.HUnit (runTestTTAndExit)

import qualified GameEngineServerTest.State.Scene.Model.GeometryIntersection (tests)

main :: IO ()
main = do
    runTestTTAndExit GameEngineServerTest.State.Scene.Model.GeometryIntersection.tests
