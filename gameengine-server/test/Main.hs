module Main (main) where

import Test.HUnit (runTestTTAndExit)

import qualified GameEngineServerTest.Main (tests)


main :: IO ()
main = runTestTTAndExit GameEngineServerTest.Main.tests