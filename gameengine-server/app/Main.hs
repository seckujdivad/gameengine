module Main where

import System.IO (withFile, IOMode(ReadMode), hClose, hGetContents)
import Control.Exception (bracket)

import Lib (someFunc)

main :: IO ()
main = do
    someFunc
    withFile "resources/file.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn contents)
