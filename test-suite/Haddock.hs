module Main (main) where

import System.Process (readProcess)
import System.Exit

arguments :: [String]
arguments =
    [ "haddock"
    ]


main :: IO ()
main = do
    output <- readProcess "cabal" arguments ""
    putStr output >> exitSuccess

