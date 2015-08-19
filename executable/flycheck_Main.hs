module Main (main) where

import Data.TZworld.Api

main :: IO ()
main = do
  tz <- findTZByLoc (41.294159, -86.622625)
  putStrLn tz
  
