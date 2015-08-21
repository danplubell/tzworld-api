{-#LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.TZworld.Api
import System.Environment
import Text.Read

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
     then error "usage: tzbl <latitude value> <longitude value>"
     else do
       lat <-case readMaybe $ head args::Maybe Double of
               Just a -> return a
               Nothing -> error "The latitude value was invalid"
  
       long <-case readMaybe $ args!!1::Maybe Double of
                Just a -> return a
                Nothing -> error "The longitude value was invalid"
  
       tz <- findTZByLoc (lat, long)
       putStrLn $ "The time zone is [" `mappend` tz `mappend` "] for latitude: " `mappend` show lat `mappend` " longitude: " `mappend` show long
  
