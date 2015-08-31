{-#LANGUAGE OverloadedStrings #-}
{-#GHC_OPTIONS -fnowarn-unused-imports #-}

module Main (main) where

import Data.TZworld.Api
import System.Environment
import Text.Read
import Data.Monoid

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
  
       tze <- findTZByLoc (lat, long)
       case tze of
         Left str -> putStrLn str
         Right tz -> case tz of
                       Nothing -> putStrLn $ "A time zone was not found for latitude: "
                                  `mappend` show lat
                                  `mappend` " longitude: "
                                  `mappend` show long
                       Just a  -> putStrLn $ "The time zone is ["
                                  `mappend` a
                                  `mappend` "] for latitude: "
                                  `mappend` show lat
                                  `mappend` " longitude: "
                                  `mappend` show long
         {-case tz of 
                         Nothing -> putStrLn $ "A time zone was not found for latitude: " `mappend` show lat `mappend` " longitude: " `mappend` show long
                         Just a  -> putStrLn $ "The time zone is [" `mappend` a `mappend` "] for latitude: " `mappend` show lat `mappend` " longitude: " `mappend` show long
-  -}
