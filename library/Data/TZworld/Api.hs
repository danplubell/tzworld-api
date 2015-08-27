{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-|
Module      : Data.TZworld.Api
Description : An api that provides a way to lookup a timezone by location
Copyright   : (c) Dan Plubell, 2015
                  
License     : MIT
Maintainer  : danplubell@gmail.com
Stability   : experimental
Portability : portable

This module expose a method for finding an Olson time zone for a location.
The location is provided as a latitude and longitude value.

-}

module Data.TZworld.Api (findTZByLoc) where

import qualified Data.List as DL
import qualified Data.Binary as DB
import GHC.Generics
import qualified Data.Set as DS
import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple
import Paths_tzworld_api
import Control.Exception
import qualified Control.Exception.Enclosed as CE
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either


data TZWorldField = TZWorldField {
      id_::Int
    , bucketbytes::BL.ByteString
  } deriving (Show,Generic)

instance FromRow TZWorldField where
  fromRow = TZWorldField <$> field <*> field

instance ToRow TZWorldField where
  toRow (TZWorldField id' blob) = toRow (id', blob)

type Longitude = Double -- | The longitude of the location
type Latitude = Double  -- | The latitude of the location

{- A polygon that has a collection of indexed polygon coordinates  -}
data TZPoly = TZPoly {
  tzpolyname::String
  , tzpolycoords::[((Latitude,Longitude),(Latitude,Longitude))]
  , tzpolyminlong::Longitude
  , tzpolymaxlong::Longitude
  } deriving (Show, Read,Generic,Eq,Ord)
instance DB.Binary TZPoly


pnpolyt::(Latitude,Longitude)->Bool->((Latitude,Longitude),(Latitude,Longitude))->Bool
pnpolyt (tx,ty) b ((xi,yi),(xj,yj))   = if pnpolytest tx ty xi yi xj yj  
                                       then not b
                                       else b

pnpolytest:: Latitude->Longitude->Latitude->Longitude->Latitude->Longitude->Bool
pnpolytest testx testy vertxi vertyi vertxj vertyj =
  ((vertyi > testy) /= (vertyj > testy)) &&
     (testx < (vertxj - vertxi) * (testy - vertyi)/(vertyj - vertyi) + vertxi)

pnpoly::(Latitude,Longitude)->[((Latitude,Longitude),(Latitude,Longitude))]->Bool
pnpoly t  = DL.foldl' (pnpolyt t) False  

isPointInPoly::(Latitude,Longitude)->TZPoly->Bool
isPointInPoly t p = pnpoly t (tzpolycoords p)

checkTZByLoc :: (Latitude,Longitude) -> DS.Set TZPoly -> String
checkTZByLoc t  = DS.foldl' getTZName []  
                      where getTZName b a  = if isPointInPoly t a
                                             then tzpolyname a
                                             else b
--buckets are 15 degrees of longitude
calcBucket ::Longitude -> Int
calcBucket c = floor ( c/15.00)::Int

catchAny:: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
 
{- Look in the database for the appropriate bucket -}
getLongitudeBucket::Longitude -> IO (Either String (DS.Set TZPoly))
getLongitudeBucket l  = do
  fp <- getDataFileName "data/tzworld.db"  
  e <- runEitherT $ do
    dbio <- lift $ CE.tryAny $ open fp
    conn <- case dbio of
             Right c -> lift $ return c
             Left _ -> left $ "Error opening database: " `mappend` fp
    erio <- lift $ CE.tryAny
      (query conn "SELECT * FROM tzworld where id = ?" (Only(calcBucketId l::Int))::IO [TZWorldField])
    r <- case erio of
           Right rio -> return rio
           Left e -> left $ show e  
    if null r
    then left "The longitude value was not in range.  The valid range is -180 to 180"
    else
        return $ Right (DB.decode (bucketbytes (head r))::(DS.Set TZPoly))
    
  case e of
    Left a -> return $ Left a -- the exception message
    Right b -> return b       -- the result

  where calcBucketId l' = 12 + calcBucket l'

-- | Find an Olson time zone by providing the latitude and longitude of a location 
findTZByLoc::(Latitude,Longitude)                  -- ^ The latitude and the longitude of the location
             -> IO (Either String  (Maybe String)) -- ^ The Olson time zone if it is defined for the location
                                                   -- ^ Or an error string if a problem occurred while retrieving time zone
findTZByLoc (la,lo) = do
  tzpolyset <- getLongitudeBucket lo
  return $
   case tzpolyset of
              Right s  -> do let tz = checkTZByLoc (la,lo) s 
                             if null tz then Right Nothing else Right (Just tz)
              Left str -> Left str
