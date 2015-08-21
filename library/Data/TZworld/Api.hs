{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}
{-|
Module      : Data.TZworld.Api
Description : An api that provides a way to lookup a timezone by location
Copyright   : (c) Dan Plubell, 2015
                  
License     : MIT
Maintainer  : danplubell@gmail.com
Stability   : experimental
Portability : POSIX

This module expose a method for finding an Olson time zone a location.
The location is input as a latitude and longitude value.

-}

module Data.TZworld.Api (findTZByLoc) where

import qualified Data.List as DL
import qualified Data.Binary as DB
import GHC.Generics
import qualified Data.Set as DS
import qualified Data.ByteString.Lazy as BL
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Paths_tzworld_api

data TZWorldField = TZWorldField {
      id_::Int
    , bucketbytes::BL.ByteString
  } deriving (Show,Generic)

instance FromRow TZWorldField where
  fromRow = TZWorldField <$> field <*> field

instance ToRow TZWorldField where
  toRow (TZWorldField id' blob) = toRow (id', blob)

type Longitude = Double 
type Latitude = Double
{- A polygon that has a collection of indexed polygon coordinates  -}
data TZPoly = TZPoly {
  tzpolyname::String
  , tzpolycoords::[((Latitude,Longitude),(Latitude,Longitude))]
  , tzpolyminlong::Double
  , tzpolymaxlong::Double
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

isPointInPoly::(Double,Double)->TZPoly->Bool
isPointInPoly t p = pnpoly t (tzpolycoords p)

checkTZByLoc :: (Double,Double) -> DS.Set TZPoly -> String
checkTZByLoc t  = DS.foldl' getTZName []  
                      where getTZName b a  = if isPointInPoly t a
                                             then tzpolyname a
                                             else b
--buckets are 15 degrees of longitude
calcBucket ::Double -> Int
calcBucket c = floor ( c/15.00)::Int

getLongitudeBucket::Double -> IO (DS.Set TZPoly)
getLongitudeBucket l = loadBucket (calcBucketId l)
  where calcBucketId l' = 12 + calcBucket l'
                        
loadBucket::Int -> IO (DS.Set TZPoly)
loadBucket id' = do
  fp <- getDataFileName "tzworld.db"
  db <- open fp
  r <- query db "SELECT * FROM tzworld where id = ?" (Only(id'::Int))::IO [TZWorldField]
  
  let tzbin = DB.decode (bucketbytes (head r))::(DS.Set TZPoly)
  close db
  return tzbin
-- | Find an Olson time zone by providing the latitude and longitude of a location 
findTZByLoc::(Latitude,Longitude) -- ^ The latitude and the longitude of the location
             -> IO (Maybe String) -- ^ The Olson time zone if it is defined for the location
findTZByLoc (la,lo) = do
  tzpolyset <- getLongitudeBucket lo
  let tz = checkTZByLoc (la, lo) tzpolyset
  return $ if null tz  then Nothing else Just tz
            
  
