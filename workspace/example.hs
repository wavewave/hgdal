{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String      ( IsString(fromString) )
import Foreign.C.String ( CString, newCString )
import Foreign.C.Types  ( CUInt )
import Foreign.Ptr      ( nullPtr )
import System.IO.Unsafe ( unsafePerformIO )
--
import GDAL

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

gDAL_OF_VECTOR :: CUInt
gDAL_OF_VECTOR = 4

main :: IO ()
main = do
  putStrLn "testing hgdal"
  gDALAllRegister
  poDS <- gDALOpenEx ("tl_2019_us_state.shp"::CString) gDAL_OF_VECTOR nullPtr nullPtr nullPtr
  n1 <- getLayerCount poDS
  print n1
  poLayer <- getLayer poDS 0
  n2 <- getFeatureCount poLayer 1
  print n2
  resetReading poLayer
  poFeature <- getNextFeature poLayer
  n3 <- oGRFeature_GetFieldCount poFeature
  print n3
  pure ()
