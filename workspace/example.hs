{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Loops ( whileJust_ )
import Data.Foldable    ( for_ )
import Data.String      ( IsString(fromString) )
import Foreign.C.String ( CString, newCString, peekCAString )
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
  putStrLn $ "getLayerCount poDS = " ++ show n1

  poLayer <- getLayer poDS 0
  n2 <- getFeatureCount poLayer 1
  putStrLn $ "getFeatureCount poLayer = " ++ show n2

  poFDefn <- getLayerDefn poLayer
  n3 <- getFieldCount poFDefn
  putStrLn $ "getFieldCount poFDefn = " ++ show n3
  n4 <- getGeomFieldCount poFDefn
  putStrLn $ "getGeomFieldCount poFDefn = " ++ show n4


  resetReading poLayer
  whileJust_ (do p@(OGRFeature p') <- getNextFeature poLayer
                 if p' == nullPtr
                   then pure Nothing
                   else pure (Just p)
             ) $
    \poFeature -> do
      putStrLn "------------------------"
      for_ [0..n3-1] $ \i -> do
        poFieldDfn <- getFieldDefn poFDefn i
        cstr <- oGRFieldDefn_GetNameRef poFieldDfn
        str <- peekCAString cstr
        putStrLn $ "GetNameRef poFieldDfn = " ++ str
  pure ()
