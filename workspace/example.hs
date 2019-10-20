{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Loops ( whileM, whileJust_ )
import Data.Foldable    ( for_ )
import Data.String      ( IsString(fromString) )
import Foreign.C.String ( CString, newCString, peekCAString )
import Foreign.C.Types  ( CUInt )
import Foreign.Marshal.Utils ( toBool )
import Foreign.Ptr      ( nullPtr )
import System.IO.Unsafe ( unsafePerformIO )
--
import GDAL
import GDAL.OGREnvelope.Implementation ( oGREnvelope_MinX_get
                                       , oGREnvelope_MaxX_get
                                       , oGREnvelope_MinY_get
                                       , oGREnvelope_MaxY_get
                                       )

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s


-- some enum definition

gDAL_OF_VECTOR :: CUInt
gDAL_OF_VECTOR = 4

oFTInteger :: CUInt
oFTInteger = 0

oFTIntegerList :: CUInt
oFTIntegerList = 1

oFTString :: CUInt
oFTString = 4

oFTInteger64 :: CUInt
oFTInteger64 = 12


wkbPolygon :: CUInt
wkbPolygon = 3

wkbMultiPolygon :: CUInt
wkbMultiPolygon = 6

-- end of enum


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
        str1 <- peekCAString cstr
        -- putStrLn $ "GetNameRef poFieldDfn = " ++ str
        t <- oGRFieldDefn_GetType poFieldDfn
        str2 <-
          if | t == oFTInteger   -> do
               v <- oGRFeature_GetFieldAsInteger poFeature i
               pure (show v)
             | t == oFTString    -> do
               v <- oGRFeature_GetFieldAsString poFeature i
               v' <- peekCAString v
               pure v'
             | t == oFTInteger64 -> do
               v <- oGRFeature_GetFieldAsInteger64 poFeature i
               pure (show v)
             | otherwise         -> pure "otherwise"
        putStrLn $ str1 ++ " = " ++ str2
      poGeometry <- oGRFeature_GetGeometryRef poFeature
      t' <- getGeometryType poGeometry
      str3 <-
        if | t' == wkbPolygon      -> do
             poPoly <- oGRGeometry_toPolygon poGeometry
             poRing <- oGRPolygon_getExteriorRing poPoly
             n6 <- getNumPoints poRing
             iter <- getPointIterator poRing
             p <- newOGRPoint
             xys <-
               whileM (toBool <$> getNextPoint iter p) $ do
                 x <- oGRPoint_getX p
                 y <- oGRPoint_getY p
                 pure (x,y)
             print xys


             poEnv <- newOGREnvelope
             getEnvelope poPoly poEnv
             xmin <- oGREnvelope_MinX_get poEnv
             xmax <- oGREnvelope_MaxX_get poEnv
             ymin <- oGREnvelope_MinY_get poEnv
             ymax <- oGREnvelope_MaxY_get poEnv
             pure ("wkbPolygon: " ++ show ((xmin,ymin),(xmax,ymax)) ++ ", n6 = " ++ show n6)
           | t' == wkbMultiPolygon -> pure "wkbMultiPolygon"
           | otherwise             -> pure "otherwise"

      print str3
  pure ()
