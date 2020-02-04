{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cstring, cstring_
                                      , double, double_
                                      , int, int_
                                      , uint, uint_
                                      , void_, voidp
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..)
                                      , modImports
                                      )
import FFICXX.Generate.Type.Class     ( Arg(..)
                                      , Class(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevel(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )


------------------------
-- import from stdcxx --
------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal = Cabal {
    cabal_pkgname            = CabalName "stdcxx"
  , cabal_version            = "0.6"
  , cabal_cheaderprefix      = "STD"
  , cabal_moduleprefix       = "STD"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

deletable :: Class
deletable =
  AbstractClass {
      class_cabal      = stdcxx_cabal
    , class_name       = "Deletable"
    , class_parents    = []
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = [ Destructor Nothing ]
    , class_vars       = []
    , class_tmpl_funcs = []
    }

-----------------
-- start hgdal --
-----------------

cabal = Cabal {
    cabal_pkgname            = CabalName "hgdal"
  , cabal_version            = "0.1.0.0"
  , cabal_cheaderprefix      = "HGDAL"
  , cabal_moduleprefix       = "GDAL"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
  , cabal_license            = Just "BSD3"
  , cabal_licensefile        = Just "LICENSE"
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = [ "gdal" ]
  , cabal_buildType          = Simple
  }

gdalclass :: String -> [Class] -> [Function] -> Class
gdalclass n ps fs =
  Class {
      class_cabal      = cabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    , class_has_proxy  = False
    }

gDALMajorObject :: Class
gDALMajorObject =
  gdalclass "GDALMajorObject" [ deletable ]
  [
  ]

gDALDataset :: Class
gDALDataset =
  gdalclass "GDALDataset" [ gDALMajorObject ]
  [ Virtual (cppclass_ oGRLayer) "GetLayer" [int "iLayer"] Nothing
  , Virtual int_ "GetLayerCount" [] Nothing
  ]

oGRCurve :: Class
oGRCurve =
  gdalclass "OGRCurve" [  oGRGeometry ]
  [ Virtual int_ "getNumPoints" [] Nothing
  , Virtual (cppclass_ oGRPointIterator) "getPointIterator" [] Nothing
  ]



oGREnvelope :: Class
oGREnvelope =
  Class {
      class_cabal      = cabal
    , class_name       = "OGREnvelope"
    , class_parents    = [ deletable ]
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      =
      [ Constructor [] Nothing
      ]
    , class_vars       =
      [ Variable (Arg double_ "MinX")
      , Variable (Arg double_ "MaxX")
      , Variable (Arg double_ "MinY")
      , Variable (Arg double_ "MaxY")
      ]
    , class_tmpl_funcs = []
    , class_has_proxy  = False
    }

oGRFeature :: Class
oGRFeature =
  gdalclass "OGRFeature" [ deletable ]
  [ NonVirtual int_ {- GIntBig = Int64 -} "GetFID" [] Nothing
  , NonVirtual int_ "GetFieldCount" [] Nothing
  , NonVirtual int_ "GetFieldAsInteger" [ int "i" ] Nothing
  , NonVirtual int_ {- GIntBig = Int64 -} "GetFieldAsInteger64" [ int "i" ] Nothing
  , NonVirtual cstring_ "GetFieldAsString" [ int "i" ] Nothing
  , NonVirtual (cppclass_ oGRGeometry) "GetGeometryRef" [] Nothing
  ]

oGRFeatureDefn :: Class
oGRFeatureDefn =
  gdalclass "OGRFeatureDefn" [ deletable ]
  [ Virtual int_ "GetFieldCount" [] Nothing
  , Virtual (cppclass_ oGRFieldDefn) "GetFieldDefn" [ int "i" ] Nothing
  , Virtual int_ "GetGeomFieldCount" [] Nothing
  ]

oGRFieldDefn :: Class
oGRFieldDefn =
  gdalclass "OGRFieldDefn" [ deletable ]
  [ NonVirtual cstring_ "GetNameRef" [] Nothing
  , NonVirtual uint_ {- OGRFieldType = enum -} "GetType" [] Nothing
  ]

oGRGeometry :: Class
oGRGeometry =
  gdalclass "OGRGeometry" [ deletable ]
  [ Virtual uint_ {- OGRwkbGeometryType -} "getGeometryType" [] Nothing
  , Virtual void_ "getEnvelope" [ cppclass oGREnvelope "psEnvelope" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Intersects" [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Equals"     [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Disjoint"   [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Touches"    [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Crosses"    [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Within"     [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Contains"   [ cppclass oGRGeometry "g" ] Nothing
  , Virtual bool_ {- OGRBoolean -}  "Overlaps"   [ cppclass oGRGeometry "g" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "Boundary"      [] Nothing
  , Virtual double_                 "Distance"      [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "ConvexHull"    [] Nothing
  , Virtual (cppclass_ oGRGeometry) "Buffer"        [ double "dfDist", int "nQuadSegs" {- default = 30 -} ] Nothing
  , Virtual (cppclass_ oGRGeometry) "Intersection"  [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "Union"         [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "UnionCascaded" [] Nothing
  , Virtual (cppclass_ oGRGeometry) "Difference"    [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "SymDifference" [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , Virtual int_ {- OGRErr -} "Centroid" [ cppclass oGRPoint "poPoint" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "Simplify"  [ double "dTolerance" ] Nothing
  , NonVirtual (cppclass_ oGRGeometry) "SimplifyPreserveTopology" [ double "dTolerance" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "DelaunayTriangulation" [ double "dfTolerance", int "bOnlyEdges" ] Nothing
  , Virtual (cppclass_ oGRGeometry) "Polygonize" [] Nothing
  , Virtual double_ "Distance3D" [ cppclass oGRGeometry "poOtherGeom" ] Nothing
  , NonVirtual (cppclass_ oGRPolygon) "toPolygon" [] Nothing
  , NonVirtual (cppclass_ oGRMultiPolygon) "toMultiPolygon" [] Nothing
  ]

oGRGeometryCollection :: Class
oGRGeometryCollection =
  gdalclass "OGRGeometryCollection" [ oGRGeometry ]
  [ NonVirtual int_ "getNumGeometries" [] Nothing
  , NonVirtual (cppclass_ oGRGeometry) "getGeometryRef" [ int "i" ] Nothing
  ]


oGRLayer :: Class
oGRLayer =
  gdalclass "OGRLayer" [ gDALMajorObject ]
  [ Virtual (cppclass_ oGRFeature) "GetFeature" [ int "nFID" {- GIntBig = 64 bit -} ] Nothing
  , Virtual int_ {- GIntBit = 64 bit -} "GetFeatureCount" [ int "bForce" ] Nothing
  , Virtual (cppclass_ oGRFeatureDefn) "GetLayerDefn" [] Nothing
  , Virtual (cppclass_ oGRFeature) "GetNextFeature" [] Nothing
  , Virtual void_ "ResetReading" [] Nothing
  ]

oGRLinearRing :: Class
oGRLinearRing =
  gdalclass "OGRLinearRing" [  oGRLineString ]
  [
  ]

oGRLineString :: Class
oGRLineString =
  gdalclass "OGRLineString" [  oGRSimpleCurve ]
  [
  ]

oGRMultiPolygon :: Class
oGRMultiPolygon =
  gdalclass "OGRMultiPolygon" [ oGRMultiSurface ]
  [
  ]

oGRMultiSurface :: Class
oGRMultiSurface =
  gdalclass "OGRMultiSurface" [ oGRGeometryCollection ]
  [
  ]

oGRPoint :: Class
oGRPoint =
  gdalclass "OGRPoint" [ oGRGeometry ]
  [ Constructor [] Nothing
  , Constructor [double "x", double "y"]                         (Just "newOGRPoint2")
  , Constructor [double "x", double "y", double "z"]             (Just "newOGRPoint3")
  , Constructor [double "x", double "y", double "z", double "m"] (Just "newOGRPoint4")
  , NonVirtual double_ "getX" [ ] Nothing
  , NonVirtual double_ "getY" [ ] Nothing
  , NonVirtual double_ "getZ" [ ] Nothing
  , NonVirtual double_ "getM" [ ] Nothing
  , NonVirtual void_   "setX" [ double "xIn" ] Nothing
  , NonVirtual void_   "setY" [ double "yIn" ] Nothing
  , NonVirtual void_   "setZ" [ double "zIn" ] Nothing
  , NonVirtual void_   "setM" [ double "mIn" ] Nothing
  ]



oGRPointIterator :: Class
oGRPointIterator =
  gdalclass "OGRPointIterator" [ deletable ]
  [ Virtual bool_ {- OGRBoolean -} "getNextPoint" [ cppclass oGRPoint "pt" ] Nothing
  ]

oGRPolygon :: Class
oGRPolygon =
  gdalclass "OGRPolygon" [ oGRCurvePolygon ]
  [ NonVirtual (cppclass_ oGRLinearRing) "getExteriorRing" [] Nothing
  ]

oGRCurvePolygon :: Class
oGRCurvePolygon =
  gdalclass "OGRCurvePolygon" [ oGRSurface ]
  [ ]

oGRSimpleCurve :: Class
oGRSimpleCurve =
  gdalclass "OGRSimpleCurve" [  oGRCurve ]
  [ NonVirtual void_ "getPoints" [ voidp "pabyX", int "nXStride"
                                 , voidp "pabyY", int "nYStride"
                                 , voidp "pabyZ", int "nZStride" ] Nothing
  ]

oGRSurface :: Class
oGRSurface =
  gdalclass "OGRSurface" [ oGRGeometry ]
  [ ]

classes =
  [ gDALDataset
  , gDALMajorObject
  , oGRCurve
  , oGRCurvePolygon
  , oGREnvelope
  , oGRFeature
  , oGRFeatureDefn
  , oGRFieldDefn
  , oGRGeometry
  , oGRGeometryCollection
  , oGRLayer
  , oGRLinearRing
  , oGRLineString
  , oGRMultiPolygon
  , oGRMultiSurface
  , oGRPoint
  , oGRPointIterator
  , oGRPolygon
  , oGRSimpleCurve
  , oGRSurface
  ]

toplevelfunctions :: [TopLevel]
toplevelfunctions =
  [ TopLevelFunction void_ "GDALAllRegister" [] Nothing
  , TopLevelFunction (cppclass_ gDALDataset) "GDALOpenEx"
    [ cstring "pszFilename", uint "nOpenFlags", charpp "papszAllowedDrivers", charpp "papszOpenOptions", charpp "papszSiblingFiles"] Nothing
  ]

templates = []

headers =
  [ ( MU_TopLevel
    , ModuleUnitImports {
        muimports_namespaces = [ ]
      , muimports_headers    = [ "gdal.h" ]
      }
    )
  , modImports "GDALMajorObject" [] ["gdal_priv.h"]
  , modImports "GDALDataset"     [] ["gdal_priv.h"]
  , modImports "OGRCurve"        [] ["ogr_geometry.h"]
  , modImports "OGRCurvePolygon" [] ["ogr_geometry.h"]
  , modImports "OGREnvelope"     [] ["ogr_core.h"]
  , modImports "OGRFeature"      [] ["ogr_feature.h"]
  , modImports "OGRFeatureDefn"  [] ["ogr_feature.h"]
  , modImports "OGRFieldDefn"    [] ["ogr_feature.h"]
  , modImports "OGRGeometry"     [] ["ogr_geometry.h"]
  , modImports "OGRGeometryCollection" [] ["ogr_geometry.h"]
  , modImports "OGRLayer"        [] ["ogrsf_frmts.h"]
  , modImports "OGRLinearRing"   [] ["ogr_geometry.h"]
  , modImports "OGRLineString"   [] ["ogr_geometry.h"]
  , modImports "OGRMultiPolygon" [] ["ogr_geometry.h"]
  , modImports "OGRMultiSurface" [] ["ogr_geometry.h"]
  , modImports "OGRPoint"        [] ["ogr_geometry.h"]
  , modImports "OGRPointIterator"[] ["ogr_geometry.h"]
  , modImports "OGRPolygon"      [] ["ogr_geometry.h"]
  , modImports "OGRSimpleCurve"  [] ["ogr_geometry.h"]
  , modImports "OGRSurface"      [] ["ogr_geometry.h"]
  ]

extraLib = []

extraDep = []


main :: IO ()
main = do
  args <- getArgs
  let tmpldir =  if length args == 1
                 then args !! 0
                 else "../template"

  cwd <- getCurrentDirectory
  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "tmp" </> "working"
               , fficxxconfig_installBaseDir = cwd </> "hgdal"
               , fficxxconfig_staticFileDir  = tmpldir
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "GDAL"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes
               , sbcTopLevels  = toplevelfunctions
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = ["LICENSE"]
               }

  simpleBuilder fficfg sbcfg
