{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( charpp
                                      , cppclass_
                                      , cstring, cstring_
                                      , int, int_
                                      , uint, uint_
                                      , void_
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Generate.Type.PackageInterface ( Namespace(..), HeaderName(..) )


modImports ::
     String
  -> [String]
  -> [HeaderName]
  -> (ModuleUnit,ModuleUnitImports)
modImports n ns hs =
  ( MU_Class n
  , ModuleUnitImports {
      muimports_namespaces = map NS ns
    , muimports_headers    = hs
    }
  )


cabal = Cabal {
    cabal_pkgname            = CabalName "hgdal"
  , cabal_version            = "0.1.0.0"
  , cabal_cheaderprefix      = "HGDAL"
  , cabal_moduleprefix       = "GDAL"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
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
    }

deletable :: Class
deletable =
  AbstractClass cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing ]
  []
  []

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

oGRFeature :: Class
oGRFeature =
  gdalclass "OGRFeature" [ deletable ]
  [ NonVirtual int_ "GetFieldCount" [] Nothing
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
  ]

oGRLayer :: Class
oGRLayer =
  gdalclass "OGRLayer" [ gDALMajorObject ]
  [ Virtual int_ {- GIntBit = 64 bit -} "GetFeatureCount" [ int "bForce" ] Nothing
  , Virtual (cppclass_ oGRFeatureDefn) "GetLayerDefn" [] Nothing
  , Virtual (cppclass_ oGRFeature) "GetNextFeature" [] Nothing
  , Virtual void_ "ResetReading" [] Nothing
  ]


classes =
  [ deletable
  , gDALDataset
  , gDALMajorObject
  , oGRFeature
  , oGRFeatureDefn
  , oGRFieldDefn
  , oGRGeometry
  , oGRLayer
  ]

toplevelfunctions :: [TopLevelFunction]
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
      , muimports_headers    = [ HdrName "gdal.h" ]
      }
    )
  , modImports "GDALMajorObject" [] ["gdal_priv.h"]
  , modImports "GDALDataset"     [] ["gdal_priv.h"]
  , modImports "OGRFeature"      [] ["ogr_feature.h"]
  , modImports "OGRFeatureDefn"  [] ["ogr_feature.h"]
  , modImports "OGRFieldDefn"    [] ["ogr_feature.h"]
  , modImports "OGRGeometry"     [] ["ogr_geometry.h"]
  , modImports "OGRLayer"        [] ["ogrsf_frmts.h"]
  ]

extraLib = []

extraDep = []


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let tmpldir = "../template"
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
