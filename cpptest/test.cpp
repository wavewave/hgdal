#include "ogrsf_frmts.h"
#include <iostream>

int main()
{
  GDALAllRegister();

  GDALDataset *poDS;
  poDS = (GDALDataset*) GDALOpenEx( "tl_2019_us_state.shp", GDAL_OF_VECTOR, NULL, NULL, NULL );

  if( poDS == NULL ) {
    printf("Open failed.\n");
    exit(1);
  }

  OGRLayer *poLayer;

  /*
  int x = poDS->GetLayerCount();
  std::cout << x << std::endl;
  poLayer = poDS->GetLayer(0);
  std::cout << "poLayer = " << poLayer->GetName() << std::endl;
  */

  poLayer = poDS->GetLayerByName("tl_2019_us_state");
  int count = 0;
  for( auto& poFeature: poLayer ) { count++; }
  std::cout << "count = " << count << std::endl;


  OGRFeature *poFeature;
  poLayer->ResetReading();
  while( (poFeature=poLayer->GetNextFeature()) != NULL ) {
    std::cout << "----------------------" << std::endl;
    OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();

    std::cout << "feature definition = " << poFDefn -> GetName() << std::endl;
    int n1 = poFeature->GetFieldCount();
    std::cout << "Feature-> field count = "    << n1 << std::endl;
    int n2 = poFDefn->GetFieldCount();
    std::cout << "FeatureDefn->field count = " << n2 << std::endl;
    int n3 = poFDefn->GetGeomFieldCount();
    std::cout << "FeatureDefn->geom field count = " << n3 << std::endl;


    for( int iField = 0; iField < poFDefn->GetFieldCount(); iField++) {
      OGRFieldDefn *poFieldDfn = poFDefn->GetFieldDefn( iField );
      //OGRFeature::FieldValue fv = (*poFeature)[iField];
      std::cout << "poFieldDfn = " << poFieldDfn << std::endl;
      std::cout << "poFieldDfn.GetName = " << poFieldDfn->GetNameRef() << std::endl;
      switch( poFieldDfn->GetType() ) {
      case OFTInteger:
	std::cout << "Integer value = " << poFeature->GetFieldAsInteger(iField) << std::endl;
      case OFTInteger64:
	std::cout << "Integer64 value = " << poFeature->GetFieldAsInteger64(iField) << std::endl;
      case OFTString:
	std::cout << "String value = " << poFeature->GetFieldAsString(iField) << std::endl;
        break;
      default:
	break;
      }
      //std::cout << "Field Value Type  = " << fv.GetType() << std::endl;

    }

    OGRGeometry *poGeometry;
    OGREnvelope* poEnv = new OGREnvelope;

    poGeometry = poFeature->GetGeometryRef();
    switch ( poGeometry -> getGeometryType() ) {
    case wkbPolygon: // 3
      std::cout << "polygon" << std::endl;
      OGRPolygon *poly;

      poly = poGeometry -> toPolygon();
      std::cout << poly->getGeometryName() << std::endl;
      std::cout << poly->hasCurveGeometry() << std::endl;
      std::cout << poly->IsRing() << std::endl;
      std::cout << poly->IsSimple() << std::endl;
      {
        OGRLinearRing *poRing = poly->getExteriorRing();
        OGRPointIterator *iter = poRing->getPointIterator();
        OGRPoint* pt = new OGRPoint;
        while( iter->getNextPoint(pt) ) {
	  double x = pt->getX();
	  double y = pt->getY();
	  std::cout << " x = " << x << ", y = " << y << std::endl;
	}
      }
      poly->getEnvelope(poEnv);
      std::cout << "MinX = " << poEnv->MinX << ", "
		<< "MaxX = " << poEnv->MaxX << ", "
		<< "MinY = " << poEnv->MinY << ", "
		<< "MaxY = " << poEnv->MaxY << std::endl;
      break;
    case wkbMultiPolygon: // 6
      std::cout << "multi-polygon" << std::endl;
      break;
    default:
      ;
    }

  }


}
