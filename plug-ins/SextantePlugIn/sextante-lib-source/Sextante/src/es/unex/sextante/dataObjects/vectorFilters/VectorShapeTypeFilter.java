package es.unex.sextante.dataObjects.vectorFilters;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IVectorLayer;

public class VectorShapeTypeFilter
         implements
            IVectorLayerFilter {

   private final int m_iShapeType;


   /**
    * Creates a new filter
    * 
    * @param iShapeType
    *                the type of shape that passes the filter. Use the constants defined in IVectorLayer
    */
   public VectorShapeTypeFilter(final int iShapeType) {

      m_iShapeType = iShapeType;

   }


   public int getShapeType() {

      return m_iShapeType;

   }


   @Override
   public boolean accept(final IFeature feature,
                         final int index) {

      final Geometry geom = feature.getGeometry();
      switch (m_iShapeType) {
         case IVectorLayer.SHAPE_TYPE_LINE:
            return (geom instanceof MultiLineString) || (geom instanceof LineString);
         case IVectorLayer.SHAPE_TYPE_POINT:
            return (geom instanceof Point) || (geom instanceof MultiPoint);
         case IVectorLayer.SHAPE_TYPE_POLYGON:
            return (geom instanceof Polygon) || (geom instanceof MultiPolygon);
         default:
            return false;
      }


   }

}
