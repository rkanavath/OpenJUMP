/*******************************************************************************
LineBufferDensityAlgorithm based on RasterizeVectorLayerAlgorithm.java
Copyright (C) Stefan Steiniger

Adapted from SAGA, System for Automated Geographical Analysis.
Copyrights (c) 2002-2005 by Olaf Conrad
Portions (c) 2002 by Andre Ringeler
Portions (c) 2005 by Victor Olaya

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *******************************************************************************/
package ca.ucalgary.engg.moveantools.util;

import java.util.Arrays;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IFeatureIterator;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IRecord;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.OptionalParentParameterException;
import es.unex.sextante.exceptions.RepeatedParameterNameException;
import es.unex.sextante.exceptions.UndefinedParentParameterNameException;

public class LineBufferDensityAlgorithm extends GeoAlgorithm {

   public static final String  LAYER   = "LAYER";
   public static final String  IDFIELD   = "IDFIELD";
   public static final String  RESULT  = "RESULT";

   private int                 m_iField;
   private int                 m_iNX, m_iNY;
   private IVectorLayer        m_Layer;
   private IRasterLayer        m_Result;
   private AnalysisExtent          m_Extent;


   public void defineCharacteristics() {

      setName("Rasterize polygon/buffer layer");
      setGroup("Rasterization_and_interpolation");
      setUserCanDefineAnalysisExtent(true);

      try {
         m_Parameters.addInputVectorLayer(LAYER, "Vector polygon layer", AdditionalInfoVectorLayer.SHAPE_TYPE_POLYGON, true);
         m_Parameters.addTableField(IDFIELD, "idField", LAYER);
         addOutputRasterLayer(RESULT, "Result");
      }
      catch (UndefinedParentParameterNameException e) {
         Sextante.addErrorToLog(e);
      }
      catch (OptionalParentParameterException e) {
         Sextante.addErrorToLog(e);
      }
      catch (RepeatedParameterNameException e) {
         Sextante.addErrorToLog(e);
      }

   }


   public boolean processAlgorithm() throws GeoAlgorithmExecutionException {

      int i;
      int iShape = 1;
      int iShapeCount;
      double bufferID;

      m_Layer = m_Parameters.getParameterValueAsVectorLayer(LAYER);
      m_iField = m_Parameters.getParameterValueAsInt(IDFIELD);

      m_Result = getNewRasterLayer(RESULT, m_Layer.getName(),
               IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_Result.assign(0.0);

      m_Extent = m_Result.getWindowGridExtent();

      m_iNX = m_Extent.getNX();
      m_iNY = m_Extent.getNY();

      Coordinate[] coords = new Coordinate[5];
      coords[0] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMin());
      coords[1] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMax());
      coords[2] = new Coordinate(m_Extent.getXMax(), m_Extent.getYMax());
      coords[3] = new Coordinate(m_Extent.getXMax(), m_Extent.getYMin());
      coords[4] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMin());
      GeometryFactory gf = new GeometryFactory();
      LinearRing ring = gf.createLinearRing(coords);
      Polygon extent = gf.createPolygon(ring, null);

      i = 0;
      //iType = m_Layer.getShapeType();
      iShapeCount = m_Layer.getShapesCount();
      IFeatureIterator iter = m_Layer.iterator();
      double lastIDValue = -1; // this variable is used to see if the buffer
      						   // are consecutive to avoid double counts
      while (iter.hasNext() && setProgress(i, iShapeCount)) {
         IFeature feature = iter.next();
         IRecord record = feature.getRecord();
         try {
            bufferID = Double.parseDouble(record.getValue(m_iField).toString());
            iShape++;
         }
         catch (Exception e) {
        	 bufferID = (double) iShape;
            iShape++;
         }

         Geometry geometry = feature.getGeometry();

         if (geometry.intersects(extent)) {
             //System.out.println("polygon " + i + " of " + iShapeCount);
             doPolygon(geometry, bufferID, lastIDValue);
         }
         i++;
         lastIDValue = bufferID;
      }
      iter.close();

      return !m_Task.isCanceled();


   }


   private void doPolygon(Geometry geom, double bufferID, double lastBufferID) {

      GeometryFactory gf = new GeometryFactory();
      for (int i = 0; i < geom.getNumGeometries(); i++) {
         Polygon poly = (Polygon) geom.getGeometryN(i);
         LinearRing lr = gf.createLinearRing(poly.getExteriorRing().getCoordinates());
         Polygon part = gf.createPolygon(lr, null);
         doPolygonPart(part, bufferID, lastBufferID, false);
         for (int j = 0; j < poly.getNumInteriorRing(); j++) {
            lr = gf.createLinearRing(poly.getInteriorRingN(j).getCoordinates());
            part = gf.createPolygon(lr, null);
            doPolygonPart(part, bufferID, lastBufferID, true);
         }
      }

   }


   private void doPolygonPart(Polygon geom, double bufferID, double lastBufferID,
                              boolean bIsHole) {

      boolean bFill;
      boolean bCrossing[];
      int x, y, ix, xStart, xStop, iPoint;
      double yPos;;
      Coordinate pLeft, pRight, pa, pb, p = new Coordinate();
      bCrossing = new boolean[m_iNX];

      Envelope extent = (Envelope) geom.getEnvelopeInternal();

      xStart = (int) ((extent.getMinX() - m_Extent.getXMin()) / m_Extent.getCellSize()) - 1;
      if (xStart < 0) {
         xStart = 0;
      }

      xStop = (int) ((extent.getMaxX() - m_Extent.getXMin()) / m_Extent.getCellSize()) + 1;
      if (xStop >= m_iNX) {
         xStop = m_iNX - 1;
      }

      Coordinate[] points = geom.getCoordinates();

      for (y = 0, yPos = m_Extent.getYMax(); y < m_iNY; y++, yPos -= m_Extent.getCellSize()) {
         if (yPos >= extent.getMinY() && yPos <= extent.getMaxY()) {
            Arrays.fill(bCrossing, false);
            pLeft = new Coordinate(m_Extent.getXMin() - 1.0, yPos);
            pRight = new Coordinate(m_Extent.getXMax() + 1.0, yPos);

            pb = points[points.length - 1];

            for (iPoint = 0; iPoint < points.length; iPoint++) {
               pa = pb;
               pb = points[iPoint];

               if (((pa.y <= yPos && yPos < pb.y) || (pa.y > yPos && yPos >= pb.y))) {
                  getCrossing(p, pa, pb, pLeft, pRight);

                  ix = (int) ((p.x - m_Extent.getXMin()) / m_Extent.getCellSize() + 1.0);

                  if (ix < 0) {
                     ix = 0;
                  }
                  else if (ix >= m_iNX) {
                     ix = m_iNX - 1;
                  }

                  bCrossing[ix] = !bCrossing[ix];
               }
            }

            for (x = xStart, bFill = false; x <= xStop; x++) {
               if (bCrossing[x]) {
                  bFill = !bFill;
               }
               if (bFill) {
                  double dPrevValue = m_Result.getCellValueAsDouble(x, y);
                  if (bIsHole) {
                	 /*
                	  //-- not sure how we handle a hole situation
                	  //   as we store now counts
                      if (dPrevValue == bufferID) {
                        	m_Result.setNoData(x, y);
                      }
                      */
                	  //-- maybe we just substract
                      if (dPrevValue == 1) {
                    	  m_Result.setCellValue(x, y, 0.0);
                      }
                      else if(dPrevValue > 1){
                    	  m_Result.setCellValue(x, y, dPrevValue-1);
                      }
                  }
                  else {
                     if (dPrevValue == 0) {
                        m_Result.setCellValue(x, y, 1);
                     }
                     else{
                    	 //-- check if this polygon/buffer follows the first one
                    	 //   if so, we do not want to increase the count
                    	 //   if different, we increase the count
                    	 if(bufferID != lastBufferID){
                    		 m_Result.setCellValue(x, y, dPrevValue + 1);
                    	 }
                    	 // otherwise we do nothing (i.e. leave the value)
                     }
                  }
               }
            }
         }
      }

   }

   private boolean getCrossing(Coordinate crossing,
                               Coordinate a1,
                               Coordinate a2,
                               Coordinate b1,
                               Coordinate b2) {

      double lambda, div, a_dx, a_dy, b_dx, b_dy;

      a_dx = a2.x - a1.x;
      a_dy = a2.y - a1.y;

      b_dx = b2.x - b1.x;
      b_dy = b2.y - b1.y;

      if ((div = a_dx * b_dy - b_dx * a_dy) != 0.0) {
         lambda = ((b1.x - a1.x) * b_dy - b_dx * (b1.y - a1.y)) / div;

         crossing.x = a1.x + lambda * a_dx;
         crossing.y = a1.y + lambda * a_dy;

         return true;

      }

      return false;
   }

}
