/*******************************************************************************
BrownianBridgeAlgorithm.java
Copyright (C) Stefan Steiniger

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

import java.awt.geom.Point2D;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
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

public class BrownianBridgeRasterAlgorithm extends GeoAlgorithm {

   public static final String  POINTLAYER   = "POINTLAYER";
   //public static final String  BANDWIDTH   = "BANDWIDTH";
   public static final String  RESULT  = "RESULT";
   public static final String  SIGMA1MOBIL   = "SIGMA1MOBIL";
   public static final String  SIGMA2LOCERROR   = "SIGMA2LOCERROR";
   public static final String  EQUALDELTATIME   = "EQUALDELTATIME";
   public static final String  TIMEFIELD   = "TIMEFIELD";

   //private double 			   m_dBandwidth;
   private IVectorLayer        m_Layer;
   private IRasterLayer        m_Result;
   private AnalysisExtent          m_Extent;
   private double 			   m_alpha[];
   private int 				   m_IntegrationSteps = 25;
   private double 			   m_sigma1;
   private double 			   m_sigma2;
   private int                 m_timeField;
   private boolean			   m_bDTOne; 


   public void defineCharacteristics() {

      setName("Brownian Bridge");
      setGroup("Rasterization_and_interpolation");
      setUserCanDefineAnalysisExtent(true);

      try {
         m_Parameters.addInputVectorLayer(POINTLAYER, "Ordered point layer", AdditionalInfoVectorLayer.SHAPE_TYPE_POINT, true);
         m_Parameters.addTableField(TIMEFIELD, "timeField - with time in secs", POINTLAYER);
         m_Parameters.addNumericalValue(SIGMA1MOBIL, "sigma 1 [in m] - related to mobility/speed", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
										70, 0, Double.MAX_VALUE);
         m_Parameters.addNumericalValue(SIGMA2LOCERROR, "sigma 2 [in m] - related to location error", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
					30, 0, Double.MAX_VALUE);
         m_Parameters.addBoolean(EQUALDELTATIME, "equal dT=1 between points", true);
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
	   int iShapeCount;

	   m_Layer = m_Parameters.getParameterValueAsVectorLayer(POINTLAYER);
	   //m_dBandwidth = m_Parameters.getParameterValueAsDouble(BANDWIDTH);
	   m_sigma1 = m_Parameters.getParameterValueAsDouble(SIGMA1MOBIL);
	   m_sigma2 = m_Parameters.getParameterValueAsDouble(SIGMA2LOCERROR);
	   m_bDTOne = m_Parameters.getParameterValueAsBoolean(EQUALDELTATIME);
	   m_timeField = m_Parameters.getParameterValueAsInt(TIMEFIELD);

	   m_Result = getNewRasterLayer(RESULT, m_Layer.getName() + "-BBR",
			   IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
	   m_Result.assign(0.0);
	   //m_Result.setNoDataValue(0.0);


	   m_Extent = m_Result.getWindowGridExtent();

	   //-- get the extend of the input
	   Coordinate[] coords = new Coordinate[5];
	   coords[0] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMin());
	   coords[1] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMax());
	   coords[2] = new Coordinate(m_Extent.getXMax(), m_Extent.getYMax());
	   coords[3] = new Coordinate(m_Extent.getXMax(), m_Extent.getYMin());
	   coords[4] = new Coordinate(m_Extent.getXMin(), m_Extent.getYMin());
	   GeometryFactory gf = new GeometryFactory();
	   LinearRing ring = gf.createLinearRing(coords);
	   Polygon extent = gf.createPolygon(ring, null);


	   //-- Build the vector alpha
	   m_alpha = new double[m_IntegrationSteps+1];
	   m_alpha[0] = 0;
	   for (i = 1; i <= m_IntegrationSteps; i++) {
		   m_alpha[i] = ((double) i) / ((double)m_IntegrationSteps);
	   }

	   //-- iterate over all input features and store their locations
	   iShapeCount = m_Layer.getShapesCount();
	   IFeatureIterator iter = m_Layer.iterator();
	   double[][] xyloc = new double[iShapeCount+1][3];
	   double[] time = new double[iShapeCount+1];
	   int[] indcons = new int[iShapeCount+1];
	   i = 1;
	   while (iter.hasNext() && setProgress(i, iShapeCount)) {
		   IFeature feature = iter.next();
		   Geometry geometry = feature.getGeometry();   	  
		   if (geometry.intersects(extent)) {
			   //System.out.println("geometry " + i + " of " + iShapeCount);
			   // note, the inout for the adehabitat algorithm start not with 0
			   xyloc[i][1] = geometry.getCoordinate().x;
			   xyloc[i][2] = geometry.getCoordinate().y;
			   if(m_bDTOne){
				   time[i] = (double)i*10000; // we multiply with 10'000 as we did for the liker function to avoid numerical problems.
			   }
			   else{
				   try {
					   IRecord record = feature.getRecord();
					   double ti = Double.parseDouble(record.getValue(m_timeField).toString());
					   time[i] = ti;
				   }
				   catch (Exception e) {
					   m_bDTOne = true; //the field may not be there, so we use dT=1;
					   time[i] = (double)i*10000; // we multiply with 10'000 as we did for the liker function to avoid numerical problems.
				   }
			   }
			   indcons[i] = i;
		   }//if geometry intersects
		   i++;
	   }
	   iter.close();
	   double sigma1 = m_sigma1 * m_sigma1;
	   double sigma2 = m_sigma2 * m_sigma2;
	   double vol = 0;
	   //-- now iterate over all cells
	   for (int y = 0; y < m_Result.getNY(); y++){
		   System.out.println("BrownianBridgeRasterAlgorithm : processing raster line: " + y + "/" + m_Result.getNY() );
		   for (int x = 0; x < m_Result.getNX(); x++){

			   Point2D pt = m_Extent.getWorldCoordsFromGridCoords(x, y);
			   double xCoord = pt.getX();
			   double yCoord = pt.getY();	
			   double[] XG = new double[3]; XG[0] = 0; XG[1]= xCoord; XG[2] = yCoord;
			   int ncons = iShapeCount-1;
			   //-- not sure why this loop would be so - leave it
			   //for (int k = 1; k < iShapeCount; k++){
				   //indcons[k]=k;
				   double dValue = BrownianBridgeUtil.udbbnoeud(XG, xyloc, time, sigma1, 
						   sigma2, m_alpha, ncons, indcons);
				   if(Double.isNaN(dValue) || Double.isInfinite(dValue)){
					   //-- some values resulted from either dt=0 or T=0, and we calculated dt/T
					   System.out.println("BrownianBridgeRasterAlgorithm : NaN or Infinity : need to debug here : cell (y,x): " + y + "," + x );
					   boolean isNan = Double.isNaN(dValue);
					   boolean isInf = Double.isInfinite(dValue);
				   }
				   else{
					   m_Result.setCellValue(x, y, dValue);
					   vol+=dValue;
				   }
			   //}
				  //System.out.print(".");
		   }
		   //System.out.println("+");
	   }
	   //-- standardization of volume
	   if (vol != 0){ // avoid division by zero
		   double scFactor = 1.0 / (vol * Math.pow(m_Result.getWindowCellSize(),2));
		   m_Result.multiply(scFactor);
	   }
	   return !m_Task.isCanceled();
   }
		
}
