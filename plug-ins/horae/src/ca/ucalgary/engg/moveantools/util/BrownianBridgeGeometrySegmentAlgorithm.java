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
import es.unex.sextante.rasterWrappers.GridCell;

public class BrownianBridgeGeometrySegmentAlgorithm extends GeoAlgorithm {

   public static final String  LINELAYER   = "LINELAYER";
   public static final String  IDFIELD   = "IDFIELD";
   //public static final String  BANDWIDTH   = "BANDWIDTH";
   public static final String  RESULT  = "RESULT";
   public static final String  TEMPRESULT  = "TEMPRESULT";
   public static final String  SIGMA1MOBIL   = "SIGMA1MOBIL";
   public static final String  SIGMA2LOCERROR   = "SIGMA2LOCERROR";
   public static final String  EQUALDELTATIME   = "EQUALDELTATIME";
   public static final String  TIMEDIFFFIELD   = "TIMEDIFFFIELD";
   public static final String  TEMPMERGEABRESULT  = "TEMPMERGEABRESULT";
   public static final String  TEMPLASTRESULT  = "TEMPLASTRESULT";

   private int                 m_iField;
   private int                 m_timeDiffField;
   private IVectorLayer        m_Layer;
   private IRasterLayer        m_Result;
   private IRasterLayer        m_tempOneLineRaster;
   private IRasterLayer        m_tempMergeLineRasterAB;
   private IRasterLayer        m_tempLastOneLineRaster;
   private AnalysisExtent          m_Extent;
   private double 			   m_alpha[];
   private int 				   m_IntegrationSteps = 25;
   private double 			   m_sigma1;
   private double 			   m_sigma2;
   private boolean			   m_bDTOne; 
   private double 			   m_maxVarDist;


   public void defineCharacteristics() {

      setName("Brownian Bridge");
      setGroup("Rasterization_and_interpolation");
      setUserCanDefineAnalysisExtent(true);

      try {
         m_Parameters.addInputVectorLayer(LINELAYER, "Vector line layer", AdditionalInfoVectorLayer.SHAPE_TYPE_LINE, true);
         m_Parameters.addTableField(IDFIELD, "idField", LINELAYER);
         m_Parameters.addTableField(TIMEDIFFFIELD, "timeDifferenceField", LINELAYER);
         m_Parameters.addNumericalValue(SIGMA1MOBIL, "sigma 1 [in m] - related to mobility/speed", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
										70, 0, Double.MAX_VALUE);
         m_Parameters.addNumericalValue(SIGMA2LOCERROR, "sigma 2 [in m] - related to location error", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
					30, 0, Double.MAX_VALUE);
         m_Parameters.addBoolean(EQUALDELTATIME, "equal dT=1 between points", true);
         addOutputRasterLayer(RESULT, "Result");
         addOutputRasterLayer(TEMPRESULT, "Result of last feature processed");
         addOutputRasterLayer(TEMPMERGEABRESULT, "Merge of last two features processed");
         addOutputRasterLayer(TEMPLASTRESULT, "Result of last but one features processed");
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
      double lineID;
      double deltaT = 0;

      m_Layer = m_Parameters.getParameterValueAsVectorLayer(LINELAYER);
      m_iField = m_Parameters.getParameterValueAsInt(IDFIELD);
      m_timeDiffField = m_Parameters.getParameterValueAsInt(TIMEDIFFFIELD);
      //m_dBandwidth = m_Parameters.getParameterValueAsDouble(BANDWIDTH);
      m_sigma1 = m_Parameters.getParameterValueAsDouble(SIGMA1MOBIL);
      m_sigma2 = m_Parameters.getParameterValueAsDouble(SIGMA2LOCERROR);
      m_bDTOne = m_Parameters.getParameterValueAsBoolean(EQUALDELTATIME);

      m_Result = getNewRasterLayer(RESULT, m_Layer.getName() + "-BBSegm",
               						IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_Result.assign(0.0);
      //m_Result.setNoDataValue(0.0);
      
      //-- we need this temporary raster, since calculations for one segment/line
      //  will be a bit different when assigning cell values
      //  However, it may be better to operate with an double array as done for the weights?
      m_tempOneLineRaster = getNewRasterLayer(TEMPRESULT, m_Layer.getName(),
					IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempOneLineRaster.assign(0.0);
      //m_tempOneLineRaster.setNoDataValue(0.0);
      
      m_tempMergeLineRasterAB = getNewRasterLayer(TEMPMERGEABRESULT, m_Layer.getName(),
				IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempMergeLineRasterAB.assign(0.0);

      m_tempLastOneLineRaster = getNewRasterLayer(TEMPLASTRESULT, m_Layer.getName(),
				IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempLastOneLineRaster.assign(0.0);
      
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
      //-- to have an extent for the calculations use the variance_max
	  double dTmax = 10000; // use 10000.0 instead of 1.0 for numerical reasons
	  if (m_bDTOne){
		  dTmax = 10000; // use 10000.0 instead of 1.0 for numerical reasons
	  }
	  else{
		  dTmax = 0;
		  IFeatureIterator iterTp = m_Layer.iterator();
	      int iShapeCountT = m_Layer.getShapesCount();
		  while (iterTp.hasNext() && setProgress(i, iShapeCountT)) {
	    	  IFeature feature = iterTp.next();
	    	  IRecord record = feature.getRecord();
	    	  try {
	    		  deltaT = Double.parseDouble(record.getValue(m_timeDiffField).toString());
	    	  }
	    	  catch (Exception e) {
	    		  m_bDTOne = true; //the field may not be there, so we use dT=1;
	    		  deltaT = 10000.0;
	    	  }
			  iShape++;
			  if(deltaT > dTmax){
				  dTmax = deltaT;
			  }
		  }
	  }
	  m_maxVarDist = BrownianBridgeUtil.calcMaxVarDist(m_sigma1, m_sigma2, dTmax);
	  //m_maxVarDist = BrownianBridgeUtil.maxh(m_sigma1, m_sigma2, 25, dTmax);
      //-- iterate over all input features 
      i = 0;
      iShapeCount = m_Layer.getShapesCount();
      IFeatureIterator iter = m_Layer.iterator();
      double lastIDValue = -1; // this variable is used to see if the lines
      						   // are consecutive to avoid double counts instead we take the max
      while (iter.hasNext() && setProgress(i, iShapeCount)) {
    	  IFeature feature = iter.next();
    	  IRecord record = feature.getRecord();
    	  try {
    		  lineID = Double.parseDouble(record.getValue(m_iField).toString());
    	  }
    	  catch (Exception e) {
    		  lineID = (double) iShape;
    	  }
    	  try {
    		  deltaT = Double.parseDouble(record.getValue(m_timeDiffField).toString());
    	  }
    	  catch (Exception e) {
    		  m_bDTOne = true; //the field may not be there, so we use dT=1;
    	  }
		  iShape++;
		  
    	  Geometry geometry = feature.getGeometry();

    	  if (geometry.intersects(extent)) {
    		  //System.out.println("geometry " + i + " of " + iShapeCount);
    		  //-- clear the tempGrid
			  //-- create a new TempGrid
			  m_tempOneLineRaster = getNewRasterLayer(TEMPRESULT, m_Layer.getName(),
					  IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
			  m_tempOneLineRaster.assign(0.0);
			  m_tempMergeLineRasterAB.assign(0.0);
    		  //-- fill the tempGrid of this line
    		  doLine(geometry, deltaT);
    		  //-- standardization of volume
    		  double sumVals = calculateSumOfRasterValuesFirstBand(m_tempOneLineRaster); //=volume
    		  if (sumVals != 0){
	    		  double scFactor = 1.0 / (sumVals * Math.pow(m_Result.getWindowCellSize(),2));
	    		  m_tempOneLineRaster.multiply(scFactor);
    		  }
    		  else{
    			  System.out.println("BrownianBridgeGeometrySegmentAlgorithm: got zero raster (appears for linelength=0! - shapeID: " + i);
    		  }
    		  //double sumValsAfter = calculateSumOfRasterValuesFirstBand(m_tempOneLineRaster); //=volume
    		  //-- add the tempGrid to the result grid
    		  if(lineID == (lastIDValue+1)){
				  for (int x = 0; x < m_Result.getWindowGridExtent().getNX(); x++){
					  for (int y = 0; y < m_Result.getWindowGridExtent().getNY(); y++){
						  double dValueA = m_tempLastOneLineRaster.getCellValueAsDouble(x, y);
						  double dValueB = m_tempOneLineRaster.getCellValueAsDouble(x, y);
						  double dValue = 0.0;
						  if((dValueA> 0) && (dValueB > 0)){
							  //-- for the common area of the kernelFunction grid of 
							  //   line A and grid line B
							  if(dValueA >= dValueB){
								  dValue = dValueA;
							  }
							  else{
								  dValue = dValueB;
							  }
							  //-- and subtract dValueA from the ResultGrid to undo adding of 
							  //   dValueA in the previous step
							  double dValueOld = m_Result.getCellValueAsDouble(x, y);
							  m_Result.setCellValue(x, y, dValueOld - dValueA);
						  }
						  else{
							  //-- for the rest take the values from B (the new grid) only
							  dValue = dValueB;
						  }    						  
						  m_tempMergeLineRasterAB.setCellValue(x, y, dValue);
					  }
				  }
    		  }
    		  else{
    			  //-- if the lines are not subsequent, then we just add all values 
    			  m_Result.add(m_tempOneLineRaster);
    		  }
			  //-- add the values to the result grid
			  m_Result.add(m_tempMergeLineRasterAB);
			  //-- to check evolution
			  double sumValsRes = calculateSumOfRasterValuesFirstBand(m_Result); //=volume
			  System.out.println("BBridgeGeomSegmAlgo - SumRasterValue (" + i + "/" + iShapeCount + "): " +  sumValsRes);
			  //-- now assign the new temp Grid to the last
			  m_tempLastOneLineRaster = m_tempOneLineRaster;
    	  }//if geometry intersects
    	  i++;
    	  lastIDValue = lineID;
      }
      iter.close();

      return !m_Task.isCanceled();
   }

   private void doLine(Geometry geom, double timeDiff) {
	   for (int i = 0; i < geom.getNumGeometries(); i++) {
		   Geometry part = geom.getGeometryN(i);
		   doLineString(part, timeDiff);
	   }
   }


   private void doLineString(Geometry geom, double timeDiff) {

	   int i;
	   double x, y, x2, y2;
	   Coordinate[] coords = geom.getCoordinates();
	   for (i = 0; i < coords.length - 1; i++) {
		   x = coords[i].x;
		   y = coords[i].y;
		   x2 = coords[i + 1].x;
		   y2 = coords[i + 1].y;
		   writeSegment(x, y, x2, y2, timeDiff);
	   }
   }


   private void writeSegment(double x, double y, double x2, double y2, double timeDiff) {
	   // get the segment dimensions and iterate over them
	   
	   //-- the base code is from the Sextante line rasterization algorithm
	   double dx, dy;
	   //GridCell cell;
	   double xtemp = x;
	   double ytemp = y;
	   dx = Math.abs(x2 - x);
	   dy = Math.abs(y2 - y);
	   
	   double xMax = 0;
	   double xMin = 0;
	   double yMin = 0;
	   double yMax = 0;
	   double extend = 0;
	   /*
	   if (dx > dy){
		   extend = 1.0*dx;
	   }
	   else{
		   extend = 1.0*dy;
	   }
	   */
	   if(m_bDTOne){
		   extend = m_maxVarDist;
	   }
	   else{
		   extend = BrownianBridgeUtil.calcMaxVarDist(m_sigma1, m_sigma2, timeDiff);
		   //extend = BrownianBridgeUtil.maxh(m_sigma1, m_sigma2, 25, timeDiff);
	   }
	   double valueToRaise =  m_Result.getWindowCellSize();
	   //-- fill, but first get the step size for dx/dy
	   if (dx > 0.0 || dy > 0.0) {
		   if (x2 < x) {
			   xMax = x + extend;
			   xMin = x2 - extend;
		   }
		   else{
			   xMax = x2 + extend;
			   xMin = x - extend;
		   }

		   if (y2 < y) {
			   yMax = y + extend;
			   yMin = y2 - extend;
		   } 
		   else{
			   yMax = y2 + extend;
			   yMin = y - extend;
		   }
		   xtemp = xMin;
		   while (xtemp <= xMax) {
			   ytemp = yMin; // reset
			   while (ytemp <= yMax){
				   if (m_Extent.contains(xtemp, ytemp)) {
					   doPointBB(xtemp,ytemp, x, y, x2, y2, timeDiff);
				   }
				   ytemp += valueToRaise;
			   }
			   xtemp += valueToRaise; 
		   }
	   }//end if dx,dy>0  
   }
	
	private void doPointBB (double xtemp, double ytemp, double p1x, double p1y, 
			double p2x, double p2y, double timeDiff){
		int iX, iY;
		GridCell cell;

		cell = m_Extent.getGridCoordsFromWorldCoords(xtemp, ytemp);
		iX = cell.getX();
		iY = cell.getY();
		
		double oldValue = m_tempOneLineRaster.getCellValueAsDouble(iX, iY);
		//TODO: calculation of proper weight and dt
		double weight = 1; //assuming unit values for now
		double dt = 1*10000; // we multiply with 10'000 as we did for the liker function to avoid numerical problems.
		if (this.m_bDTOne == false){
			dt = timeDiff;
			//double dt = T[indcons[i]+1] - T[indcons[i]];
		}
	    //dttot = T[nlocations] - T[1];
		//weight = dt / dttot;
		double[] XG = new double[3]; XG[0] = 0; XG[1]= xtemp; XG[2] = ytemp;
		double[] X1 = new double[3]; X1[0] = 0; X1[1]= p1x; X1[2] = p1y;
		double[] X2 = new double[3]; X2[0] = 0; X2[1]= p2x; X2[2] = p2y;
		
		double sigma1 = m_sigma1 * m_sigma1;
		double sigma2 = m_sigma2 * m_sigma2;
		double integralVal = BrownianBridgeUtil.integrno(XG,  X1, X2, dt, sigma1, sigma2, m_alpha);
		double newValue = oldValue + weight * integralVal;
		/*// should not appear anymore?
		if(Double.isNaN(integralVal)){
			System.out.println("value is NaN");
		}
		*/
		m_tempOneLineRaster.setCellValue(iX, iY, newValue);
	}
	
	
	private double calculateSumOfRasterValuesFirstBand(IRasterLayer raster){

		double z;
		int iValues;

		double sum = 0;
		iValues	= 0;
		for (int y = 0; y < raster.getNY(); y++){
			for (int x = 0; x < raster.getNX(); x++){
				z = raster.getCellValueAsDouble(x,y,0);
				if( z > 0){
					sum	+= z;
					iValues++;
				}
			}
		}
		return sum;
	}
}
