/*******************************************************************************
ScaledLineKernelDensityForMovementTracksAlgorithm.java
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

public class ScaledLineKernelDensityForMovementTracksAlgorithm extends GeoAlgorithm {

   public static final String  LINELAYER   = "LINELAYER";
   public static final String  IDFIELD   = "IDFIELD";
   public static final String  BANDWIDTH   = "BANDWIDTH";
   public static final String  RESULT  = "RESULT";
   public static final String  TEMPRESULT  = "TEMPRESULT";
   public static final String  TEMPLASTRESULT  = "TEMPLASTRESULT";
   public static final String  TEMPMERGEABRESULT  = "TEMPMERGEABRESULT";
   public static final String  RASTERIZEFIRST  = "RASTERIZEFIRST";
   public static final String  SCALEWITHSQRT  = "SCALEWITHSQRT";

   private int                 m_iField;
   private int				   m_iDistance;
   private double 			   m_dBandwidth;
   private IVectorLayer        m_Layer;
   private IRasterLayer        m_Result;
   private IRasterLayer        m_tempOneLineRaster;
   private IRasterLayer        m_tempLastOneLineRaster;
   private IRasterLayer        m_tempMergeLineRasterAB;
   private AnalysisExtent          m_Extent;
   private double 			   m_dWeight[][];
   private boolean 			   m_bRasterizeFirst;
   private boolean			   m_bscaleWithSqrt;


   public void defineCharacteristics() {

      setName("Line KD for Movement Tracks");
      setGroup("Rasterization_and_interpolation");
      setUserCanDefineAnalysisExtent(true);

      try {
         m_Parameters.addInputVectorLayer(LINELAYER, "Vector line layer", AdditionalInfoVectorLayer.SHAPE_TYPE_LINE, true);
         m_Parameters.addTableField(IDFIELD, "idField", LINELAYER);
         m_Parameters.addNumericalValue(BANDWIDTH, "Bandwidth [in m]", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
										100, 0, Double.MAX_VALUE);
         m_Parameters.addBoolean(SCALEWITHSQRT, "Kernel scaling with sqrt(0.5) resulting in range [1...0.707]", true);
         //m_Parameters.addBoolean(RASTERIZEFIRST, "rasterize lines first", true);
         addOutputRasterLayer(RESULT, "Result");
         addOutputRasterLayer(TEMPRESULT, "Result of last feature processed");
         addOutputRasterLayer(TEMPLASTRESULT, "Result of last but one features processed");
         addOutputRasterLayer(TEMPMERGEABRESULT, "Merge of last two features processed");
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

      m_Layer = m_Parameters.getParameterValueAsVectorLayer(LINELAYER);
      m_iField = m_Parameters.getParameterValueAsInt(IDFIELD);
      m_dBandwidth = m_Parameters.getParameterValueAsDouble(BANDWIDTH);
      m_bscaleWithSqrt = m_Parameters.getParameterValueAsBoolean(SCALEWITHSQRT);
      //====================
      //
      // TODO: sstein - July 16th 2010.
      //        I have implemented the weighted rasterization, but not tested yet.
      //		In theory the weight should allow to scale the kernel along the 
      //        line too. However I have not looked at the case when two lines cross
      //        (e.g. by testing if the cell has already a value). Here the weight 
      //        needs to be set somehow. Taking a mean or max value is 
      //        inappropriate for crossings as they should add-up there. Adding the 
      //        values would lead to unwanted results for the endpoints
      //        of segments - when two segments touch each other 
      //
      //-- not used so far  
      //m_bRasterizeFirst = m_Parameters.getParameterValueAsBoolean(RASTERIZEFIRST);
      m_bRasterizeFirst = false;
      //====================
      
      m_Result = getNewRasterLayer(RESULT, m_Layer.getName() + "-SKD4M",
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
      
      m_tempLastOneLineRaster = getNewRasterLayer(TEMPLASTRESULT, m_Layer.getName(),
				IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempLastOneLineRaster.assign(0.0);
      //m_tempLastOneLineRaster.setNoDataValue(0.0);
      
      m_tempMergeLineRasterAB = getNewRasterLayer(TEMPMERGEABRESULT, m_Layer.getName(),
				IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempMergeLineRasterAB.assign(0.0);
      //m_tempMergeLineRasterAB.setNoDataValue(0.0);
      
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
      
      //-- create the weight matrix
      m_iDistance = (int) Math.floor(m_dBandwidth / m_Extent.getCellSize());

      m_dWeight = new double [2 * m_iDistance + 1][2 * m_iDistance + 1];

      double scalingFactor = 0.0; // the sum of weight values of a kernel cross section
      for(int y = -m_iDistance; y < m_iDistance + 1; y++){
    	  for(int x = -m_iDistance; x < m_iDistance + 1; x++){
    		  double dDist = Math.sqrt(x*x + y*y);
    		  //-- u calculated with cell units 
    		  //   u = [0..1]
    		  //double u = dDist / (double)m_iDistance; // to implement 1D kernels [it may be that x'x is different from that]
    		  //-- u calculated with real units
    		  //double u = dDist*m_Extent.getCellSize() / dDistance;
    		  if (dDist < m_iDistance){
    			  /** 
    			   * Standard Sextante Kernel (Gaussian according to V. Olaya)
    			   * this one looks as it should with the profile graph tool,
    			   * also weight_max_value = 1.0
    			   * Note - this is the biweight kernel without scaling factor
    			   */
    			  m_dWeight[x + m_iDistance][y + m_iDistance] =
    				  Math.pow(1 - (dDist * dDist) / (m_iDistance * m_iDistance), 2);
    		  }
    		  else{
    			  m_dWeight[x + m_iDistance][y + m_iDistance] = 0;
    		  }
    		  if(y == 0){
    			  scalingFactor = scalingFactor + m_dWeight[x + m_iDistance][y + m_iDistance];
    		  }
    	  }
      }
		
      //-- iterate over all input features 
      i = 0;
      iShapeCount = m_Layer.getShapesCount();
      IFeatureIterator iter = m_Layer.iterator();
      double lastIDValue = -9999; // this variable is used to see if the lines
      						   // are consecutive to avoid double counts instead we take the max
      while (iter.hasNext() && setProgress(i, iShapeCount)) {
    	  IFeature feature = iter.next();
    	  IRecord record = feature.getRecord();
    	  try {
    		  lineID = Double.parseDouble(record.getValue(m_iField).toString());
    		  iShape++;
    	  }
    	  catch (Exception e) {
    		  lineID = (double) iShape;
    		  iShape++;
    	  }

    	  Geometry geometry = feature.getGeometry();

    	  if (geometry.intersects(extent)) {
    		  //System.out.println("geometry " + i + " of " + iShapeCount);
    		  if(m_bRasterizeFirst == false){
    			  //-- create a new TempGrid
    			  m_tempOneLineRaster = getNewRasterLayer(TEMPRESULT, m_Layer.getName(),
    					  IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
    			  m_tempOneLineRaster.assign(0.0);
    			  m_tempOneLineRaster.setNoDataValue(0.0);
    			  //-- clean MergeGrid
    			  m_tempMergeLineRasterAB.assign(0.0);
    			  //-- fill the tempGrid of this line
    			  doLine(geometry);
    			  //-- merge the tempGrid with the last result grid (i.e. taking the max)
    			  if(lineID == (lastIDValue+1)){ // this will be usually true = except for the first segment
    				  //-- so if these are subsequent lines, then take the max 
    				  //   value from either raster 
    				  if (m_tempLastOneLineRaster.getWindowGridExtent().equals(m_tempOneLineRaster.getWindowGridExtent())){
    					  //System.out.println("line is subsequent, line#: " + lineID);
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
    			  }
    			  else{
    				  //-- if the lines are not subsequent, then we just add all values 
    				  //System.out.println("line is NOT subsequent, line#: " + lineID);
    				  m_tempMergeLineRasterAB.add(m_tempOneLineRaster);
    			  }
    			  //-- add the values to the result grid
    			  m_Result.add(m_tempMergeLineRasterAB);
    			  //-- now assign the new temp Grid to the last
    			  m_tempLastOneLineRaster = m_tempOneLineRaster;
    		  }
    		  else{// if rasterizefirst == true
    			  // the rasterization result will be written directly 
    			  // to the result raster
    			  doLine(geometry);
    		  }
    	  }
    	  i++;
    	  lastIDValue = lineID;
      }
      iter.close();
      if(m_bRasterizeFirst){
    	  //-- swap from result to tempGrid and create a new result raster with the KD
		  m_tempOneLineRaster.assign(0.0);
    	  m_tempOneLineRaster.add(m_Result);
    	  m_Result.assign(0.0);

          //-- run the kernelDE
          //System.out.println("calc kernel:"); //int idx = 0;
		  for (int x = 0; x < m_Result.getWindowGridExtent().getNX(); x++){
			  for (int y = 0; y < m_Result.getWindowGridExtent().getNY(); y++){
				  double dValue = m_tempOneLineRaster.getCellValueAsDouble(x, y);
				  if((dValue > 0)){
					  //-- for the common area of the kernelFunction grid of 
					  doPointForRasterizeFirst(x,y, dValue);
					  //idx++;
					  //System.out.println(""); 
				  }
			  }
		  }
		  //System.out.println("");
		  //System.out.println("should have placed " + idx + " Kernels" );
	      m_Result.multiply(1.0 / scalingFactor); //note, there is actually some dependence on line direction
      }
      return !m_Task.isCanceled();
   }

   private void doLine(Geometry geom) {
	   for (int i = 0; i < geom.getNumGeometries(); i++) {
		   Geometry part = geom.getGeometryN(i);
		   doLineString(part);
	   }
   }


   private void doLineString(Geometry geom) {

	   int i;
	   double x, y, x2, y2;
	   Coordinate[] coords = geom.getCoordinates();
	   for (i = 0; i < coords.length - 1; i++) {
		   x = coords[i].x;
		   y = coords[i].y;
		   x2 = coords[i + 1].x;
		   y2 = coords[i + 1].y;
		   writeSegment(x, y, x2, y2);
	   }
   }


   private void writeSegment(double x, double y, double x2, double y2) {
	   //-- the base code is from the Sextante line rasterization algorithm
	   double dx, dy, d, n;
	   GridCell cell;

	   dx = Math.abs(x2 - x);
	   dy = Math.abs(y2 - y);
	   double dist = Math.sqrt(dx*dx + dy*dy);
	   
	   //-- fill, but first get the step size for dx/dy
	   if (dx > 0.0 || dy > 0.0) {
		   if (dx > dy) {
			   dx /= m_Result.getWindowCellSize();
			   n = dx;
			   dy /= dx;
			   dx = m_Result.getWindowCellSize();
		   }
		   else {
			   dy /= m_Result.getWindowCellSize();
			   n = dy;
			   dx /= dy;
			   dy = m_Result.getWindowCellSize();
		   }

		   if (x2 < x) {
			   dx = -dx;
		   }

		   if (y2 < y) {
			   dy = -dy;
		   }
		   //-- now walk along the line with step dx/dy and fill the cells, 
		   //System.out.println("new line");
		   for (d = 0.0; d <= n; d++, x += dx, y += dy) {			   
			   if (m_Extent.contains(x, y)) {
				   //-- distance from x2,y2
				   double dxb = Math.abs(x2 - x);
				   double dyb = Math.abs(y2 - y);
				   double distb = Math.sqrt(dxb*dxb + dyb*dyb);
				   //System.out.println(cell.getX() + " " + cell.getY());
				   if(m_bRasterizeFirst){
					   //-- rasterization with weight value depending on distance
					   //double preValue = m_Result.getCellValueAsDouble(cell.getX(), cell.getY());
					   cell = m_Extent.getGridCoordsFromWorldCoords(x, y);
					   double var = 1.0;
					   double varx = (1 - (2 * distb/ dist) + (2 * distb*distb / (dist *dist))) * var;
					   double scaleFact = Math.sqrt(varx); //max [sqrt(0.5)...1.0]
					   if(m_bscaleWithSqrt == false){
							scaleFact = varx; //max [0.5...1.0]
						}
					   m_Result.setCellValue(cell.getX(), cell.getY(), scaleFact);
				   }
				   else{
					   //-- apply the kernel for the object-based case
					   //   get all cells of neighborhood within the bandwidth 
					   //   fill those cells with the weights from the kernel function
					   //   note that we need to adjust the neighborhood based on the
					   //   distance between the start and end point				   
					   doPoint(x, y, distb, dist);
				   }
			   }
		   }
	   }//end if dx,dy>0  
   }
   	
   /**
    * 
    * @param xCoord
    * @param yCoord
    * @param ptDist
    * @param lineLength
    */
	private void doPoint(double xCoord, double yCoord, double ptDist, double lineLength) {
		
		double dValue = 1.0; // we are not using a point value, i.e. a weight
		int x,y;
		int iX, iY;
		GridCell cell;

		cell = m_Extent.getGridCoordsFromWorldCoords(xCoord, yCoord);
		iX = cell.getX();
		iY = cell.getY();
		
		//-- this is a scaling function described by W Caspary and R Scheuning (CEUS 1993)
		//   for the line error: called "Error Band Model" (!= Epsilon Band)
		//   for ptDist = 0 or lineLength : scaleFact = 1
		//   for ptDist = 0.5*lineLength : scaleFact = sqrt(0.5) = 0.707 
		//   some R outputs:
		//-----------------
		//    li <- seq(0,30, by=3)
		//    li = [0  3  6  9 12 15 18 21 24 27 30]
		//    l <- 30
		//    varx <- (1 - (2 * li/ l) + (2 * li*li / (l *l))) * 1;
		//    varx = [1.00  0.82 0.68 0.58 0.52 0.50 0.52 0.58 0.68 0.82 1.00]
		//    varxsqr <- sqrt(varx)
		//    varxsqr = [1.0000000 0.9055385 0.8246211 0.7615773 0.7211103 0.7071068 
		//                 0.7211103 0.7615773 0.8246211 0.9055385 1.0000000]
		//-----------------[
		double var = 1.0;
		double varx = (1 - (2 * ptDist/ lineLength) + (2 * ptDist*ptDist / (lineLength *lineLength))) * var;
		double scaleFact = Math.sqrt(varx); //max [sqrt(0.5)...1.0]
		if(m_bscaleWithSqrt == false){
			scaleFact = varx; //max [0.5...1.0]
		}
		//System.out.println("ptDist: " + ptDist + "; LineLength: " + lineLength + "; scaleFact: " + scaleFact);
		
		//--		
		for(y = -m_iDistance; y < m_iDistance + 1; y++){
			for(x = -m_iDistance; x < m_iDistance + 1; x++){
				if(m_dWeight[x + m_iDistance][y + m_iDistance] != 0){
					double oldValue = m_tempOneLineRaster.getCellValueAsDouble(iX + x, iY + y);
					if(oldValue == 0.0){
						double value = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
						value = value*scaleFact;
						m_tempOneLineRaster.setCellValue(iX + x, iY + y, value);
					}
					else{
						//-- take the larger value of both and assign
						double newValue = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
						newValue = newValue * scaleFact;
						double value = 0.0;
						if(newValue >= oldValue){
							value = newValue;
						}
						else{
							value = oldValue;
						}
						m_tempOneLineRaster.setCellValue(iX + x, iY + y, value);
					}
				}
			}
		}
	}
	
	/**
	 * applies the Kernel to a raster cell (if the lines have been rasterized first)
	 * @param xGridCoord
	 * @param yGridCoord
	 * @param dValue : weight
	 */
	private void doPointForRasterizeFirst(int xGridCoord, int yGridCoord, double dValue) {
		int x,y;
		int iX, iY;

		iX = xGridCoord;
		iY = yGridCoord;

		for(y = -m_iDistance; y < m_iDistance + 1; y++){
			for(x = -m_iDistance; x < m_iDistance + 1; x++){
				if(m_dWeight[x + m_iDistance][y + m_iDistance] != 0){
					double oldValue = m_Result.getCellValueAsDouble(iX + x, iY + y);
					if(oldValue > 0.0){
						//System.out.print("+");
						//-- add the values
						double value = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
						m_Result.setCellValue(iX + x, iY + y, oldValue + value);
					}
					else{
						//System.out.print("0");
						double value = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
						m_Result.setCellValue(iX + x, iY + y, value);
					}
				}
			}
		}
	}
}
