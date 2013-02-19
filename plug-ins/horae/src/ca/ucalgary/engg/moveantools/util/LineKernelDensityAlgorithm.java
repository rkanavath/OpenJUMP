/*******************************************************************************
LineKernelDensityAlgorithm.java
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

public class LineKernelDensityAlgorithm extends GeoAlgorithm {

   public static final String  LINELAYER   = "LINELAYER";
   public static final String  BANDWIDTH   = "BANDWIDTH";
   public static final String  VALUEFIELD = "VALUEFIELD";
   public static final String  RESULT  = "RESULT";
   public static final String  TEMPRESULT  = "TEMPRESULT";
   public static final String  RASTERIZEFIRST  = "RASTERIZEFIRST";

   private int                 m_iField;
   private int				   m_iDistance;
   private double 			   m_dBandwidth;
   private IVectorLayer        m_Layer;
   private IRasterLayer        m_Result;
   private IRasterLayer        m_tempOneLineRaster;
   private AnalysisExtent          m_Extent;
   private double 			   m_dWeight[][];
   private boolean 			   m_bRasterizeFirst;


   public void defineCharacteristics() {

      setName("Line Kernel Density");
      setGroup("Rasterization_and_interpolation");
      setUserCanDefineAnalysisExtent(true);

      try {
         m_Parameters.addInputVectorLayer(LINELAYER, "Vector line layer", AdditionalInfoVectorLayer.SHAPE_TYPE_LINE, true);
         m_Parameters.addTableField(VALUEFIELD, "Value field", LINELAYER);
         m_Parameters.addNumericalValue(BANDWIDTH, "Bandwidth [in m]", AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
										100, 0, Double.MAX_VALUE);
         m_Parameters.addBoolean(RASTERIZEFIRST, "rasterize lines first", true);
         addOutputRasterLayer(RESULT, "Result");
         addOutputRasterLayer(TEMPRESULT, "Rasterize Result");
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
      double lineValue;

      m_Layer = m_Parameters.getParameterValueAsVectorLayer(LINELAYER);
      m_iField = m_Parameters.getParameterValueAsInt(VALUEFIELD);
      m_dBandwidth = m_Parameters.getParameterValueAsDouble(BANDWIDTH);
      m_bRasterizeFirst = m_Parameters.getParameterValueAsBoolean(RASTERIZEFIRST);

      m_Result = getNewRasterLayer(RESULT, m_Layer.getName() + "-KDE",
               						IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_Result.assign(0.0);
      
      //- we need this temporary raster, since calculations for one segment/line
      //  will be a bit different when assigning cell values
      m_tempOneLineRaster = getNewRasterLayer(TEMPRESULT, m_Layer.getName() + "-Temp",
					IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
      m_tempOneLineRaster.assign(0.0);
      
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
    			   * Standard Sextante Kernel
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
      while (iter.hasNext() && setProgress(i, iShapeCount)) {
    	  IFeature feature = iter.next();
    	  IRecord record = feature.getRecord();
    	  try {
    		  lineValue = Double.parseDouble(record.getValue(m_iField).toString());
    		  iShape++;
    	  }
    	  catch (Exception e) {
    		  lineValue = (double) iShape;
    		  iShape++;
    	  }

    	  Geometry geometry = feature.getGeometry();

    	  if (geometry.intersects(extent)) {
    		  //System.out.println("geometry " + i + " of " + iShapeCount);
    		  //-- clear the tempGrid
    		  m_tempOneLineRaster.assign(0.0);

    		  //-- fill the tempGrid of this line
    		  doLine(geometry, lineValue);
    		  //-- check if something happend
    		  /*
    		  double maxvalue =  m_tempOneLineRaster.getMaxValue(0);
    		  double minvalue = m_tempOneLineRaster.getMinValue(0);
    		  double meanvalue =  m_tempOneLineRaster.getMeanValue(0);
    		  System.out.println("line grid vals #: " + i + " --- max: " + maxvalue + " min: " + minvalue + " mean: " + meanvalue);
    		  */
    		  //-- add the tempGrid to the result grid
    		  //   in the case of rasterizationFirst we write directly to m_Result
    		  if(m_bRasterizeFirst == false){
    			  m_Result.add(m_tempOneLineRaster);
    		  }
    	  }
    	  i++;
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
      //-- check what changed
      /*
	  double maxvalueF =  m_Result.getMaxValue(0);
	  double minvalueF = m_Result.getMinValue(0);
	  double meanvalueF =  m_Result.getMeanValue(0);
      System.out.println("result grid vals after --- max: " + maxvalueF + " min: " + minvalueF + " mean: " + meanvalueF);
      */
      return !m_Task.isCanceled();
   }

   private void doLine(Geometry geom, double dValue) {
	   for (int i = 0; i < geom.getNumGeometries(); i++) {
		   Geometry part = geom.getGeometryN(i);
		   doLineString(part, dValue);
	   }
   }


   private void doLineString(Geometry geom, double dValue) {

	   int i;
	   double x, y, x2, y2;
	   Coordinate[] coords = geom.getCoordinates();
	   for (i = 0; i < coords.length - 1; i++) {
		   x = coords[i].x;
		   y = coords[i].y;
		   x2 = coords[i + 1].x;
		   y2 = coords[i + 1].y;
		   writeSegment(x, y, x2, y2, dValue);
	   }
   }


   private void writeSegment(double x, double y, double x2, double y2, double dValue) {
	   //-- the base code is from the Sextante line rasterization algorithm
	   double dx, dy, d, n;
	   GridCell cell;

	   dx = Math.abs(x2 - x);
	   dy = Math.abs(y2 - y);
	   
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
		   for (d = 0.0; d <= n; d++, x += dx, y += dy) {			   
			   if (m_Extent.contains(x, y)) {
				   //System.out.println(cell.getX() + " " + cell.getY());
				   if(m_bRasterizeFirst){
					   //-- rasterization with dValue
					   //double preValue = m_Result.getCellValueAsDouble(cell.getX(), cell.getY());
					   cell = m_Extent.getGridCoordsFromWorldCoords(x, y);
					   m_Result.setCellValue(cell.getX(), cell.getY(), dValue);
				   }
				   else{
					   //-- apply the kernel for the object-based case
					   //   get all cells of neighborhood within the bandwidth 
					   //   fill those cells with the weights from the kernel function
					   //   note that we need to adjust the neighborhood based on the
					   //   distance between the start and end point				   
					   doPoint(x, y, dValue);
				   }
			   }
		   }
	   }//end if dx,dy>0  
   }

   /**
    * applies the kernel for a point of the line geometry
    * @param xRealCoord
    * @param yRealCoord
    * @param dValue
    */
	private void doPoint(double xRealCoord, double yRealCoord, double dValue) {
		int x,y;
		int iX, iY;
		GridCell cell;

		cell = m_Extent.getGridCoordsFromWorldCoords(xRealCoord, yRealCoord);
		iX = cell.getX();
		iY = cell.getY();

		for(y = -m_iDistance; y < m_iDistance + 1; y++){
			for(x = -m_iDistance; x < m_iDistance + 1; x++){
				if(m_dWeight[x + m_iDistance][y + m_iDistance] != 0){
					double oldValue = m_tempOneLineRaster.getCellValueAsDouble(iX + x, iY + y);
					if(oldValue == 0.0){
						//System.out.print("0");
						double value = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
						m_tempOneLineRaster.setCellValue(iX + x, iY + y, value);
					}
					else{
						//System.out.print("m");
						//-- take the larger value of both and assign
						double newValue = dValue * m_dWeight[x + m_iDistance][y + m_iDistance];
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
	 * @param dValue
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
