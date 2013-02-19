/*******************************************************************************
KernelDensitySextanteModifiedAlgorithm.java
Copyright (C) Victor Olaya
Copyright (C) Stefan Steiniger for different kernels

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

import es.unex.sextante.additionalInfo.AdditionalInfoNumericalValue;
import es.unex.sextante.additionalInfo.AdditionalInfoVectorLayer;
import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.core.GeoAlgorithm;
import es.unex.sextante.dataObjects.IFeature;
import es.unex.sextante.dataObjects.IFeatureIterator;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.exceptions.OptionalParentParameterException;
import es.unex.sextante.exceptions.RepeatedParameterNameException;
import es.unex.sextante.exceptions.UndefinedParentParameterNameException;
import es.unex.sextante.rasterWrappers.GridCell;

public class KernelDensitySextanteModifiedAlgorithm extends GeoAlgorithm {

	public static final String DENSITY = "DENSITY";
	public static final String DISTANCE = "DISTANCE";
	public static final String FIELD = "FIELD";
	public static final String LAYER = "LAYER";
	public static final String KERNEL = "KERNEL";

	public static final int KERNEL_VALUE_STANDARD = 0;
	public static final int KERNEL_VALUE_BIWEIGHT = 1;
	public static final int KERNEL_VALUE_TRIWEIGHT = 2;
	public static final int KERNEL_VALUE_EPANECHNIKOV = 3;
	public static final int KERNEL_VALUE_SCALED_NORMAL = 4;
	public static final int KERNEL_VALUE_TRIANGULAR = 5;
	public static final int KERNEL_VALUE_UNIFORM = 6;
	public static final int KERNEL_VALUE_COSINE = 7;
	
	private int m_iField;
	private IVectorLayer m_Layer;
	private IRasterLayer m_Result;
	private AnalysisExtent m_Extent;
	private int m_iDistance;
	private double m_dWeight[][];
	private int m_iKernel;

	public void defineCharacteristics() {

		setName(Sextante.getText("Densidad_kernel"));
		setGroup(Sextante.getText("Rasterizacion_e_interpolacion"));
		setUserCanDefineAnalysisExtent(true);

		String sMethod[] = {"Standard Kernel",
				"Biweight Kernel K2", "Triweight Kernel K3", "Epanechnikov Kernel", 
				"Scaled Normal Kernel", "Triangular Kernel", "Uniform Kernel", "Cosine Kernel"};
		
		try {
			m_Parameters.addInputVectorLayer(LAYER,
										Sextante.getText("Capa_vectorial"),
										AdditionalInfoVectorLayer.SHAPE_TYPE_POINT,
											true);
			m_Parameters.addTableField(FIELD,
										Sextante.getText("Campo"), LAYER);
			m_Parameters.addNumericalValue(DISTANCE,
										Sextante.getText( "Radio_de_busqueda"),
										AdditionalInfoNumericalValue.NUMERICAL_VALUE_DOUBLE,
										100, 0, Double.MAX_VALUE);
			m_Parameters.addSelection(KERNEL,
						"Kernel type",
						sMethod);
			addOutputRasterLayer(DENSITY,
										Sextante.getText("Densidad"));
		} catch (UndefinedParentParameterNameException e) {
				Sextante.addErrorToLog(e);
		} catch (OptionalParentParameterException e) {
				Sextante.addErrorToLog(e);
		} catch (RepeatedParameterNameException e) {
			Sextante.addErrorToLog(e);
		}

	}

	public boolean processAlgorithm() throws GeoAlgorithmExecutionException {

		int i;
		int x,y;
		int iShapeCount;
		double dValue;
		double dXMin, dYMin;
		double dXMax, dYMax;
		double dDistance;

		m_Layer = m_Parameters.getParameterValueAsVectorLayer(LAYER);
		m_iField = m_Parameters.getParameterValueAsInt(FIELD);
		dDistance = m_Parameters.getParameterValueAsDouble(DISTANCE);
		m_iKernel = m_Parameters.getParameterValueAsInt(KERNEL);

		m_Result = getNewRasterLayer(DENSITY, m_Layer.getName() +
											Sextante.getText("densidad2"),
											IRasterLayer.RASTER_DATA_TYPE_DOUBLE);
		m_Result.assign(0.0);

		m_Extent = m_Result.getWindowGridExtent();

		m_iDistance = (int) Math.floor(dDistance / m_Extent.getCellSize());

		dXMin = m_Extent.getXMin() - dDistance;
		dYMin = m_Extent.getYMin() - dDistance;
		dXMax = m_Extent.getXMax() + dDistance;
		dYMax = m_Extent.getYMax() +  dDistance;

		m_dWeight = new double [2 * m_iDistance + 1][2 * m_iDistance + 1];

		for(y = -m_iDistance; y < m_iDistance + 1; y++){
			for(x = -m_iDistance; x < m_iDistance + 1; x++){
				double dDist = Math.sqrt(x*x + y*y);
				//-- u calculated with cell units 
				//   u = [0..1]
				double u = dDist / (double)m_iDistance; // to implement 1D kernels [it may be that x'x is different from that]
				//-- u calculated with real units
				//double u = dDist*m_Extent.getCellSize() / dDistance;
				if (dDist < m_iDistance){
					/**
					 * Note, if new Kernels are added String sMethod[] = ... needs to be changed!
					 */
					if(m_iKernel == KERNEL_VALUE_BIWEIGHT){
						/** 
						 * Quadratic/quartic/biweight kernel K2 given in Silverman 1986 pg.76 (eq. 4.5)
						 *  w = 3/Pi * (1-x'x)^2
						 * according to Seaman and Powell (1996, pg.2076):
						 * (x'x) = dist(x_obs - x_eval)/h
						 * However, it seems like x'x = (dist/h)^2 : so I use the 1D version.
						 */
						//m_dWeight[x + m_iDistance][y + m_iDistance] =
						//	(3.0 / Math.PI) * Math.pow(1 - u, 2);	
						/** 
						 * Biweight 1D version Silverman (1986:43) - Table 3.1 and Wikipedia
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							15.0 / 16.0 * Math.pow(1 - (u*u), 2);	
					}
					else if(m_iKernel == KERNEL_VALUE_TRIWEIGHT){
						/** 
						 * Kernel K3 given in Silverman 1986 pg.76 (eq. 4.6)
						 *  w = 4/Pi * (1-x'x)^3
						 * according to Seaman and Powell (1996, pg.2076):
						 * (x'x) = dist(x_obs - x_eval)/h
						 * However, it seems like x'x = (dist/h)^2 : so I use the 1D version.
						 */
						//m_dWeight[x + m_iDistance][y + m_iDistance] =
						//	(4.0 / Math.PI) * Math.pow(1 - u, 3);	
						/** 
						 * 1D version from Wikipedia
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							35.0 / 32.0 * Math.pow(1 - (u*u), 3);	
					}
					else if(m_iKernel == KERNEL_VALUE_EPANECHNIKOV){
						/** 
						 * Epanechnikov kernel K_E2 given in Silverman 1986 pg.76 (eq. 4.4)
						 *  w = 1/2* 1/c_d * (d+2) * (1-x'x) with d=2 and c_(d=2) = Pi
						 * according to Seaman and Powell (1996, pg.2076):
						 * (x'x) = dist(x_obs - x_eval)/h
						 * However, it seems like x'x = (dist/h)^2 : so I use the 1D version.
						 */
						//m_dWeight[x + m_iDistance][y + m_iDistance] =
						//	(2.0 / Math.PI) * (1 - u);
						/** 
						 * 1-D Epanechnikov from DW Scott (1992: 134, Table 61) & Wikipedia
						 * Silverman (1986:43) - Table 3.1 - gives a different version for
						 * a different range: u < sqrt(5) 
						 * this one looks as it should with the profile graph tool
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							3.0 / 4.0 * (1 - (u*u));
					}
					else if(m_iKernel == KERNEL_VALUE_SCALED_NORMAL){
						/** 
						 * Normal/Gaussian kernel given in Silverman 1986 pg.76 (eq. 4.3)
						 *  w = (2*Pi)^(- d/2) * e^(-1/2 * x'x) with d=2 
						 * according to Seaman and Powell (1996, pg.2076):
						 * (x'x) = dist(x_obs - x_eval)/h
						 * However, it seems like x'x = (dist/h)^2 : so I use the 1D version.
						 */
						//m_dWeight[x + m_iDistance][y + m_iDistance] =
						//	(1.0 / (2.0*Math.PI)) * Math.pow(Math.E, -1 * 0.5 * u );
						/** 
						 * 1-D normal/Gaussian kernel from 
						 * Silverman (1986:43) - Table 3.1 and Wikipedia
						 */
						//-- multiply with Pi since the Shape of the Gaussian goes has different extent
						double us = u*Math.PI; //maybe this should be 2.78 or 3.12? 
								 //Silverman (1986:87) - Table 4.1 conversion factors:
								 // normal vs. biweight: 2.78
								 // normal vs. triweight: 3.12
								 // normal vs. epanechnikov: 2.4 (2D)
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							(1.0 / Math.sqrt(2.0*Math.PI)) * (1.0 / Math.pow(Math.E,  0.5 * (us*us) ));
					}
					else if(m_iKernel == KERNEL_VALUE_TRIANGULAR){
						/** 
						 * 1-D Triangular kernel from 
						 * Silverman (1986:43) - Table 3.1 and Wikipedia
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							1 - Math.abs(u);
					}
					else if(m_iKernel == KERNEL_VALUE_UNIFORM){
						/** 
						 * 1-D Uniform/Rectangular kernel from 
						 * Silverman (1986:43) - Table 3.1 and Wikipedia
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							0.5;
					}
					else if(m_iKernel == KERNEL_VALUE_COSINE){
						/** 
						 * 1-D Cosine Kernel from Wikipedia
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							(Math.PI / 4.0) * Math.cos( u * Math.PI / 2.0);
					}
					else{
						/** 
						 * Standard Sextante Kernel
						 * this one looks as it should with the profile graph tool,
						 * also weight_max_value = 1.0
						 * Note - this is the biweight kernel without scaling factor
						 */
						m_dWeight[x + m_iDistance][y + m_iDistance] =
							Math.pow(1 - (dDist * dDist) / (m_iDistance * m_iDistance), 2);
					}
				}
				else{
					m_dWeight[x + m_iDistance][y + m_iDistance] = 0;
				}
			}
		}

		i = 0;
		iShapeCount = m_Layer.getShapesCount();
		IFeatureIterator iter = m_Layer.iterator();
		while(iter.hasNext() && setProgress(i, iShapeCount)){
			IFeature feature = iter.next();
			Geometry geom = feature.getGeometry();
			Coordinate coord = geom.getCoordinate();

			try{
				dValue = Double.parseDouble(feature.getRecord().getValue(m_iField).toString());
			}
			catch (Exception e){
				dValue = 1.0;
			}

			if (coord.x > dXMin && coord.x < dXMax &&
						coord.y > dYMin && coord.y < dYMax){
				doPoint(coord, dValue);
			}
			i++;
		}
		iter.close();
		
		return !m_Task.isCanceled();

	}

	private void doPoint(Coordinate coord, double dValue) {

		int x,y;
		int iX, iY;
		GridCell cell;

		cell = m_Extent.getGridCoordsFromWorldCoords(coord.x, coord.y);
		iX = cell.getX();
		iY = cell.getY();

		for(y = -m_iDistance; y < m_iDistance + 1; y++){
			for(x = -m_iDistance; x < m_iDistance + 1; x++){
				if(m_dWeight[x + m_iDistance][y + m_iDistance] != 0){
					m_Result.addToCellValue(iX + x, iY + y, dValue * m_dWeight[x + m_iDistance][y + m_iDistance]);
				}
			}
		}
	}

}
