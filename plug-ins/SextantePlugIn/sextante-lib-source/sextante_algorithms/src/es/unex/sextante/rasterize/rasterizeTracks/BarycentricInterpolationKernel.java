package es.unex.sextante.rasterize.rasterizeTracks;

import java.util.Arrays;

import com.amd.aparapi.Kernel;

import es.unex.sextante.core.Sextante;

public class BarycentricInterpolationKernel extends Kernel{

	private static final double BARYCENTRIC_CUTOFF = 0;
	private static final int m_iSuperSamplingWindowSize = 2;
	private final double  windowStepX;
	private final double  windowStepY;
	private final double m_dXMin;
	private final double m_dYMax;
	private final double m_dHighCut;
	private final double m_dLowCut;
	
	private int[] globalrastersize;
	private double[] coordsx;
	private double[] coordsy;
	private double[] values;
	private double[] globalCalcValues;
	private double[] globalCalcCount;
	private double[] detAreas;
	private int[] rasters;
	private double dDetArea_CutOff;
	
	
	public BarycentricInterpolationKernel(double windowStepX, double windowStepY,double m_dXMin,double m_dYMax, double m_dHighCut, double m_dLowCut, double detAreaCutOff) {
		super();
		this.windowStepX = windowStepX;
		this.windowStepY = windowStepY;
		this.m_dXMin=m_dXMin;
		this.m_dYMax=m_dYMax;
		this.m_dHighCut=m_dHighCut;
		this.m_dLowCut=m_dLowCut;
		this.dDetArea_CutOff=detAreaCutOff;
	}
	
	public void init(int[] globalrastersize, double[] coordsx, double[] coordsy, double[] values){
		this.globalrastersize=globalrastersize;
		this.coordsx=coordsx;
		this.coordsy=coordsy;
		this.values=values;
		int globalWidth = globalrastersize[2]-globalrastersize[0];
		int globalHeight = globalrastersize[3]-globalrastersize[1];;
		globalCalcValues = new double[globalWidth*globalHeight];
		globalCalcCount = new double[globalWidth*globalHeight];
		detAreas = new double[coordsx.length / 3];
		rasters = new int[(coordsx.length / 3) * 4];
		for (int triangle = 0; triangle < coordsx.length / 3; triangle++) {
			double[] curcoordsx	=	Arrays.copyOfRange(coordsx, triangle * 3, (triangle + 1) * 3);
			double[] curcoordsy	=	Arrays.copyOfRange(coordsy, triangle * 3, (triangle + 1) * 3);
			int[] raster = computeRasterbounds(curcoordsx,curcoordsy);
			rasters[triangle*4]=raster[0];
			rasters[triangle*4+1]=raster[1];
			rasters[triangle*4+2]=raster[2];
			rasters[triangle*4+3]=raster[3];
			detAreas[triangle] = (curcoordsx[0] - curcoordsx[2]) * (curcoordsy[1] - curcoordsy[2])
					- (curcoordsx[1] - curcoordsx[2]) * (curcoordsy[0] - curcoordsy[2]);
		}
		for (int sensor = 0; sensor < values.length; sensor++) {
			this.values[sensor]=max(min(values[sensor], m_dHighCut), m_dLowCut);;
		}
	}
	
	@Override
	public void run() {
		// TODO Auto-generated method stub
		int globalWidth = globalrastersize[2] - globalrastersize[0];
		int i = getGlobalId();
		int x = i % (globalWidth) + globalrastersize[0];
		int y = i / (globalWidth) + globalrastersize[1];
		double[] dscaled;
		double centerx,centery,alpha,beta,gamma,detArea,total;
		int shifteddx,shifteddy,quadoffset,triangleoffset,count;
		for (int triangle = 0; triangle < coordsx.length / 3; triangle++) {
			quadoffset = 4 * triangle;
			if(!(globalrastersize[0]<=rasters[quadoffset+0]&&rasters[quadoffset+2]<=globalrastersize[2]&&
					globalrastersize[1]<=rasters[quadoffset+1]&&rasters[quadoffset+3]<=globalrastersize[3])) continue;
			detArea = detAreas[triangle];
			double scaledDetArea = (detArea * windowStepX) *windowStepY;
			if (scaledDetArea > dDetArea_CutOff) {
				Sextante.addInfoToLog(scaledDetArea + ">" + dDetArea_CutOff);
				continue;
			}
			triangleoffset = 3 * triangle;
			
					total = 0;
					count = 0;
					dscaled = getWorldCoordsFromGridCoords(x, y);
					centerx = dscaled[0];//((posx) + computedOffsets[0]) / computedOffsets[2];
					centery = dscaled[1];//((posy) + computedOffsets[1]) / computedOffsets[3];
					alpha = (centerx - coordsx[triangleoffset + 2]) * (coordsy[triangleoffset + 1] - coordsy[triangleoffset + 2])
							- (coordsx[triangleoffset + 1] - coordsx[triangleoffset + 2]) * (centery - coordsy[triangleoffset + 2]);
					beta = (centerx - coordsx[triangleoffset]) * (coordsy[triangleoffset + 2] - coordsy[triangleoffset])
							- (coordsx[triangleoffset + 2] - coordsx[triangleoffset]) * (centery - coordsy[triangleoffset]);
					gamma = (centerx - coordsx[triangleoffset + 1]) * (coordsy[triangleoffset] - coordsy[triangleoffset + 1])
							- (coordsx[triangleoffset] - coordsx[triangleoffset + 1]) * (centery - coordsy[triangleoffset + 1]);
					if (alpha > BARYCENTRIC_CUTOFF && beta > BARYCENTRIC_CUTOFF && gamma > BARYCENTRIC_CUTOFF) {
						total += (alpha * values[triangleoffset] + beta * values[triangleoffset + 1] + gamma * values[triangleoffset + 2]) / detArea;
						count++;
					} else if (m_iSuperSamplingWindowSize > 0) {
						for (int halfwindowssize = 1; halfwindowssize <= m_iSuperSamplingWindowSize; halfwindowssize++) {
							for (int dx = -halfwindowssize; dx <= halfwindowssize; dx++) {
								for (int dy = -halfwindowssize; dy <= halfwindowssize; dy++) {
									dscaled = getWorldCoordsFromGridCoords(x, y);
									centerx = dscaled[0];//((posx) + computedOffsets[0]) / computedOffsets[2];
									centery = dscaled[1];
									alpha = (centerx - coordsx[triangleoffset + 2]) * (coordsy[triangleoffset + 1] - coordsy[triangleoffset + 2])
											- (coordsx[triangleoffset + 1] - coordsx[triangleoffset + 2]) * (centery - coordsy[triangleoffset + 2]);
									beta = (centerx - coordsx[triangleoffset]) * (coordsy[triangleoffset + 2] - coordsy[triangleoffset])
											- (coordsx[triangleoffset + 2] - coordsx[triangleoffset]) * (centery - coordsy[triangleoffset]);
									gamma = (centerx - coordsx[triangleoffset + 1]) * (coordsy[triangleoffset] - coordsy[triangleoffset + 1])
											- (coordsx[triangleoffset] - coordsx[triangleoffset + 1]) * (centery - coordsy[triangleoffset + 1]);
									if (alpha < BARYCENTRIC_CUTOFF || beta < BARYCENTRIC_CUTOFF
											|| gamma < BARYCENTRIC_CUTOFF)
										continue;
									total += (alpha * values[triangleoffset] + beta * values[triangleoffset + 1] + gamma * values[triangleoffset + 2]) / detArea;
									count++;
								}
							}
							if (count > 0) {
								break;
							}
						}
					}
					//if (0 <= dx && dx < maxX && 0 <= dy && dy < maxY && count > 0) {
					if(count>0){
						shifteddx = x - globalrastersize[0];
						shifteddy = y - globalrastersize[1];						
						globalCalcValues[shifteddy * globalWidth + shifteddx] += total;
						globalCalcCount[shifteddy * globalWidth + shifteddx] += count;
					}
					//}
				}
			
		
		
	}

	/**
	 * computes the bounds for rasterizing of a track given as an Array of coordinates
	 * @param coordinates of the given track
	 * @return an int[4] { minX, minY, maxX, maxY};
	 */
	public int[] computeRasterbounds(double[] coordsx,double[] coordsy) {
		int minX = Integer.MAX_VALUE;
		int maxX = Integer.MIN_VALUE;
		int minY = Integer.MAX_VALUE;
		int maxY = Integer.MIN_VALUE;
		for (int i = 0; i < coordsy.length; i++){
			int[]  coordinate = getGridCoordsFromWorldCoords(coordsx[i],coordsy[i]);
			minX = coordinate[0] < minX ? coordinate[0] : minX;
			maxX = coordinate[0] > maxX ? coordinate[0] : maxX;
			minY = coordinate[1] < minY ? coordinate[1] : minY;
			maxY = coordinate[1] > maxY ? coordinate[1] : maxY;
		}
		return new int[] { minX, minY, maxX, maxY };
	}

	public double[] getWorldCoordsFromGridCoords(final int x, final int y) {
		return new double[] { m_dXMin + (x + 0.5) * windowStepX, m_dYMax - (y + 0.5) * windowStepY };
	}
	
	
	
	public int[] getGridCoordsFromWorldCoords(double dx, double dy) {

		final int x = (int) Math.floor((dx - m_dXMin) / windowStepX);
		final int y = (int) Math.floor((m_dYMax - dy) / windowStepY);

		return new int[]{x,y};

	}
	
	public double[] getGlobalCalcCount() {
		return globalCalcCount;
	}
	
	public double[] getGlobalCalcValues() {
		return globalCalcValues;
	}
}
