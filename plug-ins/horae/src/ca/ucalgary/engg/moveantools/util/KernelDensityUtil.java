package ca.ucalgary.engg.moveantools.util;

import java.util.Iterator;
import java.util.List;

import org.math.array.DoubleArray;
import org.math.array.LinearAlgebra;
import org.math.array.StatisticSample;
import org.math.array.util.Sorting;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.rasterWrappers.GridWrapperNotInterpolated;

public class KernelDensityUtil{

	public static double A_K = 2.78;

	/**
	 * converts a probability value into a height value based on the volume for contour generation.
	 * @param raster
	 * @param probValue [0.0 ... 1.0]
	 * @return double[] [0] is the height/contour value, [1] is the real probability obtained after 
	 * all raster cells with the same value are added.
	 */
	public static double[] calculateContourHeightValueFromProbabilityValue(OpenJUMPRasterLayer raster, double probValue){
		// create a gridwrapper to later access the cells
		GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(raster, raster.getLayerGridExtent());
		//-- create an array of cell values
		int nx = raster.getLayerGridExtent().getNX();
		int ny = raster.getLayerGridExtent().getNY();
		int numCells = nx * ny;
		double[] values = new double[numCells];
		int i = 0;
		for (int x = 0; x < nx; x++) {//cols
			for (int y = 0; y < ny; y++) {//rows
				values[i] = gwrapper.getCellValueAsDouble(x, y);
				i++;
			}
		}
		gwrapper = null; System.gc();
		//-- sort the values (ascending 0...X)
		//new Sorting(values, true); // here values are not copied, but I seem to get different results. So use the old one? 
		values = DoubleArray.sort(values);
		//-- make descending
		double[] valuesN = new double[numCells];
		for (int j = 0; j < values.length; j++) {//rows
			valuesN[j] = values[values.length-1-j];
		}
		values = valuesN;
		//-- create and fill an array of sums (so we know about the total volume)
		double[] sums = new double[values.length];
		sums[0] = values[0];
		for (int j = 1; j < values.length; j++) {//rows
			sums[j] = sums[j-1] + values[j];
		}
		double total = sums[sums.length-1];
		double threshold = probValue * total;
		double cellValue = 0; boolean found = false; int foundIndex = 0;
		//-- search the threshold in the sum set, and get the associated cell value
		//  
		int j=0;
		while(found == false){//rows
			if(sums[j] < threshold){
				//keep going
				j++;
			}
			else{
				cellValue = values[j];
				foundIndex = j;
				found = true;
			}
			if((j == sums.length) &&(found == false)){
				found = true;
				System.out.println("calculateContourHeightValueFromProbabilityValue: height value not found");
			}			
		}
		//-- we need to account for all cells that have that value
		//   assuming that rather higher probabilities are wished we are looking forwards
		//-- get the sum from the last value of the same, so we know what probability we
		//    really obtain
		boolean noChange = true; int u = 1; 
		double cellSum = sums[foundIndex];
		while(noChange){
			if(values[foundIndex] == values[foundIndex+u]){
				//-- it is still the same, update the sum value
				cellSum = sums[foundIndex+u];
				u++;
			}
			else{
				noChange = false;
			}
			if ((foundIndex+u) == (values.length)){
				noChange = false;
			}
		}
		double realProb = cellSum / total;
		double mappingResult[] = new double[2];
		//-- set outputs
		mappingResult[0] = cellValue;
		mappingResult[1] = realProb;
		return mappingResult;
	}

	/**
	 * removes points from the data set that have no other points within the given distance. 
	 * The function utilizes a quadtree for speed improvements.
	 * @param points
	 * @param distance for which points are check of having no neighbors in that circle 
	 * @param context can be null
	 * @return
	 */
	public static FeatureCollection removePoints(FeatureCollection points, double distance, PlugInContext context) {
		List<Feature> features = points.getFeatures();
		FeatureSchema fs = points.getFeatureSchema();
		FeatureDataset removedPointsFD = new FeatureDataset(fs);  
		FeatureDataset cleanedPointsFD = new FeatureDataset(fs);
		//-- put all points in a tree
		Quadtree qTree = new Quadtree();
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			qTree.insert(f.getGeometry().getEnvelopeInternal(), f);
		}
		//-- search for every point all neighbors
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			boolean hasNeighbours = false;
			//--buffer the feature
			Geometry curGeom = f.getGeometry();
			Geometry buffer = curGeom.buffer(distance);
			List candidates = qTree.query(buffer.getEnvelopeInternal());
			//-- test the distance of all those
			for (Iterator iterator2 = candidates.iterator(); iterator2.hasNext();) {
				Feature fToTest = (Feature) iterator2.next();
				//-- check if it is the item itself
				if (f.equals(fToTest)){
					//System.out.println("found myself");
					// ignore it
				}
				else{
					double d = fToTest.getGeometry().distance(curGeom);
					if (d < distance){
						hasNeighbours = true;
					}
				}
			}
			//-- if another object was found, then the point stays
			if(hasNeighbours){
				cleanedPointsFD.add(f.clone(true));
			}
			else{
				removedPointsFD.add(f.clone(true));
			}
		}
		
		if ((context != null) && (removedPointsFD.size() > 0)){
			context.addLayer(StandardCategoryNames.RESULT, "removed_points", removedPointsFD);
		}
		return cleanedPointsFD;
	}
	
	/**
	 * Calculates h_ref for Kernel Density calculation. 
	 * note, this method uses only the first point of each geometry (i.e. works only for points)
	 * see Silverman 1986, p.86-87, BJ Worton 1995 
	 * @param pointLayer
	 * @return
	 */
	public static double calculateHref(Layer pointLayer) {
		double href = 0;
		//-- get the coords
		List<Feature> features = pointLayer.getFeatureCollectionWrapper().getFeatures(); 
		href = calculateHref(features);
		return href;
	}

	/**
	 * Calculates h_ref for Kernel Density calculation. 
	 * note, this method uses only the first point of each geometry (i.e. works only for points)
	 * see Silverman 1986, p.86-87, BJ Worton 1995 
	 * @param pointFeatures
	 * @return
	 */
	public static double calculateHref(List<Feature> points) {
		double href = 0;
		//-- get the coords
		List<Feature> features = points; 
		int n = features.size(); 
		double[] x = new double[n]; 
		double[] y = new double[n];
		int i =0;
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			Coordinate coord1 = f.getGeometry().getCoordinate();
			x[i] = coord1.x;
			y[i] = coord1.y;
			i++;			
		}
		//-- get variance
		double xvar = StatisticSample.variance(x); 
		double yvar = StatisticSample.variance(y);
		double std = Math.sqrt(0.5*(xvar+yvar));
		double a_k = 1; //
		href= a_k*std / Math.pow(n, 1.0/6.0);
		//--format to 3 digits
		href = (Math.round(href*1000))/1000.0;
		return href;
	}
	
	/**
	 * Calculates h_ref for Kernel Density calculation based on a biweight Kernel. 
	 * note, this method uses only the first point of each geometry (i.e. works only for points)
	 * see Silverman 1986, p.86-87, BJ Worton 1995 
	 * @param pointLayer
	 * @return
	 */
	public static double calculateHrefBiWeightKernel(Layer pointLayer) {
		double href = 0;
		href = calculateHref(pointLayer);
		href = KernelDensityUtil.A_K * href;
		double hrefRounded = (Math.round(href*1000))/1000.0;
		return hrefRounded;
	}
	
	/**
	 * calculates the point-to-point distances
	 * @param fc a FeatureCollection of points
	 * @return returns 'null' if features are not point geometries
	 */
	public static double[][] createPointDistanceMatrix(FeatureCollection fc) {
		int numPoints = fc.size();
		double[][] dists = DoubleArray.fill(numPoints,numPoints, 0);
		int i = 0;
		List<Feature> features = fc.getFeatures();
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if(f.getGeometry() instanceof Point){
				Point pt =	(Point)f.getGeometry();
				for(int j = i; j < numPoints; j++){
					// note, by starting with j=i, the value should be 0 (on the diagonal)
					Feature ftemp = features.get(j);
					if(f.getGeometry() instanceof Point){
						Point ptTemp =	(Point)ftemp.getGeometry();
						double d = pt.distance(ptTemp);
						//-- set for both matrix positions
						dists[i][j] = d;
						dists[j][i] = d;
					}
					else{
						return null;
					}
				}
			}
			else{
				return null;
			}
			i++;
		}
		return dists;
	}
	
	/**
	 * calculates the score value CV(h) as given in BJ Worton (1995). 
	 * Note, it seems like this function can't handle well zero distances
	 * between points (would be e^-0). So I exclude those distances.
	 * @param h the smoothing parameter (i.e. window size, band width)
	 * @param dists a n*n distance matrix that stores the point distance 
	 *        (can be obtained with createPointDistanceMatrix())
	 * @return
	 */
	public static double calculateCVScore(double h, double[][] distances){
		double numPoints = distances.length;
		//-- calc CV(h) after Worton (1995)
		// CV(h) = 1/(Pi*h^2*n) + 
		//         1/(4Pi*h^2*n^2) * Sum_i_n (Sum_j_n [exp(-d_ij^2/4h^2) - 4*exp(-d_ij^2/2h^2)])
		double sumVal = 0; int countD0 = 0;
		boolean precProblemReportedForThisH = false;
		for(int i=0; i < numPoints; i++){
			for(int j=0; j < numPoints; j++){
				if( i != j){// not sure, but maybe its good to leave the point itself out
					if(distances[i][j] > 0){
						double b1 = -1 * (distances[i][j] * distances[i][j]) / (4 * (h * h) );
						double b2 = -1 * (distances[i][j] * distances[i][j]) / (2 * (h * h) );
						double exp1 = Math.exp(b1);
						double exp2 = Math.exp(b2);
						if((exp1 == 0) || (exp2 == 0)){
							// the precision issue will especially happen for small values of h
							// e^(RealBigNumber) = ReallyReallyBigNumer (e.g. e^709) = 8.2*10^307
							// e^(-RBN) = ReallyReallySmallNumber (e.g. e^-709) = 1.2*10^-308
							// below: sum(RsmallNumber-RsmallNumber) = sum(almostZeroValues) 
							//   => so, the effect on the final value may be small, i.e. neglectable,
							//      since the graph still shows that for small h we have the largest values
							if (precProblemReportedForThisH == false){ //use this condition to avoid unnecessary output
								System.out.println("KernelDensityUtil.calculateCVScore: one or both of exp returns zero - we may have a precision problem. h: " + h + "; exp1: " + exp1 + "; exp2: " + exp2);								
								precProblemReportedForThisH = true;								
							}
							// it occurs that the minimum value may be affected or not by the precision 
							// problem, e.g. for one dataset (13 points) we got max affected h = 292 whereas
							// the calculated minima was h=284...ups
						}
						double sum = exp1 - 4 * exp2;
						sumVal = sumVal + sum;
					}
					else{
						countD0++;
						//System.out.println("calculateCVScore: found dist=0");
					}
				}
			}
		}
		double numPointsCorr = numPoints - countD0;
		double const1 = 1.0 / ( Math.PI * (h*h) * numPointsCorr ); 
		double const2 =	1.0 / ( 4 * Math.PI * (h*h) * (numPointsCorr * numPointsCorr) );
		double score = const1 + const2 * sumVal;
		if (countD0 > 0){
			System.out.println("calculateCVScore: found dist=0 in " + countD0 + " cases for h = " + h);
		}
		return score;
	}

	/**
	 * generates an STR tree with all point features
	 * @param fc a FeatureCollection of points
	 * @return returns the tree with point features only
	 */
	public static STRtree createSTRreeofPoints(FeatureCollection fc) {
		STRtree tree = new STRtree();
		List<Feature> features = fc.getFeatures();
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if(f.getGeometry() instanceof Point){
				tree.insert(f.getGeometry().getEnvelopeInternal(), f);
			}
		}
		return tree;
	}
	
	/**
	 * Calculates the density value that separates the home range core area. The method used has been 
	 * described in Seaman and Powell (1990), but see also RA Powell (2000) and PN Laver (2005).<br>
	 * Note, this code is a copy from the CreateCoreAreaFromSelectedImageLayerPlugIn - so updates
	 * need to be done in both classes.
	 * @param homeRangeCellValues the cell values containing density
	 * @param cellSize
	 * @return the contouring/density value that defines the core area.
	 */
	public static double calculateCoreAreaProbability(double[] homeRangeCellValues, double cellSize){
		//sort the array from smallest to largest
		double[] densValues = DoubleArray.sort(homeRangeCellValues);
		int numCells = homeRangeCellValues.length;
		//-- make descending
		double[] valuesN = new double[numCells];
		for (int j = 0; j < densValues.length; j++) {//rows
			valuesN[j] = densValues[densValues.length-1-j];
		}
		densValues = valuesN;
		//-- calculate the area
		double cellArea = cellSize * cellSize;
		double[] area = new double[densValues.length];
		area[0] = cellArea;
		for (int j = 1; j < densValues.length; j++) {//rows
			area[j] = area[j-1] + cellArea;
		}
		//-- normalize [0..100]
		double maxDenseValue = DoubleArray.max(densValues);
		double maxAreaValue = DoubleArray.max(area);
		if(maxDenseValue != 0){
			densValues = LinearAlgebra.times(densValues, 100 * 1.0/maxDenseValue );
		}
		if(maxAreaValue != 0){
			area = LinearAlgebra.times(area, 100 * 1.0/maxAreaValue);
		}
		//-- get the probability value for the core HR
		//-- create the random use graph/line 
		double[][] datas2 = new double [densValues.length][2];
		for (int j = 0; j < densValues.length; j++) {			
			datas2[j][0] = densValues[j];
			datas2[j][1] = 100-densValues[j];			
		}
		//-- difference between both
		//double[][] diffs = new double [probabilityValuesInPercent.length][2];
		double[] diffsV = new double [densValues.length];
		for (int j = 0; j < densValues.length; j++) {			
			double diff = (100-densValues[j]) - area[j];
			diffsV[j] = diff;
			
		}
		int maxDiffIdx = DoubleArray.maxIndex(diffsV);
		double maxDiff = diffsV[maxDiffIdx];
		double diffProb = densValues[maxDiffIdx];
		//-- scale probability in % from [0..1] and derive 
		//   correct probability value p=1-x
		double coreDensityVal = maxDenseValue * diffProb/100.00; 
		return coreDensityVal;
	}
}
