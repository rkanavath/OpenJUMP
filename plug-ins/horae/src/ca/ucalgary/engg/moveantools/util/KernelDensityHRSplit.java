/*****************************************************
 * created:  		16.Nov.2009
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * The class contains methods to calculate the Kernel density by choosing h (the bandwidth) 
 * automatically via the LSCV approach.
 *  
 *****************************************************/
package ca.ucalgary.engg.moveantools.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.math.array.DoubleArray;
import org.openjump.core.rasterimage.RasterImageLayer;

import ca.ucalgary.engg.moveantools.util.optimization.FMinMethods;
import ca.ucalgary.engg.moveantools.util.optimization.GoldenSearchOptimization;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.parameters.Parameter;
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;

/**
 * The class contains methods to calculate the Kernel density by choosing h (the bandwidth) 
 * automatically via the HomeRange Split approach. This method is called "h_ad-hoc" by Berger
 * and Gese (2007, J. of. A. Ecol.), KL Schuler et al. (2010/11 - incl. JG Kie), and 
 * JG Kie (Idaho State University) cited in CN Jacques et al (2009, J. of Mam.)
 * 
 * @author sstein
 *
 */
public class KernelDensityHRSplit{

	double hOpt = 0;

	double hRef = 0;
	//int pointNumToLeaveOut = 1; //not needed > LSCV works different then normal CV
	//int leaveOutIterations = 1; //not needed > LSCV works different then normal CV
	int numberOfHs = 50;
	
	double hMax = 0;
	double hMin = 0;
	double hMinMultiplierForHref = 0.01;
	double hMaxMultiplierForHref = 3.0; // set to 3.0*x because I got a 3/20 cases where 2*href had already 3 patches
	double stepSizeThreshold = 10;
	double stepSizeThresholdMultiplier = 2.0; //to be multiplied with the cellsize
											  //take 2.0 so there will be still a connection

	ArrayList<Double> calledHValues = new ArrayList<Double>();
	ArrayList<Integer> calledFunctionScores = new ArrayList<Integer>();
	
	double[] gridHValues = null;
	boolean[] gridScoresValues = null;
	double gridHBest = 0;
	boolean stoppedOptimizationLoop = false;

	STRtree rtree = null;
	
	AnalysisExtent gExtent = null;
	RasterImageLayer outputRaster = null;
	
	TaskMonitor tmonitor = null;
	
	/**
	 * TODO: update this description
	 * calculates the KernelDensity by selecting an appropriate h value (i.e. Bandwidth)
	 * by checking when the calculate HomeRange 99.5 percent contour splits.
	 * Standard values by now are <br>
	 * hStartMultiplierForHref = 0.01, <br>
	 * hEndMultiplierForHref=1.5, <br>
	 * and for "grid" calculations to evaluate the function to optimize: numberOfHs = 50. <br>
	 * The derived value for h depends on the Kernel used, i.e. needs to be multiplied by AK
	 * for other kernel types [AK(biweight) = 2.78].<br>  
	 * A number of results can be obtained with the Getters.    
	 * @param points the point dataset
	 * @param vAttribute the attribute which contains weight values
	 * @param cellSize of the output raster
	 * @param kernelType depending on the once implemented on Sextante (0: Sextante Standard Kernel: Gaussian, 1: Biweight )
	 * @param removeSinglePoints if yes, it removes possible outliers. This is based on chosen bandwidth h) 
	 * @param calculateGridValues if yes, score function values are calculated for grid points to get a picture of the score function
	 * @param context the OpenJUMP plugin context (can be null)
	 * @param monitor the OpenJUMP monitor to deliver status messages and cancel (can be null)
	 * @return returns null if something went wrong (i.e. no point geometry layer), otherwise the kernel density raster is returned.
	 * @throws GeoAlgorithmExecutionException
	 * @throws IOException
	 */
	public RasterImageLayer calculateHRSplitKernelDensity(Layer points,
			String vAttribute, double cellSize, int kernelType, 
			boolean removeSinglePoints, boolean calculateGridValues,
			PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException, IOException {
		
		this.tmonitor = monitor;
		
		OpenJUMPVectorLayer layer = new OpenJUMPVectorLayer();
		layer.create(points);

		//- get href for the full dataset (not for removed points) 
		if(monitor != null){
			monitor.report("calculate h_ref");
		}
		this.hRef = KernelDensityUtil.calculateHref(points);
		this.hMax = this.hMaxMultiplierForHref * this.hRef;
		this.hMin = this.hMinMultiplierForHref * this.hRef;	
		if(this.hMin < (1*cellSize)){ ///otherwise we may get empty rasters
			this.hMin = 1*cellSize;
		}
		//- This will create a grid extent that has the full extent of our
		// input layer. Note, the layer extent is generated before the points are
		// filtered, so we keep the original size
		this.gExtent = new AnalysisExtent(layer);
		//=======
		//- calculate a spatial index for all points (this way we are faster later, when
	    //  using testing if the polygon contains several h)
		FeatureCollection fc = points.getFeatureCollectionWrapper().getUltimateWrappee();
		if(monitor != null){
			monitor.report("create spatial index");
		}
		STRtree tree = KernelDensityUtil.createSTRreeofPoints(fc);
		if(tree.size() == 0){
			if(context != null){
				context.getWorkbenchFrame().warnUser("no point layer! stopped!");
			}
			return null;
		}
		else{
			this.rtree = tree;
		}
		//==============================================================
		if (calculateGridValues){
			//TODO
			//set this.gridHBest=...
		}
	    this.stepSizeThreshold = this.stepSizeThresholdMultiplier * cellSize;
		if(monitor != null){
			monitor.report("start searching h");
		}
	    /**
	    this.calledHValues.clear();
	    this.calledFunctionScores.clear();
		//-- this can be deleted later if h is chosen in an optimization process
	    double hRange = this.hMax - this.hMin;
		double deltaH = hRange/this.numberOfHs;
		//--
	    int numRegions = 0; double hrunning = this.hMax;
	    double optH = this.hMax; int loopCount = 0; 
	    while(numRegions < 2){
	    	loopCount = loopCount + 1;
			if(monitor != null){
				monitor.report("loop: " + loopCount + " - max: " + this.numberOfHs + " h: " + hrunning);
			}
	    	this.calledHValues.add(new Double(hrunning));
	    	numRegions = this.calculateHRSplitScore(hrunning, tree, removeSinglePoints, 
	    			cellSize, layer, vAttribute, kernelType, context);
	    	if(numRegions < 2){
	    		optH = hrunning;
	    	}
	    	this.calledHValues.add(new Double(hrunning));
	    	this.calledFunctionScores.add(new Integer(numRegions));
	    	//TODO: implement  procedure for choosing h (i.e. half steps)
	    	//      now it just uses the grid approach
	    	hrunning = hrunning - deltaH;
	    	
	    	//-- check in case we reach the boundary and 
	    	//   loop needs to be finished
	    	if (hrunning < this.hMin){
	    		numRegions = 2;
	    		this.stoppedOptimizationLoop = true;
	    	}
	    }
	    //old stuff - to remember:
	    //optH = GoldenSearchOptimization.fmin(this.hStart, this.hEnd, this, this.stepSizeThreshold);
	    **/
	    double optH = this.findHoptRegionSplit(this.hMin, this.hMax, this.stepSizeThreshold, this.rtree, 
	    		removeSinglePoints, cellSize, layer, vAttribute, kernelType, context, monitor);
	    this.hOpt = optH;

		//==============================================================
		//-- create the final raster, for hOpt
	    if(this.hOpt > cellSize){
	    	// note need to use the biweight kernel here, due to hOpt 
	    	// so the correct value for KernelType is hopefully set by gui functions
		    OpenJUMPRasterLayer raster = this.calculateKernelDensity(layer, vAttribute, this.hOpt, cellSize, 
		    		kernelType, this.gExtent, removeSinglePoints, context);
			this.outputRaster = (RasterImageLayer)raster.getBaseDataObject();
			//-- make the layer name informative
			String oldLayerName = this.outputRaster.getName();
			double roundedHOpt = ((int)(100*optH))/100.0;			 
			double hRefRounded = ((int)(100*this.hRef))/100.0;
			//-- need to switch off firing of events otherwise 
			//   this results in an exception with the GUI
			this.outputRaster.getLayerManager().setFiringEvents(false);
			this.outputRaster.setName(oldLayerName + "_h_" + roundedHOpt);
			this.outputRaster.getLayerManager().setFiringEvents(true);

	    	if(context != null){
				if(calculateGridValues){
					context.getWorkbenchFrame().warnUser("h_ref: " + hRefRounded + "; h_opt: " + roundedHOpt + "; h_min(Biweight): " + gridHBest);
				}
				else{
					context.getWorkbenchFrame().warnUser("h_ref: " + hRefRounded + "h_opt: " + roundedHOpt);
				}
			}
	    }
	    else{
	    	if(context != null){
	    		context.getWorkbenchFrame().warnUser("problem: h_best < cellSize");
	    	}
	    }
		return this.outputRaster;
	}
	
	
	/**
	 * calculates the kernel density
	 * @param vectorLayer
	 * @param vAttribute
	 * @param distance
	 * @param cellSize
	 * @param kernelType
	 * @param extent
	 * @param removeSinglePoints
	 * @param context
	 * @return a Sextante raster (IRasterLayer), note: can be null!
	 * @throws GeoAlgorithmExecutionException
	 * @throws IOException
	 */
	public static OpenJUMPRasterLayer calculateKernelDensity(OpenJUMPVectorLayer vectorLayer, String vAttribute, double distance, double cellSize, int kernelType, 
			AnalysisExtent extent, boolean removeSinglePoints, PlugInContext context)
			throws GeoAlgorithmExecutionException, IOException {

		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		
		//-- note, removing single points may affect the scores, and with it the optimization
		OpenJUMPVectorLayer filteredLayer = new OpenJUMPVectorLayer();
		if(removeSinglePoints){
			Layer ojLayer = (Layer) vectorLayer.getBaseDataObject();
			FeatureCollection pointFc = ojLayer.getFeatureCollectionWrapper().getUltimateWrappee();
			FeatureCollection filteredPointsFc = KernelDensityUtil.removePoints(pointFc, 2*distance, null);
			Layer filteredPoints = new Layer(vectorLayer.getName() + "_filtered", context.getLayerManager().generateLayerFillColor(),
									filteredPointsFc, context.getLayerManager());
			filteredLayer.create(filteredPoints);
		}
		else{
			filteredLayer = vectorLayer;
		}
		//--
		KernelDensitySextanteModifiedAlgorithm alg = new KernelDensitySextanteModifiedAlgorithm();
		
		ParametersSet params = alg.getParameters();
		Parameter layerParam = (Parameter)params.getParameter(KernelDensitySextanteModifiedAlgorithm.LAYER);
		boolean worked = layerParam.setParameterValue(filteredLayer);
		if(worked){
			params.getParameter(KernelDensitySextanteModifiedAlgorithm.DISTANCE).setParameterValue(new Double(distance));
			params.getParameter(KernelDensitySextanteModifiedAlgorithm.FIELD).setParameterValue(filteredLayer.getFieldIndexByName(vAttribute));
			params.getParameter(KernelDensitySextanteModifiedAlgorithm.KERNEL).setParameterValue(new Integer(kernelType));
			
			//-- we will use a cell size of x meters
			extent.setCellSize(cellSize);
			//-- enlarge by kernel size (otherwise the outer points are missed)
			int ntimes = (int)Math.ceil(distance/cellSize) + 2; // plus two reserve
			for( int i = 0; i < ntimes; i++){
				extent.enlargeOneCell();
			}
			
			//-- And now we set the extent as the one to use to create new raster
			// layers within the rasterizing algorithm.
			alg.setAnalysisExtent(extent);
			
			OutputObjectsSet outputs = alg.getOutputObjects();
			Output raster = outputs.getOutput(KernelDensitySextanteModifiedAlgorithm.DENSITY);
			//monitor.report("computation");
			alg.execute(null, outputFactory);
			
			//monitor.report("retrieving results");
			IRasterLayer result = (IRasterLayer)raster.getOutputObject();
			//RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();
			OpenJUMPRasterLayer ojRasterLayer = (OpenJUMPRasterLayer)result; // let's see if this cast works
			return ojRasterLayer;
		}
		else{
			return null;
		}
		
	}
	
	/**
	 * calculates a range of scores for h, i.e. regions that are produced. Outliers (i.e. regions 
	 * with one data point) are removed from the score.
	 * @param hMin
	 * @param hMax
	 * @param numberOfSteps the number of h's for which the function score should be calculated
	 * @param strtree a tree/spatial index with all points 
	 * @param cellSize of raster 
	 * @param monitor can be null
	 * @return an Array of double[]. The first double[] entry contains the hValues and the second the function scores (number of regions)
	 */
	/** TODO: adapt
	public ArrayList<double[]> calculateHRSplitScoreValues(double hMin, double hMax, int numberOfSteps, 
													STRtree strtree, double cellSize, TaskMonitor monitor){
		ArrayList<double[]> returnVals	= new ArrayList<double[]>();
		double[] hValues = new double[numberOfSteps];
		double[] functionScores = new double[numberOfSteps];
		double hRange = hMax - hMin;
		double deltaH = hRange/numberOfSteps;
		for (int i = 0; i < numberOfSteps; i++) {
			double h = hMin + (i * deltaH);
			hValues[i] = h;
			int score = this.calculateHRSplitScore(h, strtree, true, cellSize, null);
			functionScores[i] = (double)score;
			if(monitor != null){
				monitor.report("Searching h_best: Loop " + (i+1) + " of " + numberOfSteps + "; h = " + h);
				if (monitor.isCancelRequested()){
					returnVals.add(hValues);
					returnVals.add(functionScores);
					return returnVals;
				}
			}
		}
		returnVals.add(hValues);
		returnVals.add(functionScores);
		return returnVals;
	}
	**/
	
	/**
	 * calculates the number of home range regions created from 
	 * the (?99.5 percent?) contour. Outliers will create 
	 * regions with only one data point. 
	 * @param h
	 * @param strtree
	 * @param neglectOutliers
	 * @param cellSize
	 * @param context can be null
	 * @return the number of home range regions
	 */
	private int calculateHRSplitScore(double h, STRtree strtree, boolean neglectOutliers, 
			double cellSize, OpenJUMPVectorLayer layer, String vAttribute, 
			int kernelType, PlugInContext context)throws GeoAlgorithmExecutionException, IOException {
		
		double probability = 0.995;
		OutputFactory outputFactory2 = new OpenJUMPOutputFactory(context.getWorkbenchContext());	
		int numRegions = -1;
		
		//-- create KDE raster 
	    OpenJUMPRasterLayer raster = this.calculateKernelDensity(layer, vAttribute, h, cellSize, 
	    		kernelType, this.gExtent, neglectOutliers, context);
	    System.gc();
	    
		RasterImageLayer outputRaster = (RasterImageLayer)raster.getBaseDataObject();
		//context.getLayerManager().addLayerable("KDE rasters", outputRaster);
		
		//-- derive xx percent contour
		double[] mappingResult = KernelDensityUtil.calculateContourHeightValueFromProbabilityValue(raster, probability);
		double contourValue = mappingResult[0];
		double realProbability = mappingResult[1];

		ContourLinesAlgorithm alg = new ContourLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(ContourLinesAlgorithm.LAYER).setParameterValue(raster);
		params.getParameter(ContourLinesAlgorithm.DISTANCE).setParameterValue(new Double(1));
		params.getParameter(ContourLinesAlgorithm.MAX).setParameterValue(new Double(contourValue));
		params.getParameter(ContourLinesAlgorithm.MIN).setParameterValue(new Double(contourValue));
		
		alg.execute(null, outputFactory2);
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		//-- get memory space
		alg = null;
		System.gc();
		//--
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);
		//-- convert the contour in polygons
		FeatureCollection polys = FeatureConversionUtils.calculatePolysFromContours(fd, null, context);
		//-- get memory space
		fd = null;
		System.gc();
		//-delete the files
		String rasterFileName = raster.getFilename();
		String rasterFileNameTfw = raster.getFilename(); rasterFileNameTfw = rasterFileNameTfw.replace(".tif", ".tfw");
		String contourFileName = resultOJLayer.getFilename();
		String contourFileNameShx = resultOJLayer.getFilename(); contourFileNameShx = contourFileNameShx.replace(".shp", ".shx");
		String contourFileNameDbf = resultOJLayer.getFilename(); contourFileNameDbf = contourFileNameDbf.replace(".shp", ".dbf");
		try{
			File rasterFile = new File(rasterFileName);
			boolean rasterDeleted = rasterFile.delete();
			File rasterFileTfw = new File(rasterFileNameTfw); rasterFileTfw.delete();
			File vectorFile = new File(contourFileName);
			boolean vectorDeleted = vectorFile.delete();
			File vectorFileShx = new File(contourFileNameShx); vectorFileShx.delete();
			File vectorFileDbf = new File(contourFileNameDbf); vectorFileDbf.delete();
			System.out.println("deleted rasterFile: " + rasterDeleted + ", deleted contourFile: " + vectorDeleted);
		}
		catch(Exception e){
			e.printStackTrace();
			//eat
		}
		System.gc();
		//--
		numRegions = polys.size();
		//-- vis
		if(context != null){
			context.addLayer(StandardCategoryNames.RESULT, "99p_contourpoly_h_" + h, polys);
		}
		if (numRegions > 1){
			//-- filter the regions for outliers
			//   Note, this should actually not be necessary, since the KDE function earlier 
			//   excludes outlier already.
			int regionsWithOnePoint = 0;
			for (Iterator iterator = polys.iterator(); iterator.hasNext();) {
				Feature feature = (Feature) iterator.next();
				Geometry polyGeom = feature.getGeometry();
				//-- check if this region has more than one point
				List candidates = strtree.query(polyGeom.getEnvelopeInternal());
				int numbTotalIntersects = 0;
				Iterator iterator2 = candidates.iterator();
				boolean cont = true;
				if(candidates.size() == 0){
					cont = false;
					System.out.println("found region with no points?, h = " + h);
				}
				while (cont) {
					Feature pointF = (Feature) iterator2.next();
					if (pointF.getGeometry().intersects(polyGeom)){
						numbTotalIntersects = numbTotalIntersects + 1;
					}
					cont=iterator2.hasNext();
					//don't need to continue searching if more than 2 are found
					if (numbTotalIntersects > 2){
						cont = false;
					}
				}
				if(numbTotalIntersects <= 1 ){
					regionsWithOnePoint = regionsWithOnePoint + 1;
					System.out.println("found region with one point, h = " + h);
				}
			}
			numRegions = numRegions - regionsWithOnePoint;
		}
		//--
		return numRegions;
	}
	
	private double findHoptRegionSplit(double hMin, double hMax, double threshold, STRtree rtree, 
			boolean removeSinglePoints, double cellSize, OpenJUMPVectorLayer layer, 
			String vAttribute, int kernelType, PlugInContext context, TaskMonitor monitor) 
						throws GeoAlgorithmExecutionException, IOException {
		ArrayList<Double> testedHs = new ArrayList<Double>();
		ArrayList<Integer> scores = new ArrayList<Integer>();
		double hfound = hMax;
		//check if the split happens in between the values
		if(monitor != null){
			monitor.report("test h_min: " + hMin);
		}
		int valmin = this.calculateHRSplitScore(hMin, rtree, removeSinglePoints, 
    			cellSize, layer, vAttribute, kernelType, context);
		if(monitor != null){
			monitor.report("test h_max: " + hMax + " (hMin regions: " + valmin + ")");
		}
		int valmax = this.calculateHRSplitScore(hMax, rtree, removeSinglePoints, 
    			cellSize, layer, vAttribute, kernelType, context);
		//if ((valmax == 1) && (valmin > 1)){
		if (valmax == 1){ //not sure if valmin > 1 condition is necessary, since we filter outliers 
			// everything is ok, start with optimization
			boolean cont = true;
			double lastpos = hMax; double newpos = hMax;
			double minpos = hMin; double maxpos = hMax;
			int minposVal = valmin; int maxposVal = valmax;
			int lastposVal = valmax; int newposVal = valmax;
			double distanceRemaining = maxpos - minpos;
			while(cont){
				//-- get new position at 2/3 of interval
				double dist = maxpos - minpos;
				newpos = minpos + (dist * 2.0/3.0);
				if(monitor != null){
					monitor.report("test h: " + newpos + " old test: h = " + lastpos + " regions: " + lastposVal);
					if (monitor.isCancelRequested()){
						context.getWorkbenchFrame().warnUser("stopped");
						return lastpos;
					}
				}
				newposVal = this.calculateHRSplitScore(newpos, rtree, removeSinglePoints, 
		    			cellSize, layer, vAttribute, kernelType, context);
				System.gc();
				testedHs.add(newpos); scores.add(newposVal);
				if (newposVal > 1){
					// split occured, this will be the new lower bound
					minpos = newpos;
					minposVal = newposVal;
				}
				else{
					//split did not occur, move upper bound
					maxpos = newpos;
					maxposVal = newposVal;
				}
				//-- this is not really used but for tracing
				lastpos = newpos;
				lastposVal = newposVal;
				//-- check if continuing
				distanceRemaining = maxpos - minpos;
				if(distanceRemaining < threshold){
					cont = false;
					hfound = maxpos; //take the upper bound
				}
				else{
					cont = true;
				}
			}			
		}
		else{
			context.getWorkbenchFrame().warnUser("r(h_min)= " + valmin + "; r(h_max)= " + valmax + "; no split in initial search region");
			System.out.println("r(h_min)= " + valmin + "; r(h_max)= " + valmax + "; no split in initial search region");
		}
		
		return hfound;
	}
	
	
	
	//================= getters & setters ================================
	/**
	 * 
	 * @return
	 */
	public double getOptimalH() {
		return hOpt;
	}
	
	/**
	 * 
	 * @return
	 */
	public double getHref() {
		return hRef;
	}
	
	public double[] getGridHValues() {
		return gridHValues;
	}

	public boolean[] getGridScoresValues() {
		return gridScoresValues;
	}

	public double getGridHBest() {
		return gridHBest;
	}
	
	/**
	 * H_max
	 * @return
	 */
	public double gethStart() {
		return hMax;
	}

	/**
	 * H_min
	 * @return
	 */
	public double gethEnd() {
		return hMin;
	}

	public int getGridNumberOfHs() {
		return numberOfHs;
	}
	
	/**
	 * 
	 * @return tested h values. The scores for those values can be obtained with getScores().
	 */
	public double[] gethValuesFromOptimization() {
		double[] vals = new double[calledHValues.size()];
		int i=0;
		for (Iterator iterator = calledHValues.iterator(); iterator.hasNext();) {
			double val = (Double) iterator.next();
			vals[i] = val;
			i++;
		}
		return vals;
	}

	/**
	 * 
	 * @return the score (number of regions)
	 */
	public int[] getScoresFromOptimization() {
		int[] vals = new int[calledFunctionScores.size()];
		int i=0;
		for (Iterator iterator = calledFunctionScores.iterator(); iterator.hasNext();) {
			int val = (Integer) iterator.next();
			vals[i] = val;
			i++;
		}
		return vals;
	}

	/**
	 * 
	 * @return the optimization stopping criterion calculated from
	 * StepSizeThresholdMultiplier * CellSize 
	 */
	public double getStepSizeThreshold() {
		return stepSizeThreshold;
	}

	/**
	 * used to calculate the optimization stopping criterion
	 * StepSizeThresholdMultiplier * CellSize 
	 * @return
	 */
	public double getStepSizeThresholdMultiplier() {
		return stepSizeThresholdMultiplier;
	}
	
	
	/**
	 * 
	 * @return the point-point distance matrix used for the calculation of LSCV
	 */
	public STRtree getSTRtreeForPoints() {
		return this.rtree;
	}

	/**
	 * set the number of grid points for which score function values
	 * should be calculated. I.e. the more points the better the
	 * impression of how the score function looks like. 
	 * @param numberOfHs
	 */
	public void setNumberOfHs(int numberOfHs) {
		this.numberOfHs = numberOfHs;
	}

	/**
	 * Used to determin the search interval for H_opt, i.e.<br>
	 * H_start = hStartMultiplierForHref * h_ref, <br>
	 * where h_ref is obtained based on the variance in x,y according
	 * to Worton (1995).
	 * @param hStartMultiplierForHref (default is set to 0.01)
	 */
	public void sethStartMultiplierForHref(double hStartMultiplierForHref) {
		this.hMinMultiplierForHref = hStartMultiplierForHref;
	}

	/**
	 * Used to determin the search interval for H_opt, i.e.<br>
	 * H_end = hEndMultiplierForHref * h_ref, <br>
	 * where h_ref is obtained based on the variance in x,y according
	 * to Worton (1995).
	 * @param hEndMultiplierForHref (default is set to 0.7) Worton (1995) 
	 * gives an example value of 1.5 (p.795)  
	 */
	public void sethEndMultiplierForHref(double hEndMultiplierForHref) {
		this.hMaxMultiplierForHref = hEndMultiplierForHref;
	}

	/**
	 * used to calculate the optimization stopping criterion
	 * StepSizeThresholdMultiplier * CellSize 
	 * @param stepSizeThresholdMultiplier
	 */
	public void setStepSizeThresholdMultiplier(double stepSizeThresholdMultiplier) {
		this.stepSizeThresholdMultiplier = stepSizeThresholdMultiplier;
	}
	
	/**
	 * 
	 * @return was the optimization process stopped since the lower bound (H = H_End) has been reached
	 */
	public boolean isStoppedOptimizationLoop() {
		return stoppedOptimizationLoop;
	}
	
}
