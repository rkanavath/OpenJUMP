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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import org.math.array.DoubleArray;
import org.openjump.core.rasterimage.RasterImageLayer;

import ca.ucalgary.engg.moveantools.util.optimization.FMinMethods;
import ca.ucalgary.engg.moveantools.util.optimization.GoldenSearchOptimization;

import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.parameters.Parameter;

/**
 * The class contains methods to calculate the Kernel density by choosing h (the bandwidth) 
 * automatically via the LSCV approach.
 * 
 * @author sstein
 *
 */
public class KernelDensityLSCV implements FMinMethods{

	double hOpt = 0;
	double hOptBiweight = 0;

	double hRef = 0;
	//int pointNumToLeaveOut = 1; //not needed > LSCV works different then normal CV
	//int leaveOutIterations = 1; //not needed > LSCV works different then normal CV
	int numberOfHs = 50;
	
	double hStart = 0;
	double hEnd = 0;
	double hStartMultiplierForHref = 0.01;
	double hEndMultiplierForHref = 1.5;
	double stepSizeThreshold = 10;
	double stepSizeThresholdMultiplier = 1.0/8.0; //to be multiplied with the cellsize

	ArrayList<Double> calledHValues = new ArrayList<Double>();
	ArrayList<Double> calledFunctionScores = new ArrayList<Double>();
	
	double[] gridHValues = null;
	double[] gridScoresValues = null;
	double gridHBest = 0;
	
	double[][] distances = null;
	
	AnalysisExtent gExtent = null;
	RasterImageLayer outputRaster = null;
	
	TaskMonitor tmonitor = null;
	
	/**
	 * calculates the KernelDensity by selecting an appropriate h value (i.e. Bandwidth)
	 * by utilizing the Least Squares Cross Validation Approach described in Silverman (1986) 
	 * and BJ Worton (1995) etc. Several Values used in this method can/should be set 
	 * additionally using the Setter methods. Standard values by now are <br>
	 * hStartMultiplierForHref = 0.01, <br>
	 * hEndMultiplierForHref=1.5, <br>
	 * and for "grid" calculations to evaluate the function to optimize: numberOfHs = 50. <br>
	 * According to Lavender (ABode Manual) the derived value for h needs to be multiplied by AK,
	 * with respect to the kernel type used [AK(biweight) = 2.78].<br>  
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
	public RasterImageLayer calculateLSCVKernelDensity(Layer points,
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
		this.hStart = this.hStartMultiplierForHref * this.hRef;
		this.hEnd = this.hEndMultiplierForHref * this.hRef;	
		//- This will create a grid extent that has the full extent of our
		// input layer. Note, the layer extent is generated before the points are
		// filtered, so we keep the original size
		this.gExtent = new AnalysisExtent(layer);
		//=======
		//- calculate a distance matrix for all points (this way we are faster later, when
	    //  using different h)
		FeatureCollection fc = points.getFeatureCollectionWrapper().getUltimateWrappee();
		if(monitor != null){
			monitor.report("calculate point-to-point distances");
		}
		double[][] dists = KernelDensityUtil.createPointDistanceMatrix(fc);
		if(dists == null){
			if(context != null){
				context.getWorkbenchFrame().warnUser("no point layer! stopped!");
			}
			return null;
		}
		else{
			this.distances = dists;
		}
		//==============================================================
		if (calculateGridValues){
			//-- calculate some values to get an impression of how the function
			//   looks like and to compare if the optimization did work
			ArrayList<double[]> testFunctionVals = this.calculateLSCVScoreFunctionValues(this.hStart, this.hEnd, 
					this.numberOfHs, this.distances, monitor);
		    this.gridHValues = testFunctionVals.get(0);
		    this.gridScoresValues = testFunctionVals.get(1);
			int minIndex = DoubleArray.minIndex(this.gridScoresValues);
		    double testhBest = this.gridHValues[minIndex];
		    this.gridHBest = testhBest;
		}
		//==== use optimization (golden section search) ===
		// see Wikipedia and WH Press et al. 2007
	    this.calledHValues.clear();
	    this.calledFunctionScores.clear();
	    this.stepSizeThreshold = this.stepSizeThresholdMultiplier * cellSize;
	    double optH = GoldenSearchOptimization.fmin(this.hStart, this.hEnd, this, this.stepSizeThreshold);
	    this.hOpt = optH;
	    this.hOptBiweight = this.hOpt * KernelDensityUtil.A_K;
		//-- visualize the scores in a plot
	    /*
		double hRefScore = KernelDensityUtil.calculateCVScore(this.hRef, this.distances);
	    ShowScores myScorePlot = new ShowScores(this.gridHValues, this.gridScoresValues, 
	    		this.hRef, hRefScore, this.gethValues(), this.getScores());
		*/
		//==============================================================
		//-- create the final raster, for hOpt
	    if(this.hOptBiweight > cellSize){
	    	// note need to use the biweight kernel here, due to hOpt 
	    	// so the correct value for KernelType is hopefully set by gui functions
		    OpenJUMPRasterLayer raster = this.calculateKernelDensity(layer, vAttribute, this.hOptBiweight, cellSize, 
		    		kernelType, this.gExtent, removeSinglePoints, context);
			this.outputRaster = (RasterImageLayer)raster.getBaseDataObject();
			//-- make the layer name informative
			String oldLayerName = this.outputRaster.getName();
			double roundedHBestBiweight = 0;
			if(calculateGridValues){
				roundedHBestBiweight = ((int)(100*this.gridHBest*KernelDensityUtil.A_K))/100.0;
			}
			double roundedHOpt = ((int)(100*optH))/100.0;
			double roundedHOptBiweight = ((int)(100*this.hOptBiweight))/100.0;
			 
			double hRefBiweightRounded = this.hRef * KernelDensityUtil.A_K;
			hRefBiweightRounded = ((int)(100*hRefBiweightRounded))/100.0;
			//-- need to switch off firing of events otherwise 
			//   this results in an exception with the GUI
			this.outputRaster.getLayerManager().setFiringEvents(false);
			this.outputRaster.setName(oldLayerName + "_h(K2)_" + roundedHOptBiweight);
			this.outputRaster.getLayerManager().setFiringEvents(true);

	    	if(context != null){
				if(calculateGridValues){
					context.getWorkbenchFrame().warnUser("h_ref(Biweight): " + hRefBiweightRounded + "; h_opt(Biweight): " + roundedHOptBiweight + "; h_min(Biweight): " + roundedHBestBiweight);
				}
				else{
					context.getWorkbenchFrame().warnUser("h_ref(Biweight): " + hRefBiweightRounded + "h_opt(Biweight): " + roundedHOptBiweight);
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
	 * Calculates objective function values for Least Squares Cross Validation Approach 
	 * described in Silverman (1986) and BJ Worton (1995) etc. 
	 * Several Values used in this method can/should be set additionally using the Setter 
	 * methods. Standard values by now are <br>
	 * hStartMultiplierForHref = 0.01, <br>
	 * hEndMultiplierForHref=1.5, <br>
	 * and for "grid" calculations to evaluate the function to optimize: numberOfHs = 50. <br>
	 * According to Lavender (ABode Manual) the derived value for h needs to be multiplied by AK,
	 * with respect to the kernel type used [AK(biweight) = 2.78].<br>  
	 * A number of results can be obtained with the Getters.    
	 * @param points the point dataset
	 * @param cellSize of the output raster (used to stop the optimization process)
	 * @param calculateGridValues if yes, score function values are calculated for grid points to get a picture of the score function
	 * @param context the OpenJUMP plugin context (can be null)
	 * @param monitor the OpenJUMP monitor to deliver status messages and cancel (can be null)
	 * @return returns false if something went wrong (i.e. no point geometry layer).
	 */
	public boolean calculateLSCVFunction(Layer points, double cellSize,
			boolean calculateGridValues,
			PlugInContext context, TaskMonitor monitor){
		
		this.tmonitor = monitor;

		//- get href for the full dataset (not for removed points) 
		if(monitor != null){
			monitor.report("calculate h_ref");
		}
		this.hRef = KernelDensityUtil.calculateHref(points);
		this.hStart = this.hStartMultiplierForHref * this.hRef;
		this.hEnd = this.hEndMultiplierForHref * this.hRef;	
		//=======
		//- calculate a distance matrix for all points (this way we are faster later, when
	    //  using different h)
		FeatureCollection fc = points.getFeatureCollectionWrapper().getUltimateWrappee();
		if(monitor != null){
			monitor.report("calculate point-to-point distances");
		}
		double[][] dists = KernelDensityUtil.createPointDistanceMatrix(fc);
		if(dists == null){
			if(context != null){
				context.getWorkbenchFrame().warnUser("no point layer! stopped!");
			}
			return false;
		}
		else{
			this.distances = dists;
		}
		//==============================================================
		if (calculateGridValues){
			//-- calculate some values to get an impression of how the function
			//   looks like and to compare if the optimization did work
			ArrayList<double[]> testFunctionVals = this.calculateLSCVScoreFunctionValues(this.hStart, this.hEnd, 
					this.numberOfHs, this.distances, monitor);
		    this.gridHValues = testFunctionVals.get(0);
		    this.gridScoresValues = testFunctionVals.get(1);
			int minIndex = DoubleArray.minIndex(this.gridScoresValues);
		    double testhBest = this.gridHValues[minIndex];
		    this.gridHBest = testhBest;
		}
		//==== use optimization (golden section search) ===
		// see Wikipedia and WH Press et al. 2007
	    this.calledHValues.clear();
	    this.calledFunctionScores.clear();
	    this.stepSizeThreshold = this.stepSizeThresholdMultiplier * cellSize;
	    double optH = GoldenSearchOptimization.fmin(this.hStart, this.hEnd, this, this.stepSizeThreshold);
	    this.hOpt = optH;
	    this.hOptBiweight = this.hOpt * KernelDensityUtil.A_K;
		//-- visualize the scores in a plot
	    /*
		double hRefScore = KernelDensityUtil.calculateCVScore(this.hRef, this.distances);
	    ShowScores myScorePlot = new ShowScores(this.gridHValues, this.gridScoresValues, 
	    		this.hRef, hRefScore, this.gethValues(), this.getScores());
		*/
		//==============================================================
	    //-- create some output
	    double roundedHBestBiweight = 0;
	    if(calculateGridValues){
	    	roundedHBestBiweight = ((int)(100*this.gridHBest*KernelDensityUtil.A_K))/100.0;
	    }
	    double roundedHOpt = ((int)(100*optH))/100.0;
	    double roundedHOptBiweight = ((int)(100*this.hOptBiweight))/100.0;

	    if(context != null){
	    	if(calculateGridValues){
	    		context.getWorkbenchFrame().warnUser("h_opt(Normal): " + roundedHOpt + "; h_opt(Biweight): " + roundedHOptBiweight + "; h_min(Biweight): " + roundedHBestBiweight + "; h_ref: " + this.hRef);
	    	}
	    	else{
	    		context.getWorkbenchFrame().warnUser("h_opt(Normal): " + roundedHOpt + "; h_opt(Biweight): " + roundedHOptBiweight + "; h_ref: " + this.hRef);
	    	}
	    }
	    return true;
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
	public OpenJUMPRasterLayer calculateKernelDensity(OpenJUMPVectorLayer vectorLayer, String vAttribute, double distance, double cellSize, int kernelType, 
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
	 * calculates a range of scores for the LSCV function. Note, that point distances=0 are discarded 
	 * @param hMin
	 * @param hMax
	 * @param numberOfSteps the number of h's for which the function score should be calculated
	 * @param distances A matrix of distances between all points calculated with KernelDensityUtil.createPointDistanceMatrix(...)
	 * @param monitor can be null
	 * @return an Array of double[]. The first double[] entry contains the hValues and the second the function scores
	 */
	public ArrayList<double[]> calculateLSCVScoreFunctionValues(double hMin, double hMax, int numberOfSteps, 
													double[][] distances, TaskMonitor monitor){
		ArrayList<double[]> returnVals	= new ArrayList<double[]>();
		double[] hValues = new double[numberOfSteps];
		double[] functionScores = new double[numberOfSteps];
		double hRange = hMax - hMin;
		double deltaH = hRange/numberOfSteps;
		for (int i = 0; i < numberOfSteps; i++) {
			double h = hMin + (i * deltaH);
			hValues[i] = h;
			double score = KernelDensityUtil.calculateCVScore(h, distances);
			functionScores[i] = score;
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
	
	/**
	 * This is a convenience method to give the golden search optimization access
	 * to the function to be optimized. The function uses several class variables,
	 * and, hence, is called from calculateLSCVKernelDensity(...). 
	 * Therefore it is not a "public" function.
	 * @param x value for which the score is to be calculated
	 * @return LSCV function score value
	 */
	public double functionToMinimize(double x) {
		if(this.tmonitor != null){
			tmonitor.report("optimization step: " + this.calledFunctionScores.size() + ", h: " + x);
		}
		double fvalue = KernelDensityUtil.calculateCVScore(x, this.distances); 
		this.calledFunctionScores.add(fvalue);
		this.calledHValues.add(x);		
		return fvalue;
	}
	
	//================= getters & setters ================================
	/**
	 * 
	 * @return
	 */
	public double getOptimalH() {
		return hOpt;
	}
	
	public double gethOptimalHBiweightKernel() {
		return hOptBiweight;
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

	public double[] getGridScoresValues() {
		return gridScoresValues;
	}

	public double getGridHBest() {
		return gridHBest;
	}
	
	public double gethStart() {
		return hStart;
	}

	public double gethEnd() {
		return hEnd;
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
	 * @return the score
	 */
	public double[] getScoresFromOptimization() {
		double[] vals = new double[calledFunctionScores.size()];
		int i=0;
		for (Iterator iterator = calledFunctionScores.iterator(); iterator.hasNext();) {
			double val = (Double) iterator.next();
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
	public double[][] getDistances() {
		return distances;
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
		this.hStartMultiplierForHref = hStartMultiplierForHref;
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
		this.hEndMultiplierForHref = hEndMultiplierForHref;
	}

	/**
	 * used to calculate the optimization stopping criterion
	 * StepSizeThresholdMultiplier * CellSize 
	 * @param stepSizeThresholdMultiplier
	 */
	public void setStepSizeThresholdMultiplier(double stepSizeThresholdMultiplier) {
		this.stepSizeThresholdMultiplier = stepSizeThresholdMultiplier;
	}
	
	
	
    //-- not needed > LSCV works different then normal CV
//	public int getLeaveOutIterations() {
//		return leaveOutIterations;
//	}
//	
//	public int getNumPointsToLeaveOut() {
//		return pointNumToLeaveOut;
//	}

	
	/**
	 * this was my earlier, but not finished, programming. It turns out that 
	 * LSCV is not a real Cross Validation in the common sense (...I learned),
	 * i.e. no leave-x-out and no error function = (x_Refernce-x_Current) 
	 * But maybe I need this stuff later.
	 * @param points
	 * @param vAttribute
	 * @param cellSize
	 * @param kernelType
	 * @param removeSinglePoints
	 * @param context
	 * @param monitor
	 * @return
	 * @throws GeoAlgorithmExecutionException
	 * @throws IOException
	 **/
//	private RasterImageLayer calculateLSCVKernelDensityOLD(Layer points,
//			String vAttribute, double cellSize, int kernelType, boolean removeSinglePoints, PlugInContext context, TaskMonitor monitor) 
//			throws GeoAlgorithmExecutionException, IOException {
//		
//		OpenJUMPVectorLayer layer = new OpenJUMPVectorLayer();
//		layer.create(points);
//		
//		//======= some settings (TODO: generate setter at a later point)
//		int pointNumToLeaveOut = 1; //not needed > LSCV works different then normal CV
//		int leaveOutIterations = 30; //not needed > LSCV works different then normal CV
//		this.numberOfHs = 20;
//		//- get href for the full dataset (not for removed points) 
//		this.hRef = KernelDensityUtil.calculateHrefBiWeightKernel(points);
//		this.hStart = 0.01 * this.hRef;
//		this.hEnd = 10.0 * this.hRef;	
//		//- This will create a grid extent that has the full extent of our
//		// input layer. Note, the layer extent is generated before the points are
//		// filtered, so we keep the original size
//		this.gExtent = new GridExtent(layer);
//		//=======
//		/*==============================================================
//		*   create some scores : this is done just for initial testing
//		*   later we need to use a real optimization method, to find hBest
//		*   and remove single points multiple times 
//		================================================================*/
//		//TODO: remove the list variable, since it may take quite a bit of memory
//		//ArrayList<OpenJUMPRasterLayer> rasters = new ArrayList<OpenJUMPRasterLayer>();
//		//- get deltaH
//		double hRange = this.hEnd - this.hStart;
//		double deltaH = hRange/this.numberOfHs;
//		this.hValues = new double[this.numberOfHs];
//		this.functionScores = new double[this.numberOfHs];
//		for (int i = 0; i < this.numberOfHs; i++) {
//			double h = this.hStart + (i * deltaH);
//			this.hValues[i] = h;
//			//not needed > LSCV works different then normal CV
//			double[] scores = new double[leaveOutIterations];
//			//-- CV loop over n "leave-out x points"-trials 
//			for (int j = 0; j < leaveOutIterations; j++) {
//				//-- remove x random points (TODO: implement and move it to Util class)
//				OpenJUMPVectorLayer removedRandPointsLayer = removeRandomPointsOLD(layer, pointNumToLeaveOut);
//				//--
//				OpenJUMPRasterLayer resultRaster = this.calculateKernelDensity(removedRandPointsLayer, vAttribute, h, cellSize, 
//					kernelType, this.gExtent, removeSinglePoints, context);
//				//rasters.add(resultRaster);
//				//-- calculate the error score (TODO: implement and move it to Util class)
//				double score = calculatePointErrorOLD(points, resultRaster, vAttribute);	
//				scores[j] = score;
//			}
//			//-- get the mean score for all trials
//			this.functionScores[i] = StatisticSample.mean(scores);
//			
//		}
//		//-- visualize the scores in a plot
//	    ShowScores myScorePlot = new ShowScores(this.hValues, this.functionScores);
//		//=======
//		//-- create the final raster, for hBest
//		//TODO
//		this.outputRaster = null;	
//		return this.outputRaster;
//	}

	/**
	 * not needed anymore, see my comment for calculateLSCVKernelDensityOLD()
	 * Calculates the estimation error. The reference function is given in
	 * XXX (YEAR).<br>
	 * <b>Note</b>: it could be that the reference function
	 * depends on the kernel type used (I need to check that).
	 * @param pointLayer
	 * @param resultRaster
	 * @param attributeName
	 * @return
	 * TODO: implement & move to Util Class
	 */
//	private double calculatePointErrorOLD(Layer pointLayer, OpenJUMPRasterLayer resultRaster, String attributeName ) {
//		double score = 0;
//		
//		// create a gridwrapper to access the cell values
//		//GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(resultRaster, resultRaster.getLayerGridExtent());
//		GridExtent extent = resultRaster.getLayerGridExtent();
//		//-- loop over all points and calculate the difference at each point
//		double sumDiff = 0;
//		FeatureCollection fc = pointLayer.getFeatureCollectionWrapper().getUltimateWrappee();
//		for (Iterator iterator = fc.iterator(); iterator.hasNext();) {
//			Feature f = (Feature) iterator.next();
//			Object o = f.getAttribute(attributeName); 
//			double weight = 0;
//			if (o instanceof Double){
//				weight = (Double)o;
//			}
//			else if(o instanceof Integer){
//				weight = (Integer)o;
//			}
//			else{// set weight to one
//				// should not be a big problem if all have the same weight 
//				weight = 1;
//			}
//			Point p = (Point)f.getGeometry(); 
//			GridCell cell = extent.getGridCoordsFromWorldCoords(p.getX(), p.getY());
//			double rasterDensityValue = cell.getValue();
//			//---------------------------------
//			// TODO Auto-generated method stub
//			// What is the reference function?
//			// calc diff!
//			//--------------------------------
//			double diff = 0;
//			double pointScore =  diff * diff;
//			sumDiff = sumDiff + pointScore;
//		}
//		//score = xxx;
//		return score;
//	}
	
	/**
	 * not needed anymore, see my comment for calculateLSCVKernelDensityOLD()
	 * Removes a random number of points in a layer. 
	 * @param layer
	 * @param numPointsToRemove
	 * @return
	 * TODO: implement & move to Util Class
	 */
//	public OpenJUMPVectorLayer removeRandomPointsOLD(OpenJUMPVectorLayer layer, int numPointsToRemove) {
//		
//		OpenJUMPVectorLayer result = layer;
//		
//		// TODO Auto-generated method stub
//		//result = xxx;
//		return result;
//	}

}
/*
final class ShowScores extends JFrame{

	public ShowScores(double[] testedh, double[] testedScores, double href, double hrefScore, 
			double[] optimizationHs, double[] optimizationScores){
			
		// Build a 2D data set	    
		double[][] datas1 = new double [testedh.length][2];
		for (int j = 0; j < testedh.length; j++) {			
			datas1[j][0] = testedh[j];
			datas1[j][1] = testedScores[j];			
		}
		double[][] datas2 = new double [1][2];
		datas2[0][0] = href;
		datas2[0][1] = hrefScore;
		double[][] datas3 = new double [optimizationScores.length][2];
		for (int j = 0; j < optimizationScores.length; j++) {			
			datas3[j][0] = optimizationHs[j];
			datas3[j][1] = optimizationScores[j];			
		}
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addLinePlot("CV Scores",datas1);
		plot2dA.addScatterPlot("CV Scores dots",datas1);
		plot2dA.addLinePlot("h_optimize",datas3);
		plot2dA.addScatterPlot("h_ref",datas2);
		//====================
		plot2dA.setAxisLabel(0,"h values");
		plot2dA.setAxisLabel(1,"Score");
		// Display a Frame containing the plot panel
		new FrameView(plot2dA);		

	}            
}
*/
