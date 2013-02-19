/*
 * The Unified Mapping Platform (JUMP) is an extensible, interactive GUI 
 * for visualizing and manipulating spatial features with geometry and attributes.
 *
 * JUMP is Copyright (C) 2003 Vivid Solutions
 *
 * This class implements extensions to JUMP and is
 * Copyright (C) Stefan Steiniger.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * 
 * For more information, contact:
 * Stefan Steiniger
 * perriger@gmx.de
 */

/*****************************************************
 * created:  		9.June.2010
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin.hranalysis;

import java.awt.BorderLayout;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;

import org.math.array.DoubleArray;
import org.math.array.LinearAlgebra;
import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plot.Plot2DPanelOJ;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.FeatureConversionUtils;
import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.rasterWrappers.GridWrapperNotInterpolated;
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;

/**
 * @description: Creates the core area from a density raster. The method used has been
 * described in Seaman and Powell (1990), but see also RA Powell (2000) and PN Laver (2005).
 * 
 * @TODO: I was going todo this as a normal plugin, but this won't work since
 * raster images are Layerables and not layer objects, so the drop down list doesn't
 * display them
 *	
 * @author sstein
 *
 **/
public class CreateCoreAreaFromSelectedImageLayerPlugIn extends AbstractThreadedUiPlugIn{
  
    private PlugInContext context = null;
    private MultiInputDialog dialog;
    
    private String sSidebar ="Creates the core area from a density raster. The method used is described in Seaman and Powell (1990) and " +
    		"Laver (2005). A comparison graph is generated as well based on probability contour line evaluation (see Harris et al. 1990). " +
    		"Note, this function does not work for rasters with NoData values = -99999.0";

	public String sCreatePolygon = "create polygon from contour";
	private boolean createPolygon = false;
	private double coreProbability = 0.50; //[0.0 ... 1.0]
	private double calculatedContourValue = 0;
        
    public void initialize(PlugInContext context) throws Exception {

        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Create Core Area from Density Raster...", null),
                    createEnableCheck(context.getWorkbenchContext()), -1); 
    }

    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);

        return new MultiEnableCheck()
                        .add(checkFactory.createAtLeastNLayerablesMustBeSelectedCheck(1, RasterImageLayer.class));
    }
    
    /**
     *@inheritDoc
     */
    public String getIconString() {
        return null;
    }
   
    
	public boolean execute(PlugInContext context) throws Exception{
        //Unlike ValidatePlugIn, here we always call #initDialog because we want
        //to update the layer comboboxes.
		
		//-- calc min and max values
        RasterImageLayer rLayer = (RasterImageLayer) LayerTools.getSelectedLayerable(context, RasterImageLayer.class);
        if (rLayer==null){
            context.getWorkbenchFrame().warnUser("no layer selected");
            return false;
        }
		
        initDialog(context);
        dialog.setVisible(true);
        if (!dialog.wasOKPressed()) {
            return false;
        }
        else{
        	this.getDialogValues(dialog); 
        }
        return true;	    
	}

	public void run(TaskMonitor monitor, PlugInContext context)
			throws Exception {
		monitor.allowCancellationRequests();
		GeometryFactory gf = new GeometryFactory();
		//-- get the rasterimage layer
        RasterImageLayer rLayer = (RasterImageLayer) LayerTools.getSelectedLayerable(context, RasterImageLayer.class);
        
        if (rLayer==null){
            context.getWorkbenchFrame().warnUser("no layer selected");
            return;
        }
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}
		OpenJUMPRasterLayer rstLayer = new OpenJUMPRasterLayer();
		rstLayer.create(rLayer);
		GridWrapperNotInterpolated gwrapper = new GridWrapperNotInterpolated(rstLayer, rstLayer.getLayerGridExtent());
		//-- put all cell values larger than 0 in a list
		ArrayList<Double> cellvals = new ArrayList();
		int nx = rstLayer.getLayerGridExtent().getNX();
		int ny = rstLayer.getLayerGridExtent().getNY();
		//int numPoints = nx * ny;
		for (int x = 0; x < nx; x++) {//cols
			for (int y = 0; y < ny; y++) {//rows 
				//Point2D pt = rstLayer.getLayerGridExtent().getWorldCoordsFromGridCoords(x, y);
				//-- read only for 1st band of image
				double value = gwrapper.getCellValueAsDouble(x, y, 0);
				//take only the values > 0, so we would get only all values of the home range
				if(value > 0){
					cellvals.add(value);
				}
			}
		}
		//make a double Array out of it
		int numCells = cellvals.size();
		double[] densValues = new double[numCells];
		double[] idx = new double[numCells];
		for (int i = 0; i < numCells; i++) {
			densValues[i] = cellvals.get(i);
			idx[i] = i;
		}
		//--------------------------- begin probability calc code ---------
		// the following code is also contained in the function
		// KernelDensityUtil.calculateCoreAreaProbability()
		// so if improvements need to be done, change code there too.
		//----------------------------------------------------------------
		//sort the array from smallest to largest
		densValues = DoubleArray.sort(densValues);
		//-- make descending
		double[] valuesN = new double[numCells];
		for (int j = 0; j < densValues.length; j++) {//rows
			valuesN[j] = densValues[densValues.length-1-j];
		}
		densValues = valuesN;
		//-- calculate the area
		double cellArea = gwrapper.getCellSize() * gwrapper.getCellSize();
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
		this.coreProbability = 1 - (diffProb/100.00);
		//[sstein 29th July 2010] don't use the line below, because the value returns a different
		//                        contour than if I use the normal contouring function
		//this.calculatedContourValue = diffProb/100.00 * maxDenseValue;
		//-- use this instead
		double[] mappingResultN = KernelDensityUtil.calculateContourHeightValueFromProbabilityValue(rstLayer, this.coreProbability);
		this.calculatedContourValue = mappingResultN[0];
		double realProbabilityN = mappingResultN[1];
		//[sstein end]
		//--------------------------- end probability calc code ---------
		//-- not sure if this is necessary (maybe if one function calls another Sextante algorithm)???
		monitor.report("initialize sextante");
		Sextante.initialize();

		//-- calculate the probability contour
		FeatureCollection pcontour = null;
		try{
			pcontour = this.calculateProbabilityContour(rLayer, this.calculatedContourValue, monitor, context);
		}
		catch(GeoAlgorithmExecutionException e){
			String sWarningText = "calculated Probability " + this.coreProbability + " too small - too few cells for contouring";
			context.getWorkbenchFrame().warnUser(sWarningText);
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText(sWarningText);
			context.getWorkbenchFrame().getOutputFrame().addText("cell density values need to be larger than: " + this.calculatedContourValue);
		}
		double coreProbShort = (Math.floor(this.coreProbability * 1000.0)) / 1000.0;
		if((pcontour != null) && (pcontour.size() > 0)){
			context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_corecontour_" + coreProbShort, pcontour);
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("Core area calculation:");
			context.getWorkbenchFrame().getOutputFrame().addText("Layer: " + rstLayer.getName());
			context.getWorkbenchFrame().getOutputFrame().addText("Calculated Contour Value with Cell-based method: " + this.calculatedContourValue);
			context.getWorkbenchFrame().getOutputFrame().addText("Calculated Probability Value with Cell-based method: " + this.coreProbability);
		}
		
		//-- make polygons out of the contours
		if(this.createPolygon){
			FeatureCollection polys = FeatureConversionUtils.calculatePolysFromContours(pcontour, monitor, context);
			if((polys != null) && (polys.size() > 0)){
				context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_corepolygon_" + coreProbShort, polys);
			}
		}
		//---------------------------
		// for testing/comparison
		// create the graph for probabilities from 5..100
		//---------------------------
		double[] probsT = new double[40];
		double[] areasT = new double[40];
		for (int i = 0; i < areasT.length; i++) {
			probsT[i] = 0.025 + i*0.025;
			monitor.report("creating comparison graph p[0.05...1.0]: " + probsT[i]);
			double[] mappingResult = KernelDensityUtil.calculateContourHeightValueFromProbabilityValue(rstLayer, probsT[i]);
			double contourValue = mappingResult[0];
			double realProbability = mappingResult[1];
			if(contourValue > 0){
				FeatureCollection pcontourT = this.calculateProbabilityContour(rLayer, contourValue, monitor, context);
				if(pcontourT != null){
					FeatureCollection polysT = FeatureConversionUtils.calculatePolysFromContours(pcontourT, monitor, context);
					List<Feature> polysTL = polysT.getFeatures();
					double areaSum = 0;
					for (Iterator iterator = polysTL.iterator(); iterator.hasNext();) {
						Feature f = (Feature) iterator.next();
						areaSum = areaSum + f.getGeometry().getArea();
					}
					areasT[i] = areaSum;
					probsT[i] = realProbability;
				}
				else{
					areasT[i] = 0;
					probsT[i] = realProbability;
				}
			}
			else{
				areasT[i] = 0;
			}
		}
		double maxAreaT = DoubleArray.max(areasT);
		double[] areasTPercent = LinearAlgebra.times(areasT, 1.0/maxAreaT);
		double[] probsTInv = LinearAlgebra.minus(1.0, probsT);
		
		monitor.report("drawing graphs");
		
        ShowCoreTestGraph myScoreTestPlot = new ShowCoreTestGraph(probsTInv, areasTPercent);
        double comparisonContourPValue = myScoreTestPlot.getContourProbabilityValue();
		if((pcontour != null) && (pcontour.size() > 0)){
			context.getWorkbenchFrame().getOutputFrame().addText("Calculated Probability Value with Contour-based method: " + comparisonContourPValue);
		}

    	Plot2DPanelOJ plott = myScoreTestPlot.getPlot();

    	// FrameView fv = new FrameView(plot);
    	// -- replace the upper line by:
    	JInternalFrame framet = new JInternalFrame("probability-area function plot based on contour line area.");
    	framet.setLayout(new BorderLayout());
    	framet.add(plott, BorderLayout.CENTER);
    	framet.setClosable(true);
    	framet.setResizable(true);
    	framet.setMaximizable(true);
    	framet.setSize(450, 450);
    	framet.setVisible(true);

    	context.getWorkbenchFrame().addInternalFrame(framet);
    	
    	//-----------------------------------------------------------------
		/* NOTE: the code below changes the context to the new frame object
		 * as a consequence this code needs to be called after all
		 * layer operations are done.
		 * i.e. context.getLayerManager() will return null if this code
		 * is proceeded earlier.
		 */
        //-- plot the HR values
        //double cvalues[] = {0.0, 1.0, 2.0};
        //double idx[] = {10.0, 16.0, 20.0};
        ShowCoreGraph myScorePlot = new ShowCoreGraph(densValues, area);

    	Plot2DPanelOJ plot = myScorePlot.getPlot();

    	// FrameView fv = new FrameView(plot);
    	// -- replace the upper line by:
    	JInternalFrame frame = new JInternalFrame("core area function plot based on raster cells - used p_core for output");
    	frame.setLayout(new BorderLayout());
    	frame.add(plot, BorderLayout.CENTER);
    	frame.setClosable(true);
    	frame.setResizable(true);
    	frame.setMaximizable(true);
    	frame.setSize(450, 450);
    	frame.setVisible(true);

    	context.getWorkbenchFrame().addInternalFrame(frame);
	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Core Area", true);
        dialog.setSideBarDescription(sSidebar);
        dialog.addCheckBox(sCreatePolygon, createPolygon);
        GUIUtil.centreOnWindow(dialog);
    }
	
    private void getDialogValues(MultiInputDialog dialog) {
    	this.createPolygon = dialog.getBoolean(this.sCreatePolygon);
      }
    

    
    /**
     * creates a contour line for the given probability value based on the volume of the raster.
     * @param rasterImagelayer
     * @param contourDensityValue
     * @param monitor
     * @param context
     * @return
     * @throws GeoAlgorithmExecutionException
     */
	private FeatureCollection calculateProbabilityContour(
			RasterImageLayer rasterImagelayer, double contourDensityValue, 
			TaskMonitor monitor, PlugInContext context) throws GeoAlgorithmExecutionException {
		
		//-- assign the datasources
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());		
		OpenJUMPRasterLayer raster = new OpenJUMPRasterLayer();
		raster.create(rasterImagelayer);
		/*
		if (monitor != null){
			monitor.report("prepare probability computation");
		}
		*/
		ContourLinesAlgorithm alg = new ContourLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(ContourLinesAlgorithm.LAYER).setParameterValue(raster);
		params.getParameter(ContourLinesAlgorithm.DISTANCE).setParameterValue(new Double(1));
		params.getParameter(ContourLinesAlgorithm.MAX).setParameterValue(new Double(contourDensityValue));
		params.getParameter(ContourLinesAlgorithm.MIN).setParameterValue(new Double(contourDensityValue));
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		/*
		if (monitor != null){
			monitor.report("computation probability contour");
		}
		*/
		try{
			alg.execute(null, outputFactory);
		}
		catch(Exception e){
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("Problem with method CreateCoreAreaFromSelectedImageLayerPlugIn.calculateProbabilityContour()");
			context.getWorkbenchFrame().getOutputFrame().addText("can't retrieving results for DEM value: " + contourDensityValue);
			return null;
		}
		/*
		if (monitor != null){
			monitor.report("retrieving probability results");
		}
		*/
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);

		//-- set the values as attribute
		String attName = fd.getFeatureSchema().getAttributeName(2);
		for (Iterator iterator = fd.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			f.setAttribute(attName, new Double(contourDensityValue));
		}
		//-delete the files
		String contourFileName = resultOJLayer.getFilename();
		String contourFileNameShx = resultOJLayer.getFilename(); contourFileNameShx = contourFileNameShx.replace(".shp", ".shx");
		String contourFileNameDbf = resultOJLayer.getFilename(); contourFileNameDbf = contourFileNameDbf.replace(".shp", ".dbf");
		try{
			File vectorFile = new File(contourFileName);
			boolean vectorDeleted = vectorFile.delete();
			File vectorFileShx = new File(contourFileNameShx); vectorFileShx.delete();
			File vectorFileDbf = new File(contourFileNameDbf); vectorFileDbf.delete();
			System.out.println("deleted contourFile: " + vectorDeleted);
		}
		catch(Exception e){
			e.printStackTrace();
			//eat
		}
		return fd;
	}
	
}

final class ShowCoreGraph extends JFrame{
	
	Plot2DPanelOJ plot = null;

	public ShowCoreGraph(double[] probabilityValuesInPercent, double[] areaValuesInPercent){
			
		// Build a 2D data set out of the density data	    
		double[][] datas1 = new double [probabilityValuesInPercent.length][2];
		for (int j = 0; j < probabilityValuesInPercent.length; j++) {			
			datas1[j][0] = probabilityValuesInPercent[j];
			datas1[j][1] = areaValuesInPercent[j];			
		}
		//-- create the random use graph/line 
		double maxProb = DoubleArray.max(probabilityValuesInPercent);
		double[][] datas2 = new double [probabilityValuesInPercent.length][2];
		for (int j = 0; j < probabilityValuesInPercent.length; j++) {			
			datas2[j][0] = probabilityValuesInPercent[j];
			datas2[j][1] = maxProb-probabilityValuesInPercent[j];			
		}
		//-- difference between both
		//double[][] diffs = new double [probabilityValuesInPercent.length][2];
		double[] diffsV = new double [probabilityValuesInPercent.length];
		for (int j = 0; j < probabilityValuesInPercent.length; j++) {			
			//diffs[j][0] = probabilityValuesInPercent[j]; //
			double diff = (maxProb-probabilityValuesInPercent[j]) - areaValuesInPercent[j];
			//diffs[j][1] = diff;
			diffsV[j] = diff;
		}
		int maxDiffIdx = DoubleArray.maxIndex(diffsV);
		double maxDiff = diffsV[maxDiffIdx];
		double diffProb = probabilityValuesInPercent[maxDiffIdx];
		double diffArea = areaValuesInPercent[maxDiffIdx];
		//-- create a vertical line that shows the largest difference	
		double[][] datas3 = new double [2][2];
		datas3[0][0] = diffProb; // x1 probability value
		datas3[0][1] = diffArea; // the current value y1
		datas3[1][0] = diffProb; // x2
		datas3[1][1] = maxProb-diffProb; // the random value
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		//plot2dA.addLinePlot("Scores",datas1);
		plot2dA.addScatterPlot("Scores dots",datas1);
		plot2dA.addLinePlot("random use line",datas2);
		plot2dA.addLinePlot("difference for probability " + diffProb,datas3);
		//plot2dA.addScatterPlot("difference dots",datas3);
		//====================
		plot2dA.setAxisLabel(0,"density values [%] (eq. prob)");
		plot2dA.setAxisLabel(1,"area covered by density [%]");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
	
}

final class ShowCoreTestGraph extends JFrame{
	
	Plot2DPanelOJ plot = null;
	double contourProbValue = 0;

	public ShowCoreTestGraph(double[] probabilityValues, double[] areaValues){
			
		// Build a 2D data set out of the density data	    
		double[][] datas1 = new double [probabilityValues.length][2];
		for (int j = 0; j < probabilityValues.length; j++) {			
			datas1[j][0] = probabilityValues[j];
			datas1[j][1] = areaValues[j];			
		}
		double[][] datas2 = new double [probabilityValues.length][2];
		for (int j = 0; j < probabilityValues.length; j++) {			
			datas2[j][0] = probabilityValues[j];
			datas2[j][1] = 1-probabilityValues[j];			
		}
		double[] diffsV = new double [probabilityValues.length];
		double[][] diffPlot = new double [probabilityValues.length][2];
		for (int j = 0; j < probabilityValues.length; j++) {			
			double diff = datas2[j][1] - datas1[j][1];
			diffsV[j] = diff;
			diffPlot[j][0] = probabilityValues[j];
			diffPlot[j][1] = diff;	
		}
		int maxDiffIdx = DoubleArray.maxIndex(diffsV);
		double maxDiff = diffsV[maxDiffIdx];
		double diffProb = probabilityValues[maxDiffIdx];
		double diffArea = areaValues[maxDiffIdx];
		contourProbValue = 1.0-diffProb;
		//-- create a vertical line that shows the largest difference	
		double[][] datas3 = new double [2][2];
		datas3[0][0] = diffProb; // x1 probability value
		datas3[0][1] = diffArea; // the current value y1
		datas3[1][0] = diffProb; // x2
		datas3[1][1] = datas2[maxDiffIdx][1]; // the random value
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addScatterPlot("Scores",datas1);
		plot2dA.addLinePlot("random line",datas2);
		plot2dA.addLinePlot("difference for probability " + diffProb,datas3);
		plot2dA.addScatterPlot("differences random vs current", diffPlot );
		//====================
		plot2dA.setAxisLabel(0,"100-prob");
		plot2dA.setAxisLabel(1,"area");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
	
	public double getContourProbabilityValue(){
		return contourProbValue;
	}
}