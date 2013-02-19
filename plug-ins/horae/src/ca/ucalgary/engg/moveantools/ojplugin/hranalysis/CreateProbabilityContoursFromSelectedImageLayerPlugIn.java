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
 * created:  		2.Nov.2009
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Iterator;
import java.util.List;

import javax.swing.JCheckBox;
import javax.swing.JMenuItem;
import javax.swing.JTextField;

import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;
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
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;

/**
 * @description: creates probability contours and polygons from the current selected raster image
 * @TODO: I was going todo this as a normal plugin, but this won't work since
 * raster images are Layerables and not layer objects, so the drop down list doesn't
 * display them
 *	
 * @author sstein
 *
 **/
public class CreateProbabilityContoursFromSelectedImageLayerPlugIn extends AbstractThreadedUiPlugIn{
  
    private PlugInContext context = null;
    private MultiInputDialog dialog;
    private JCheckBox contourCheckBox = null;
    private JTextField equiField = null;
    private JTextField minField = null;
    private JTextField maxField = null;
    
    private String sSidebar ="Create probablity contour lines and polygons from raster. Note, this function does not work for rasters with NoData values = -99999.0";
	public String sMaxValue = "max value to display";
	public String sMinValue = "min value to display";
	public String sEquiValue = "Equidistance";
	public String sChosenValue = "probability [0.0..1.0]";
	public String sCreatePolygon = "create polygon from contour";
	public String sCreateContours = "create surface contours (topographic, i.e. not probability) to get a feeling for the surface";
	
	public double maxValue = 0;
	public double minValue = 0;
	public double equiValue = 0;
	public double chosenValue = 0.95;
	public boolean createPolygon = true;
	public boolean createContours = false;
	private double calculatedContourValue = 0;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Create Probability Contours from Raster...", null),
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

		OpenJUMPRasterLayer rstLayer = new OpenJUMPRasterLayer();
		rstLayer.create(rLayer);
		double max = rstLayer.getMaxValue();
		double min = rstLayer.getMinValue();
		this.maxValue = max;
		this.minValue = min;
		this.equiValue = Math.floor(Math.abs(max-min)/10.0);
		
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
		
		//-- not sure if this is necessary (maybe if one function calls another Sextante algorithm)???
		monitor.report("initialize sextante");
		Sextante.initialize();
		
		//-- calculate the topographic contours
		if(this.createContours){
			FeatureCollection fd1 = this.calculateEquidistantContours(rLayer, this.equiValue, this.maxValue, this.minValue, monitor, context);
			//-- output topo contours
			if((fd1 != null) && (fd1.size() > 0)){
				context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_contours", fd1);
			}
		}
		
		//-- calculate the probability contour
		//-- calculate the probability contour
		FeatureCollection pcontour = null;
		try{
			pcontour = this.calculateProbabilityContour(rLayer, this.chosenValue, monitor, context);
		}
		catch(GeoAlgorithmExecutionException e){
			String sWarningText = "calculated Probability too small - too few cells for contour";
			context.getWorkbenchFrame().warnUser(sWarningText);
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText(sWarningText);
			context.getWorkbenchFrame().getOutputFrame().addText("cell density values need to be larger than: " + this.calculatedContourValue);
		}	
		if((pcontour != null) && (pcontour.size() > 0)){
			context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_probcontour_" + + this.chosenValue, pcontour);
		}
		
		//-- make polygons out of the contours
		if(this.createPolygon){
			FeatureCollection polys = FeatureConversionUtils.calculatePolysFromContours(pcontour, monitor, context);
			if((polys != null) && (polys.size() > 0)){
				context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_polygon_" + + this.chosenValue, polys);
			}
		}
	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Convert", true);
        dialog.setSideBarDescription(sSidebar);
        dialog.addLabel("particular probability contour");
        dialog.addDoubleField(this.sChosenValue, this.chosenValue, 10);
        dialog.addCheckBox(sCreatePolygon, createPolygon);
        this.contourCheckBox = dialog.addCheckBox(sCreateContours, createContours);
        this.equiField = dialog.addDoubleField(this.sEquiValue, this.equiValue, 10);
        this.minField = dialog.addDoubleField(this.sMinValue, this.minValue, 10);
        this.maxField = dialog.addDoubleField(this.sMaxValue, this.maxValue, 10);
		if (contourCheckBox.isSelected()){
			this.equiField.setEnabled(true);
			this.minField.setEnabled(true);
			this.maxField.setEnabled(true);
		}
		else{
			this.equiField.setEnabled(false);
			this.minField.setEnabled(false);
			this.maxField.setEnabled(false);
		}
 	    //-- add listener
		contourCheckBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
        GUIUtil.centreOnWindow(dialog);
    }
    
	private void updateControls() {
		//System.out.print("process update method: ");
		if (contourCheckBox.isSelected()){
			this.equiField.setEnabled(true);
			this.minField.setEnabled(true);
			this.maxField.setEnabled(true);
		}
		else{
			this.equiField.setEnabled(false);
			this.minField.setEnabled(false);
			this.maxField.setEnabled(false);
		}
		//System.out.println("");
	}
	
    private void getDialogValues(MultiInputDialog dialog) {
    	this.equiValue =  dialog.getDouble(sEquiValue); 
    	this.maxValue = dialog.getDouble(this.sMaxValue);
    	this.minValue = dialog.getDouble(this.sMinValue);
    	this.chosenValue = dialog.getDouble(this.sChosenValue);
    	this.createPolygon = dialog.getBoolean(this.sCreatePolygon);
    	this.createContours = dialog.getBoolean(this.sCreateContours);
      }
    

    /**
     * calculates (topographic) contours for the delivered raster
     * @param rasterImagelayer the input raster layer
     * @param equiDistValue the equidistance value (i.e. the height distance between two contours)
     * @param maxValue the maximun value to be contoured
     * @param minValue the minimum value to be contoured
     * @param monitor (can be null)
     * @param context (cannot be null)
     * @return
     * @throws GeoAlgorithmExecutionException
     */
    public FeatureCollection calculateEquidistantContours(RasterImageLayer rasterImagelayer, 
    		double equiDistValue, double maxValue, double minValue,
    		TaskMonitor monitor, PlugInContext context) throws GeoAlgorithmExecutionException{
		//-- assign the datasources
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		
		OpenJUMPRasterLayer raster = new OpenJUMPRasterLayer();
		raster.create(rasterImagelayer);
		if (monitor != null){
			monitor.report("prepare computation");
		}
		ContourLinesAlgorithm alg = new ContourLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(ContourLinesAlgorithm.LAYER).setParameterValue(raster);
		params.getParameter(ContourLinesAlgorithm.DISTANCE).setParameterValue(new Double(equiDistValue));
		params.getParameter(ContourLinesAlgorithm.MAX).setParameterValue(new Double(maxValue));
		params.getParameter(ContourLinesAlgorithm.MIN).setParameterValue(new Double(minValue));
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		
		if (monitor != null){
			monitor.report("computation topo contours");
		}
		alg.execute(null, outputFactory);
		
		if (monitor != null){
			monitor.report("retrieving topo value results");
		}
		
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);
		
		return fd;
    }
    
    /**
     * creates a contour line for the given probability value based on the volume of the raster.
     * @param rasterImagelayer
     * @param probability
     * @param monitor
     * @param context
     * @return
     * @throws GeoAlgorithmExecutionException
     */
	private FeatureCollection calculateProbabilityContour(
			RasterImageLayer rasterImagelayer, double probability, 
			TaskMonitor monitor, PlugInContext context) throws GeoAlgorithmExecutionException {
		
		//-- assign the datasources
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());		
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}
		OpenJUMPRasterLayer raster = new OpenJUMPRasterLayer();
		raster.create(rasterImagelayer);
		
		//-- map the probability value into a contour value
		double[] mappingResult = KernelDensityUtil.calculateContourHeightValueFromProbabilityValue(raster, probability);
		double contourValue = mappingResult[0];
		double realProbability = mappingResult[1];
		this.calculatedContourValue = contourValue; //for error reporting
		if (monitor != null){
			monitor.report("prepare probability computation");
		}
		ContourLinesAlgorithm alg = new ContourLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(ContourLinesAlgorithm.LAYER).setParameterValue(raster);
		params.getParameter(ContourLinesAlgorithm.DISTANCE).setParameterValue(new Double(1));
		params.getParameter(ContourLinesAlgorithm.MAX).setParameterValue(new Double(contourValue));
		params.getParameter(ContourLinesAlgorithm.MIN).setParameterValue(new Double(contourValue));
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		
		if (monitor != null){
			monitor.report("computation probablity contour");
		}
		alg.execute(null, outputFactory);
		
		if (monitor != null){
			monitor.report("retrieving probablity results");
		}
		
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);

		context.getWorkbenchFrame().warnUser("real probability: " + realProbability);
		//-- set the values as attribute
		String attName = fd.getFeatureSchema().getAttributeName(2);
		for (Iterator iterator = fd.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			f.setAttribute(attName, new Double(realProbability));
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
