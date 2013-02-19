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
 * created:  		28.Oct.2009
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package org.openjump.core.ui.plugin.raster;

import java.util.List;

import org.openjump.core.apitools.LayerTools;
import org.openjump.core.rasterimage.RasterImageLayer;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.AbstractPlugIn;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.plugin.ThreadedPlugIn;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MenuNames;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;
import com.vividsolutions.jump.workbench.ui.plugin.FeatureInstaller;

import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.core.Sextante;
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;

/**
 * @description: creates contour lines from the current selected raster image
 * @TODO: I was going todo this as a normal plugin, but this won't work since
 * raster images are Layerables and not layer objects, so the drop down list doesn't
 * display them
 *	
 * @author sstein
 *
 **/
public class CreateContourLineFromSelectedImageLayerPlugIn extends AbstractPlugIn implements ThreadedPlugIn{
  
    private PlugInContext context = null;
    private MultiInputDialog dialog;
    
    private String sSidebar ="Create contour lines from Raster";
	public String sMaxValue = "max value to display";
	public String sMinValue = "min value to display";
	public String sEquiValue = "Equidistance";
	
    GeometryFactory gfactory = new GeometryFactory();
	public double maxValue = 0;
	public double minValue = 0;
	public double equiValue = 0;
        
    public void initialize(PlugInContext context) throws Exception {
    				
	        FeatureInstaller featureInstaller = new FeatureInstaller(context.getWorkbenchContext());
	    	featureInstaller.addMainMenuItem(
	    	        this,								//exe
	                new String[] {MenuNames.RASTER}, 	//menu path
	                "Create Contours from Raster", 
	                false,			//checkbox
	                null,			//icon
	                createEnableCheck(context.getWorkbenchContext())); //enable check
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
		
		//-- assign the datasources
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		
		OpenJUMPRasterLayer raster = new OpenJUMPRasterLayer();
		raster.create(rLayer);

		monitor.report("prepare computation");
		ContourLinesAlgorithm alg = new ContourLinesAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(ContourLinesAlgorithm.LAYER).setParameterValue(raster);
		params.getParameter(ContourLinesAlgorithm.DISTANCE).setParameterValue(new Double(this.equiValue));
		params.getParameter(ContourLinesAlgorithm.MAX).setParameterValue(new Double(this.maxValue));
		params.getParameter(ContourLinesAlgorithm.MIN).setParameterValue(new Double(this.minValue));
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output result = outputs.getOutput(ContourLinesAlgorithm.RESULT);
		
		monitor.report("computation");
		alg.execute(null, outputFactory);
		
		monitor.report("retrieving results");
		IVectorLayer resultLayer = (IVectorLayer)result.getOutputObject();
		OpenJUMPVectorLayer resultOJLayer = (OpenJUMPVectorLayer)result.getOutputObject();
		Layer newResultLayer = (Layer)resultOJLayer.getBaseDataObject();
		List<Feature> features = newResultLayer.getFeatureCollectionWrapper().getWrappee().getFeatures();
		FeatureSchema fsnew = newResultLayer.getFeatureCollectionWrapper().getFeatureSchema();
		FeatureDataset fd = new FeatureDataset(fsnew);
		fd.addAll(features);
		//-- output
		if((fd != null) && (fd.size() > 0)){
			context.addLayer(StandardCategoryNames.RESULT, rLayer.getName() + "_contours", fd);
		}
	}
    
	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Convert", true);
        dialog.setSideBarDescription(sSidebar);
        dialog.addDoubleField(this.sEquiValue, this.equiValue, 10);
        dialog.addDoubleField(this.sMinValue, this.minValue, 10);
        dialog.addDoubleField(this.sMaxValue, this.maxValue, 10);
 	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.equiValue =  dialog.getDouble(sEquiValue); 
    	this.maxValue = dialog.getDouble(this.sMaxValue);
    	this.minValue = dialog.getDouble(this.sMinValue);
      }
}
