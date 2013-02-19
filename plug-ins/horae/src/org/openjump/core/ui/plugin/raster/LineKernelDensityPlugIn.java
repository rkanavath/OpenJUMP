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
 * created:  		13.May.2010
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;

import ca.ucalgary.engg.moveantools.util.LineKernelDensityAlgorithm;

import com.vividsolutions.jump.feature.FeatureCollection;
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

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.outputs.Output;

/**
 * @description: Calculates kernel density for a set of lines
 *	
 * @author sstein
 *
 **/
public class LineKernelDensityPlugIn extends AbstractPlugIn implements ThreadedPlugIn{

    private String sSidebar ="Creates a density raster based on a moving kernel function along the line. Note, currently only the Biweight Kernel without scaling is used";   
    private final String sLAYERLINES = "Layer with Lines";
    private final String sCellSize = "raster cell size";
    private final String sBandWidth = "Bandwidth";
    private final String sATTRIBUTEA = "weight field";
    private final String sRasterizeFirst = "rasterize lines first (note: results will be different, \nmodeling of lines as one or many objects makes no difference)";
    
    private double cellSize = 25.0;
    private double bandWidth = 200;
    private String valueFieldName = "";
    private boolean rasterizeFirst = true;
    
    private FeatureCollection lines = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private JCheckBox rasterizeBox = null;
    //private JCheckBox useMeanBox = null;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
	        FeatureInstaller featureInstaller = new FeatureInstaller(context.getWorkbenchContext());
	    	featureInstaller.addMainMenuItem(
	    	        this,								//exe
	                new String[] {MenuNames.PLUGINS}, 	//menu path
	                "Line Kernel Density", 
	                //AbstractPlugIn.createName(DisplayMovementTracksPlugIn.class),
	                false,			//checkbox
	                null,			//icon
	                createEnableCheck(context.getWorkbenchContext())); //enable check
    }

    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);

        return new MultiEnableCheck()
                        .add(checkFactory.createAtLeastNLayersMustExistCheck(1));
    }
    
	public boolean execute(PlugInContext context) throws Exception{
        //Unlike ValidatePlugIn, here we always call #initDialog because we want
        //to update the layer comboboxes.
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
    
    public void run(TaskMonitor monitor, PlugInContext context) throws Exception{            		
	    	System.gc(); //flush garbage collector
	    	this.context = context;
	    	monitor.allowCancellationRequests();
	        //-- create density raster
	        RasterImageLayer raster = calcBBDensityGrid(this.input, bandWidth, cellSize, 
	        		this.valueFieldName, this.rasterizeFirst, context, monitor);
	        if(raster != null){
	        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
	        }
	        //--
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Line Kernel Density", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERLINES, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        //useMeanBox = dialog.addCheckBox(sUseMean, useMean);
        rasterizeBox = dialog.addCheckBox(sRasterizeFirst, rasterizeFirst);
        dialog.addDoubleField(sBandWidth, this.bandWidth, 8);
	    dialog.addDoubleField(sCellSize, this.cellSize, 8);
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERLINES).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERLINES));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERLINES);
    	this.lines= this.input.getFeatureCollectionWrapper(); 
    	this.bandWidth = dialog.getDouble(this.sBandWidth);
    	this.cellSize = dialog.getDouble(this.sCellSize);
    	this.valueFieldName = dialog.getText(this.sATTRIBUTEA);
    	this.rasterizeFirst = dialog.getBoolean(this.sRasterizeFirst);
      }
	
	
	private RasterImageLayer calcBBDensityGrid(Layer singleLines,
			double bandwidth, double cellSize, String valueFieldName, boolean rasterizeFirst, PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException {
		
        
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		OpenJUMPVectorLayer ojlineLayer = new OpenJUMPVectorLayer();
	
		//--
		ojlineLayer.create(singleLines);
		
		LineKernelDensityAlgorithm alg = new LineKernelDensityAlgorithm();
		ParametersSet params = alg.getParameters();
		boolean worked = params.getParameter(LineKernelDensityAlgorithm.LINELAYER).setParameterValue(ojlineLayer);
		if(worked){
			params.getParameter(LineKernelDensityAlgorithm.VALUEFIELD).setParameterValue(ojlineLayer.getFieldIndexByName(valueFieldName));
			params.getParameter(LineKernelDensityAlgorithm.BANDWIDTH).setParameterValue(new Double(bandwidth));
			params.getParameter(LineKernelDensityAlgorithm.RASTERIZEFIRST).setParameterValue(new Boolean(rasterizeFirst));
			
			//-- we will use a cell size of x meters
			AnalysisExtent extent = new AnalysisExtent(ojlineLayer);
			extent.setCellSize(cellSize);
			//-- we will use a cell size of x meters
			extent.setCellSize(cellSize);
			//-- enlarge by kernel size (otherwise the outer points are missed)
			int ntimes = (int)Math.ceil(bandwidth/cellSize) + 2; // plus two reserve
			for( int i = 0; i < ntimes; i++){
				extent.enlargeOneCell();
			}
			//-- And now we set the extent as the one to use to create new raster
			// layers within the rasterizing algorithm.
			alg.setAnalysisExtent(extent);
			
			OutputObjectsSet outputs = alg.getOutputObjects();
			Output raster = outputs.getOutput(LineKernelDensityAlgorithm.RESULT);	
			//Output rasterizeRes = outputs.getOutput(LineKernelDensityAlgorithm.TEMPRESULT);
			if(monitor != null){
				monitor.report("create line KD grid");
			}
			alg.execute(null, outputFactory);		
	
			IRasterLayer result = (IRasterLayer)raster.getOutputObject();
			RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();
			/*
			{
				IRasterLayer resultRasterize = (IRasterLayer)rasterizeRes.getOutputObject();
				RasterImageLayer resultOJLayerRR = (RasterImageLayer)resultRasterize.getBaseDataObject();
		        if(resultOJLayerRR != null){
		        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, resultOJLayerRR);
		        }
			}
			*/
			return resultOJLayer;
		}
		else{
			context.getWorkbenchFrame().warnUser("layer not a line layer; has ShapeType: " + ojlineLayer.getShapeType());
			return null;
		}	
	}
	
}
