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
 * created:  		12.May.2010
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description:
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin.hrestimator;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.BrownianBridgeGeometrySegmentAlgorithm;
import ca.ucalgary.engg.moveantools.util.BrownianBridgeUtil;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.WorkbenchContext;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.EnableCheckFactory;
import com.vividsolutions.jump.workbench.plugin.MultiEnableCheck;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GUIUtil;
import com.vividsolutions.jump.workbench.ui.MultiInputDialog;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.core.OutputFactory;
import es.unex.sextante.core.OutputObjectsSet;
import es.unex.sextante.core.ParametersSet;
import es.unex.sextante.dataObjects.IRasterLayer;
import es.unex.sextante.exceptions.GeoAlgorithmExecutionException;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;
import es.unex.sextante.outputs.Output;

/**
 * @description: Creates a density raster based on the Brownian Bridge approach described in Horne et al. (2007) 
 *	
 * @author sstein
 *
 **/
public class BrownianBridgeDensityPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Creates a density raster based on the Brownian Bridge approach described in Horne et al. (2007).";   
    private final String sLAYERPTS = "Layer with Points";
    private final String sDTOne = "equal time difference between points, i.e. dT = 10'000 sec";
    private String sATTRIBUTEA = "Location Attribute (for ordering points)";
    private String sATTRIBUTEDay = "Time Attribute (double value, with time in sec)";
    private final String sCellSize = "raster cell size";
    private final String sSigma1Mob = "Sigma 1 - related to animal mobility/speed [m]";
    private final String sSigma2Err = "Sigma 2 - related to location error [m]";
    
    public double cellSize = 25.0;
    public double sigma1Mob = 100;
    public double sigma2Err = 30;
    
    private boolean useDTone = true;
	private String locAttribute = "";
	private String timeAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private JCheckBox useDTOneBox = null;
    private JComboBox jcb_attributeB = null;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE", "Brownian Bridge"}, 	//menu path
                    this,
                    new JMenuItem("Brownian Bridge Home Ranges - Vector Segment Version...", null),
                    createEnableCheck(context.getWorkbenchContext()), -1); 
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
	    	//-- sort points by location
	    	FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
	    	//-- generate tracks
	    	if (this.useDTone){
	    		this.timeAttribute = null;
	    	}
	    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedPoints, 
	    			this.locAttribute, this.timeAttribute, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", singleLines);
	        /*// the value calculated here mey be used as input for the bandwidth
	        double[] errorDists = TrackCalculationUtil.calcOneDayActivityRadiusDistances(sortedPoints, this.dayAttribute, 
	        		this.input.getName(), true, context);
	        //-- calculate median for buffer radius
    	    double[] sortvals = DoubleArray.sort(errorDists);
    	    int index = (int)Math.ceil(errorDists.length/2.0);	                
	        double bufferDist = sortvals[index-1];
	        double meanDist = StatisticSample.mean(errorDists);
	        context.getWorkbenchFrame().warnUser("median: " + bufferDist + "-- mean: " + meanDist);
	        if (useMean){
	        	bufferDist = meanDist;
	        }
	        */
	        //-- create density raster
	        RasterImageLayer raster = calcBBDensityGrid(singleLines, sigma1Mob, sigma2Err, 
	        		cellSize, this.useDTone, context, monitor);
	        if(raster != null){
	        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
	        }
	        //--
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Brownian Bridge Segment Version", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        Object valB = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        useDTOneBox = dialog.addCheckBox(sDTOne, useDTone);
        jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
        if (list.size() == 0) jcb_attributeB.setEnabled(false);
	    dialog.addDoubleField(sSigma1Mob, this.sigma1Mob, 8);
	    dialog.addDoubleField(sSigma2Err, this.sigma2Err, 8);
	    dialog.addDoubleField(sCellSize, this.cellSize, 8);
	    if (useDTOneBox.isSelected()){
	    	jcb_attributeB.setEnabled(false);
	    }
	    else{
	    	jcb_attributeB.setEnabled(true);
	    }
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                    jcb_attributeB.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeB.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
                jcb_attributeB.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
 	    //-- add listener
        useDTOneBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
        GUIUtil.centreOnWindow(dialog);
    }
	
	private void updateControls() {
	    if (useDTOneBox.isSelected()){
	    	jcb_attributeB.setEnabled(false);
	    }
	    else{
	    	jcb_attributeB.setEnabled(true);
	    }
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
    	this.useDTone = useDTOneBox.isSelected();
    	this.sigma1Mob = dialog.getDouble(this.sSigma1Mob);
    	this.sigma2Err = dialog.getDouble(this.sSigma2Err);
    	this.cellSize = dialog.getDouble(this.sCellSize);
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        this.timeAttribute = dialog.getText(this.sATTRIBUTEDay);
      }
	
	
	private RasterImageLayer calcBBDensityGrid(FeatureCollection singleLines,
			double sigma1Mobil, double sigma2Error, double cellSize, 
			boolean useDtOne, PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException {
		
        
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}
		OpenJUMPVectorLayer ojlineLayer = new OpenJUMPVectorLayer();
		
		Layer lineLayer = new Layer("tracks", context.getLayerManager().generateLayerFillColor(),
				singleLines, context.getLayerManager());
		//--
		ojlineLayer.create(lineLayer);
		
		BrownianBridgeGeometrySegmentAlgorithm alg = new BrownianBridgeGeometrySegmentAlgorithm();
		ParametersSet params = alg.getParameters();
		boolean worked = params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.LINELAYER).setParameterValue(ojlineLayer);
		if(worked){
			params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.IDFIELD).setParameterValue(ojlineLayer.getFieldIndexByName("bufferProcID"));
			params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.SIGMA1MOBIL).setParameterValue(new Double(sigma1Mobil));
			params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.SIGMA2LOCERROR).setParameterValue(new Double(sigma2Error));
			params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.EQUALDELTATIME).setParameterValue(new Boolean(useDtOne));
			params.getParameter(BrownianBridgeGeometrySegmentAlgorithm.TIMEDIFFFIELD).setParameterValue(ojlineLayer.getFieldIndexByName("timeDiff"));
			//-- we will use a cell size of x meters
			AnalysisExtent extent = new AnalysisExtent(ojlineLayer);
			extent.setCellSize(cellSize);
			//-- we will use a cell size of x meters
			extent.setCellSize(cellSize);
			//-- enlarge by "kernel size" (otherwise the outer points are missed)
			int ntimes = 2;

			double varMaxDist = 0;
			if (this.useDTone){
				// use 10000 instead 1 for numerical reasons
				varMaxDist = BrownianBridgeUtil.calcMaxVarDist(sigma1Mobil, sigma2Error, 10000.0);
				//varMaxDist = BrownianBridgeUtil.maxh(sigma1Mobil, sigma2Error, 25, 10000.0);
			}
			else{
				double dTMax = 0;
				int attID = singleLines.getFeatureSchema().getAttributeIndex("timeDiff");
				for (Iterator iterator = singleLines.iterator(); iterator
						.hasNext();) {
					Feature f = (Feature) iterator.next();
					double dt = f.getDouble(attID);
					if(dt > dTMax){
						dTMax = dt;
					}
				}
				varMaxDist = BrownianBridgeUtil.calcMaxVarDist(sigma1Mobil, sigma2Error, dTMax);
				//varMaxDist = BrownianBridgeUtil.maxh(sigma1Mobil, sigma2Error, 25, dTMax);
			}
			ntimes = (int)Math.ceil(varMaxDist/cellSize); 
			if(ntimes > 100){
				System.out.println("BrownianBridgeDensityPlugIn: reset raster enlargement to 100 cells, demanded extension (cells): " + ntimes);
				ntimes = 100; 
			}
			for( int i = 0; i < ntimes; i++){
				extent.enlargeOneCell();
			}
			//-- And now we set the extent as the one to use to create new raster
			// layers within the rasterizing algorithm.
			alg.setAnalysisExtent(extent);
			
			OutputObjectsSet outputs = alg.getOutputObjects();
			Output raster = outputs.getOutput(BrownianBridgeGeometrySegmentAlgorithm.RESULT);		
			if(monitor != null){
				monitor.report("create brownian bridge grid");
			}
			alg.execute(null, outputFactory);		
	
			IRasterLayer result = (IRasterLayer)raster.getOutputObject();
			RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();
			return resultOJLayer;
		}
		else{
			context.getWorkbenchFrame().warnUser("layer not a line layer; has ShapeType: " + ojlineLayer.getShapeType());
			return null;
		}	
	}
	
}
