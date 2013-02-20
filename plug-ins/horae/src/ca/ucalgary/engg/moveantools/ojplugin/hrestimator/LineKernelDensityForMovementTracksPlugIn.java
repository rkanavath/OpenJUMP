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
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JMenuItem;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;
import ca.ucalgary.engg.moveantools.util.LineKernelDensityForMovementTracksAlgorithm;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

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
 * @description: TODO
 *	
 * @author sstein
 *
 **/
public class LineKernelDensityForMovementTracksPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Creates a density raster based on line-based Kernel-Density - with modifications for " +
    		"movement tracks. The difference to normal Line-based KD is the handling of subsequent track segments" +
    		" - to avoid summation on ends. Note, only the Sextante-Biweight kernel is implemented so far.";   
    private final String sLAYERPTS = "Layer with Points";
    //private final String sUseMean = "use mean instead of median";
    private String sATTRIBUTEA = "Location Attribute (for sorting)";
    //private String sATTRIBUTEDay = "Day Attribute (no string fields!) [information not used yet!!]";
    private final String sCellSize = "raster cell size";
    private final String sBandWidth = "Bandwidth";
    private final String sRasterizeFirst = "rasterize lines first (note: results will be different)";
    private String sHRefLabel = "h_ref (normal Kernel) = ";
    
    public double cellSize = 25.0;
    public double bandWidth = 200;
    private boolean rasterizeFirst = false;
    
    //private boolean useMean = false;
	private String locAttribute = "";
	//private String dayAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private JLabel hrefLabel = null;
    //private JCheckBox useMeanBox = null;
    private JCheckBox rasterizeBox = null;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE", "Line KDE"}, 	//menu path
                    this,
                    new JMenuItem("Line-based Kernel Density for Movement Points...", null),
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
	    	if(this.cellSize > this.bandWidth){
	    		context.getWorkbenchFrame().warnUser("Cell size needs to be smaller than Bandwidth!");
	    		return;
	    	}
	    	if((2 * this.cellSize) > this.bandWidth){
	    		context.getWorkbenchFrame().warnUser("Cell size should be < 1/2 * Bandwidth!");
	    	}
	    	//-- sort points by location
	    	FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
	    	//-- generate tracks
	    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedPoints, 
	    			this.locAttribute, null, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", singleLines);
	        /*// the value calculated here may be used as input for the bandwidth
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
	        RasterImageLayer raster = calcLKDensityGrid(singleLines, bandWidth, cellSize, 
	        		this.rasterizeFirst, context, monitor);
	        if(raster != null){
	        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
	        	raster.setName(this.input.getName() + "-LineKDE");
	        }
	        //--
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Line-KD for Movement", true);
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
        //final JComboBox jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
        //if (list.size() == 0) jcb_attributeB.setEnabled(false);
        //useMeanBox = dialog.addCheckBox(sUseMean, useMean);
        rasterizeBox = dialog.addCheckBox(sRasterizeFirst, rasterizeFirst);
 	    this.hrefLabel = dialog.addLabel(this.sHRefLabel);
 	    this.updateHref();
	    dialog.addDoubleField(sBandWidth, this.bandWidth, 8);
	    dialog.addDoubleField(sCellSize, this.cellSize, 8);
	    dialog.addSeparator();
	    dialog.addLabel("Note, the bandwidth can be also obtained from travel distance & velocity");
	    dialog.addLabel("That is besides h_ref the mean/median travel distance per day can be used");
        //-- add listener stuff
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
        //            jcb_attributeB.setModel(new DefaultComboBoxModel(new String[0]));
        //            jcb_attributeB.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
        //        jcb_attributeB.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
    	//this.useMean = useMeanBox.isSelected();
    	this.bandWidth = dialog.getDouble(this.sBandWidth);
    	this.cellSize = dialog.getDouble(this.sCellSize);
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        //this.dayAttribute = dialog.getText(this.sATTRIBUTEDay);
    	this.rasterizeFirst = dialog.getBoolean(this.sRasterizeFirst);
      }
	
	private void updateHref() {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	double hrefNormal = KernelDensityUtil.calculateHref(this.input);
		double hrefBiweight = KernelDensityUtil.A_K * hrefNormal;
		double hrefBiweightRounded = (Math.round(hrefBiweight*1000))/1000.0;
    	this.sHRefLabel = "h_ref (normal Kernel) = " + hrefNormal + "; h_ref (Biweight) = " +  hrefBiweightRounded; 
    	this.hrefLabel.setText(this.sHRefLabel);
		//System.out.print("process h_ref");
	}
	
	private RasterImageLayer calcLKDensityGrid(FeatureCollection singleLines,
			double bandwidth, double cellSize, boolean rasterizeLFirst, PlugInContext context, TaskMonitor monitor) 
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
		
		LineKernelDensityForMovementTracksAlgorithm alg = new LineKernelDensityForMovementTracksAlgorithm();
		ParametersSet params = alg.getParameters();
		boolean worked = params.getParameter(LineKernelDensityForMovementTracksAlgorithm.LINELAYER).setParameterValue(ojlineLayer);
		if(worked){
			params.getParameter(LineKernelDensityForMovementTracksAlgorithm.IDFIELD).setParameterValue(ojlineLayer.getFieldIndexByName("bufferProcID"));
			params.getParameter(LineKernelDensityForMovementTracksAlgorithm.BANDWIDTH).setParameterValue(new Double(bandwidth));
			params.getParameter(LineKernelDensityForMovementTracksAlgorithm.RASTERIZEFIRST).setParameterValue(new Boolean(rasterizeLFirst));
			
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
			Output raster = outputs.getOutput(LineKernelDensityForMovementTracksAlgorithm.RESULT);		
			if(monitor != null){
				monitor.report("create KD grid");
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
