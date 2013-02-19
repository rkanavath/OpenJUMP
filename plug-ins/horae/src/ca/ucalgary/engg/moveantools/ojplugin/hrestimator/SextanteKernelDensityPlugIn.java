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
 * created:  		20.Oct.2009
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
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JTextField;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.KernelDensityHRSplit;
import ca.ucalgary.engg.moveantools.util.KernelDensityLSCV;
import ca.ucalgary.engg.moveantools.util.KernelDensitySextanteModifiedAlgorithm;
import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;

import com.vividsolutions.jts.geom.GeometryFactory;
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
import es.unex.sextante.parameters.Parameter;

/**
 * @description: Calculates Kernel Density using a Sextante algorithm
 *	
 * @author sstein
 *
 **/
public class SextanteKernelDensityPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Calculates Kernel Density using a Sextante algorithm (fixed Kernel). \n" +
    		"Note 1: The retuned raster values correspond to the input units, and are not density values f(t). \n" +
    		"Note 2: If h is determined using LSCV then the Biweight Kernel and no weights are used for KD(h_opt).";   
    private final String sLAYERPTS = "Layer with Points";    
    private Layer input = null;
    private double distance = 0;
    private double cellSize = 25;
    private int kernelType = 0;
    private double hrefBiweight = 0;
    
    private MultiInputDialog dialog;
    private JTextField distField;
    private JCheckBox lscvBox;
    private JCheckBox hrSplitBox;
    private JCheckBox singlePointBox;
    private PlugInContext context = null;
    private JLabel hrefLabel = null;
    private JComboBox kernelComboBox = null;
    
	private String vAttribute = "";
    private String sATTRIBUTEA = "weight attribute";
    private String sDistance = "Distance (h, Bandwidth, Radius)";
    private String sCellSize = "Cell Size";
    private String sKernelType = "Select Kernel Type (see Silverman (1986) and Wikipedia)";
    private String sDetermineHLSCV = "Determine Bandwith (h) with Least Squares Cross Validation";
    private String sDetermineHSplit = "Determine Bandwith (h) by identifying the Home Range split point (ad-hoc)";
    private String sRemoveSinglePt = "Remove single points, distance: 2*h (eg. outliers, no matter what the weight is)";
    private String sHRefLabel = "h_ref (biweight Kernel) = ";
    private boolean useLSCV = false;
    private boolean useHRSplit = false;
    private boolean removeSinglePoints = false;
    
    //--
    private List kernelTypes;
    private int KERNEL_SEXTANTE = 0; // wij=(1 - d^2/h^2)^2
    private int KERNEL_BIWEIGHT = 1; // wij= 3/Pi*(1 - d/h)^2
	private int KERNEL_TRIWEIGHT = 2;
	private int KERNEL_EPANECHNIKOV = 3;
	private int KERNEL_SCALED_NORMAL = 4;
	private int KERNEL_TRIANGULAR = 5;
	private int KERNEL_UNIFORM = 6;
	private int KERNEL_COSINE = 7;
    private String kernelSelected;
    private String sKERNEL_TYPE_STANDARD = "Standard Sextante Biweight Kernel (w_max=1)";
    private String sKERNEL_TYPE_BIWEIGHT = "Biweight Kernel (K2) (w_max=15/16)";
    private String sKERNEL_TYPE_TRIWEIGHT = "Triweight Kernel (K3) (w_max=35/32)";
    private String sKERNEL_TYPE_EPANECHNIKOV = "Epanechnikov Kernel (w_max=0.75)";
    private String sKERNEL_TYPE_SCALED_NORMAL = "Scaled Normal/Gaussian Kernel (w_max~0.4)";
    private String sKERNEL_TYPE_TRIANGULAR = "Triangular Kernel (w_max=1)";
    private String sKERNEL_TYPE_UNIFORM = "Uniform/Rectangular Kernel (w_max=0.5)";
    private String sKERNEL_TYPE_COSINE = "Cosine Kernel (w_max=Pi/2)";
    
    GeometryFactory gfactory = new GeometryFactory();
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE", "Point KDE"}, 	//menu path
                    this,
                    new JMenuItem("Point Kernel Density...", null),
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
	    	if ((this.useLSCV == false) && (this.useHRSplit == false)){
		    	if(this.cellSize > this.distance){
		    		context.getWorkbenchFrame().warnUser("Cell size needs to be smaller than Bandwidth!");
		    		return;
		    	}
		    	if((2 * this.cellSize) > this.distance){
		    		context.getWorkbenchFrame().warnUser("Cell size should be < 1/2 * Bandwidth!");
		    	}
		    	RasterImageLayer raster = this.calculateKernelDensity(this.input, 
		    			this.vAttribute, this.distance, this.cellSize, this.kernelType, this.removeSinglePoints , context, monitor);
		        if(true){//fill in condition (i.e. not null, or working with subset)
		        	//context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", resultC);
		     	    context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
		        }
	    	}
	    	if (this.useLSCV == true){
	    		KernelDensityLSCV kdlscv = new KernelDensityLSCV();
	    		RasterImageLayer raster = kdlscv.calculateLSCVKernelDensity(this.input, 
		    			this.vAttribute, this.cellSize, this.kernelType, this.removeSinglePoints , 
		    			false, context, monitor);
		        if(raster != null){
		        	//context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", resultC);
		     	    context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
		        }
		        else{
		        	context.getWorkbenchFrame().warnUser("no point dataset");
		        }
	    	}
	    	if (this.useHRSplit == true){
	    		KernelDensityHRSplit kdhrsplit = new KernelDensityHRSplit();
	    		RasterImageLayer raster = kdhrsplit.calculateHRSplitKernelDensity(this.input, 
		    			this.vAttribute, this.cellSize, this.kernelType, this.removeSinglePoints , 
		    			false, context, monitor);
		        if(raster != null){
		        	//context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", resultC);
		     	    context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
		        }
	    	}
    	    System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Kernel Density", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        final JComboBox jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
                updateHref();
            }            
        });
        this.lscvBox = dialog.addCheckBox(this.sDetermineHLSCV, this.useLSCV);   
        this.hrSplitBox = dialog.addCheckBox(this.sDetermineHSplit, this.useHRSplit);
 	    this.distField = dialog.addDoubleField(sDistance, this.distance, 6);
 	    this.hrefLabel = dialog.addLabel(this.sHRefLabel);
 	    this.updateHref();
		if (lscvBox.isSelected()){
			this.distField.setEnabled(false);
			this.useHRSplit = false;
			this.hrSplitBox.setSelected(false);
		}
		else{
			this.distField.setEditable(true);
		}
		if (hrSplitBox.isSelected()){
			this.useLSCV = false;
			this.lscvBox.setSelected(false);
			this.distField.setEnabled(false);
			//this.removeSinglePoints = true;	
			//this.singlePointBox.setSelected(true);
		}
 	    dialog.addDoubleField(sCellSize, this.cellSize, 6);
 	    //-- add kernel selection combobox
 	    kernelTypes = new ArrayList();
 	    kernelTypes.add(this.sKERNEL_TYPE_STANDARD);
 	    kernelTypes.add(this.sKERNEL_TYPE_BIWEIGHT);
 	    kernelTypes.add(this.sKERNEL_TYPE_TRIWEIGHT);
 	    kernelTypes.add(this.sKERNEL_TYPE_EPANECHNIKOV);
 	    kernelTypes.add(this.sKERNEL_TYPE_SCALED_NORMAL);
 	    kernelTypes.add(this.sKERNEL_TYPE_TRIANGULAR);
 	    kernelTypes.add(this.sKERNEL_TYPE_UNIFORM);
 	    kernelTypes.add(this.sKERNEL_TYPE_COSINE);
 	    kernelSelected = this.sKERNEL_TYPE_STANDARD; 
 	    this.kernelComboBox = dialog.addComboBox(sKernelType, kernelSelected, kernelTypes, null);
		if (lscvBox.isSelected()){
			this.kernelComboBox.setEnabled(false);
		}
		else{
			this.kernelComboBox.setEditable(true);
		}
		this.singlePointBox = dialog.addCheckBox(this.sRemoveSinglePt, this.removeSinglePoints);
 	    
 	    //-- add listener
 	    lscvBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
 	    hrSplitBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
 	    GUIUtil.centreOnWindow(dialog);
    }
	
	private void updateControls() {
		//System.out.print("process update method: ");
		if (this.lscvBox.isSelected()){
			this.distField.setEnabled(false);
			this.kernelComboBox.setEnabled(false);
			this.useHRSplit = false;
			this.hrSplitBox.setSelected(false);
			//System.out.print(" switch off ");
		}
		else{
			this.distField.setEnabled(true);
			this.kernelComboBox.setEnabled(true);
			//System.out.print(" switch on ");
		}
		if (hrSplitBox.isSelected()){
			this.useLSCV = false;
			this.lscvBox.setSelected(false);
			//this.lscvBox.setEnabled(false);
			this.distField.setEnabled(false);
			//this.removeSinglePoints = true;	
			//this.singlePointBox.setSelected(true);
		}
		//System.out.println("");
	}
	
	private void updateHref() {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.hrefBiweight = KernelDensityUtil.calculateHrefBiWeightKernel(this.input);
    	double hrefNormal = this.hrefBiweight / KernelDensityUtil.A_K;
    	double hrefNormalRounded = (Math.round(hrefNormal*1000))/1000.0;
    	this.sHRefLabel = "h_ref (normal Kernel) = " + hrefNormalRounded 
    			+ "; h_ref (biweight Kernel) = " + this.hrefBiweight; 
    	this.hrefLabel.setText(this.sHRefLabel);
		//System.out.print("process h_ref");
	}

	private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
        this.vAttribute = dialog.getText(this.sATTRIBUTEA); 
        this.distance = dialog.getDouble(this.sDistance);
        this.cellSize = dialog.getDouble(this.sCellSize);
        this.removeSinglePoints = dialog.getBoolean(this.sRemoveSinglePt);
        
        this.kernelSelected = dialog.getText(sKernelType);
        this.kernelType = getKernelTypeCode(this.kernelSelected);
		if (this.lscvBox.isSelected()){
			this.kernelType =  KERNEL_BIWEIGHT;
		}
		else{
	        this.kernelType = getKernelTypeCode(this.kernelSelected);
		}
        this.useLSCV = dialog.getBoolean(this.sDetermineHLSCV);
        this.useHRSplit = dialog.getBoolean(this.sDetermineHSplit);
      }
    
    private int getKernelTypeCode(String type)
    {
      if (type == sKERNEL_TYPE_STANDARD) return KERNEL_SEXTANTE;
      if (type == sKERNEL_TYPE_BIWEIGHT) return KERNEL_BIWEIGHT;
      if (type == sKERNEL_TYPE_TRIWEIGHT) return KERNEL_TRIWEIGHT;
      if (type == sKERNEL_TYPE_EPANECHNIKOV) return KERNEL_EPANECHNIKOV;
      if (type == sKERNEL_TYPE_SCALED_NORMAL) return KERNEL_SCALED_NORMAL;
      if (type == sKERNEL_TYPE_TRIANGULAR) return KERNEL_TRIANGULAR;
      if (type == sKERNEL_TYPE_UNIFORM) return KERNEL_UNIFORM;
      if (type == sKERNEL_TYPE_COSINE) return KERNEL_COSINE;
      return KERNEL_SEXTANTE;
    }
    
	private RasterImageLayer calculateKernelDensity(Layer points,
			String vAttribute, double distance, double cellSize, int kernelType, boolean removeSinglePoints, PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException, IOException {
		

		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}

		monitor.report("prepare computation");
		OpenJUMPVectorLayer layer = new OpenJUMPVectorLayer();
		layer.create(points);		
		//-- This will create a grid extent that has the full extent of our
		// input layer. Note, the layer extent is generated before the points are
		// filtered, so we keep the original size
		AnalysisExtent extent = new AnalysisExtent(layer);
		//--
		OpenJUMPVectorLayer filteredLayer = new OpenJUMPVectorLayer();
		if(removeSinglePoints){
			FeatureCollection pointFc = points.getFeatureCollectionWrapper().getUltimateWrappee(); 
			FeatureCollection filteredPointsFc = KernelDensityUtil.removePoints(pointFc, 2*distance, context);
			Layer filteredPoints = new Layer(points.getName() + "_filtered", context.getLayerManager().generateLayerFillColor(),
									filteredPointsFc, context.getLayerManager());
			filteredLayer.create(filteredPoints);
		}
		else{
			filteredLayer = layer;
		}
		//--
		KernelDensitySextanteModifiedAlgorithm alg = new KernelDensitySextanteModifiedAlgorithm();
		
		ParametersSet params = alg.getParameters();
		Parameter layerParam = (Parameter)params.getParameter(KernelDensitySextanteModifiedAlgorithm.LAYER);
		boolean worked = layerParam.setParameterValue(filteredLayer);
		//System.out.println("kerneltype: " + kernelType);
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
			monitor.report("computation");
			alg.execute(null, outputFactory);
			
			monitor.report("retrieving results");
			IRasterLayer result = (IRasterLayer)raster.getOutputObject();
			RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();
			return resultOJLayer;
		}
		else{
			context.getWorkbenchFrame().warnUser("layer not a point layer; has ShapeType: " + layer.getShapeType());
			return null;
		}		
	}
	
}
