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
 * created:  		25.April.2010
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
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;
import javax.swing.JTextField;

import org.math.array.DoubleArray;
import org.math.array.StatisticSample;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.geomutils.algorithm.GeometryConverter;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.LineBufferDensityAlgorithm;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.operation.buffer.BufferOp;
import com.vividsolutions.jts.operation.union.UnaryUnionOp;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureDatasetFactory;
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
 * @description: buffers animal tracks (positive and negative) to calculate the home range.
 * It is assumed that an animal has explored an area twice (negative buffering). The buffer 
 * value r is based on the median (or mean) travel distance per day. The negative (r_n) and the 
 * following positive buffer use r_n = -1.1*r. 
 *	
 * @author sstein
 *
 **/
public class LineBufferbasedHomeRangesPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Create home ranges  based on the tracks by applying buffer operations. " +
    		"The buffer distance is calculated based on average daily travel and length of the MBR of the convex hull (day).";   
    private final String sLAYERPTS = "Layer with Points";
    private final String sDetermineBufferSize = "Obtain buffer size from travel distance";
    private final String sBufferSize = "Buffer size [m]";
    private final String sUseMean = "use mean instead of median";
    private String sATTRIBUTEA = "Location Attribute (for sorting)";
    private String sATTRIBUTEDay = "Day Attribute (for avg. travel distance, no string fields, unqiue per year!)";
    private final String sCreateDensityGrid = "create usage density grid - so probability contours can be calculated later";
    private final String sCellSize = "raster cell size for density grid (in m)";
    
    public double negativeBufferFactor = 1.1;
    public double cellSize = 25.0;
    public double bufferSize = 0.0;
    
    private boolean determineBuffer = true;
    private boolean useMean = false;
    private boolean createDensityGrid = false;
	private String locAttribute = "";
	private String dayAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private JCheckBox useMeanBox = null;
    private JCheckBox determineBufferBox = null;
    private JCheckBox createDensityBox = null;
    private JComboBox jcb_attributeA = null;
    private JComboBox jcb_attributeB = null;
    JTextField bufferSizeField = null;
    JTextField densityField = null;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE"}, 	//menu path
                    this,
                    new JMenuItem("Line Buffer-based Home Ranges...", null),
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
	    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedPoints, 
	    			this.locAttribute, this.dayAttribute, context, monitor);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", singleLines);
	        double bufferDist = 0.0;
            DecimalFormat format = new DecimalFormat("#########.##");
	        if(this.determineBuffer == true){
	        	/*
		        Coordinate[] pointCoords = new Coordinate[sortedPoints.size()];
		        int i = 0;
		        for (Iterator iterator = sortedPoints.iterator(); iterator.hasNext();) {
					Feature ftemp = (Feature) iterator.next();
					Geometry geom = ftemp.getGeometry();
					if (geom instanceof Point){
						pointCoords[i] = geom.getCoordinate();
					}
					else{
						context.getWorkbenchFrame().warnUser("geom not point, use random vertex, geom-id:" +i);
						//-- not sure this may return an arbitrary vertex
						pointCoords[i] = geom.getCoordinate();
					}
					i++;
				}			
		        double[] errorDists = calcLeaveOneOutDistance(pointCoords);
		        */
		        double[] errorDists = TrackCalculationUtil.calcOneDayActivityRadiusDistances(sortedPoints, this.dayAttribute, 
		        		this.input.getName(), true, context);
		        //-- calculate median for buffer radius
	    	    double[] sortvals = DoubleArray.sort(errorDists);
	    	    int index = (int)Math.ceil(errorDists.length/2.0);	                
		        bufferDist = sortvals[index-1];
		        double meanDist = StatisticSample.mean(errorDists);
		        context.getWorkbenchFrame().warnUser("median: " + format.format(bufferDist) + " --- mean: " + format.format(meanDist));
		        if (useMean){
		        	bufferDist = meanDist;
		        }
		    }
	        else{
	        	bufferDist = this.bufferSize;
	        }
	        //-- apply positive buffer op
	        monitor.report("buffering with r=" + bufferDist);	
	        FeatureCollection buffer1FC = runBuffer(bufferDist, singleLines, true);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-trackbuffer", buffer1FC);
	        //-- apply negative buffer op
	        double bufferDistInward = -1*this.negativeBufferFactor*bufferDist;
	        monitor.report("buffering with -r*1.1=" + bufferDistInward);	
	        FeatureCollection buffer2FC = runBuffer(bufferDistInward, buffer1FC, true);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-trackErosion", buffer2FC);
	        //-- apply positive buffer op
	        double bufferDistOutward = -1*bufferDistInward;
	        monitor.report("buffering with +r*1.1=" + bufferDistOutward);	
	        FeatureCollection buffer3FC = runBuffer(bufferDistOutward, buffer2FC, true);
	        //-- explode
	        ArrayList<Geometry> geoms = new ArrayList<Geometry>();
	        for (Iterator iterator = buffer3FC.iterator(); iterator.hasNext();) {
				Feature f = (Feature) iterator.next();
				ArrayList<Geometry> expGeom = GeometryConverter.explodeGeomsIfMultiG(f.getGeometry());
				geoms.addAll(expGeom);
	        }
	        FeatureDatasetFactory fdf = new FeatureDatasetFactory();
	        FeatureCollection explodedFC = fdf.createFromGeometry(geoms);
	        //context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-trackHR-" + bufferDist, buffer3FC);
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-trackHR-" + format.format(bufferDist), explodedFC);
	        //-- create raster grid
	        if(createDensityGrid){
	        	RasterImageLayer raster = calcDensityGrid(singleLines, bufferDist, cellSize, context, monitor);
	        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
	        }
	        //--
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Line Buffer HR", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        determineBufferBox = dialog.addCheckBox(sDetermineBufferSize, determineBuffer);
        dialog.addSeparator();
	    bufferSizeField = dialog.addDoubleField(sBufferSize, this.bufferSize, 8);
	    dialog.addSeparator();
        Object valB = list.size()>0?list.iterator().next():null;
        jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
        if (list.size() == 0) jcb_attributeB.setEnabled(false);
        useMeanBox = dialog.addCheckBox(sUseMean, useMean);
	    dialog.addSeparator();
        createDensityBox = dialog.addCheckBox(sCreateDensityGrid, createDensityGrid);
	    densityField = dialog.addDoubleField(sCellSize, this.cellSize, 8);
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
        this.updateControls();
        this.updateControlsDensity();
 	    //-- add listenerz
        determineBufferBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
        createDensityBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControlsDensity();
 	        }
 	    });
        GUIUtil.centreOnWindow(dialog);
    }
	
	private void updateControls() {
	    if (determineBufferBox.isSelected()){
        	useMeanBox.setEnabled(true);
        	jcb_attributeA.setEnabled(true);
        	jcb_attributeB.setEnabled(true);
	    	bufferSizeField.setEnabled(false);
	    }
	    else{
        	useMeanBox.setEnabled(false);
        	jcb_attributeA.setEnabled(true);
        	jcb_attributeB.setEnabled(false);
	    	bufferSizeField.setEnabled(true);
	    }
	}
	private void updateControlsDensity() {
	    if (createDensityBox.isSelected()){
	    	densityField.setEnabled(true);
	    }
	    else{
	    	densityField.setEnabled(false);
	    }
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
    	this.determineBuffer = determineBufferBox.isSelected();
    	this.bufferSize = dialog.getDouble(sBufferSize);
    	this.useMean = useMeanBox.isSelected();
    	this.createDensityGrid = createDensityBox.isSelected();
    	this.cellSize = dialog.getDouble(this.sCellSize);
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        this.dayAttribute = dialog.getText(this.sATTRIBUTEDay);
      }
	
	/**
	 * buffers the features in a layer
	 * @param bufferDistance
	 * @param inputFC
	 * @param doUnion perform a union of all buffers?
	 * @return
	 */
	public static FeatureCollection runBuffer(double bufferDistance, FeatureCollection inputFC, boolean doUnion){
	    FeatureSchema fSchema = new FeatureSchema();
	    fSchema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
	    fSchema.addAttribute("bufferProcID", AttributeType.INTEGER);
	    FeatureCollection resultFC = new FeatureDataset(fSchema);
	    //-- buffer
	    ArrayList<Geometry> geoms = new ArrayList<Geometry>();
	    int i=0;
		for (Iterator iterator = inputFC.iterator(); iterator.hasNext();) {
			i++;
			Feature f = (Feature) iterator.next();
			Geometry geom = f.getGeometry();
		    Geometry result = null;
		    try {
			    BufferOp bufOp = new BufferOp(geom);
			    bufOp.setQuadrantSegments(4);
			    bufOp.setEndCapStyle(BufferOp.CAP_ROUND);
			    result = bufOp.getResultGeometry(bufferDistance);
		    }
		    catch (RuntimeException ex) {
		        // simply eat exceptions and report them by returning null
		    }
		    if (result != null){
		    	 geoms.add(result);
		    	 Feature ftemp = new BasicFeature(fSchema);
		    	 ftemp.setGeometry(result);
		    	 ftemp.setAttribute("bufferProcID", new Integer(i));
		    	 resultFC.add(ftemp);
		    }
		}
		if(doUnion){
			//-- union
			Geometry g = UnaryUnionOp.union(geoms);
			geoms.clear();
			geoms.add(g);
			resultFC = FeatureDatasetFactory.createFromGeometry(geoms);
		}
		return resultFC;
	}
	
	private RasterImageLayer calcDensityGrid(FeatureCollection singleLines,
			double bufferDist, double cellSize, PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException {
		
		/* // not needed if I am interest in raster only
    	FeatureSchema fs = new FeatureSchema();
    	fs.addAttribute("geometry", AttributeType.GEOMETRY);
    	fs.addAttribute("percent-cover", AttributeType.DOUBLE);
    	fs.addAttribute("param", AttributeType.DOUBLE);
    	FeatureCollection fcRes = new FeatureDataset(fs);
    	*/
		
		//-- get the buffers
        FeatureCollection buffer1FC = runBuffer(bufferDist, singleLines, false);
        
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}
		OpenJUMPVectorLayer ojBufferLayer = new OpenJUMPVectorLayer();
		
		Layer bufferLayer = new Layer("usagegrid", context.getLayerManager().generateLayerFillColor(),
				buffer1FC, context.getLayerManager());
		//--
		ojBufferLayer.create(bufferLayer);
		
		LineBufferDensityAlgorithm alg = new LineBufferDensityAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(LineBufferDensityAlgorithm.LAYER).setParameterValue(ojBufferLayer);
		params.getParameter(LineBufferDensityAlgorithm.IDFIELD).setParameterValue(ojBufferLayer.getFieldIndexByName("bufferProcID"));
		
		//-- we will use a cell size of x meters
		AnalysisExtent extent = new AnalysisExtent(ojBufferLayer);
		extent.setCellSize(cellSize);
		//-- enlarge by kernel size
		int ntimes = 5; // plus five reserve cells
		for( int i = 0; i < ntimes; i++){
			extent.enlargeOneCell();
		}
		//-- And now we set the extent as the one to use to create new raster
		// layers within the rasterizing algorithm.
		alg.setAnalysisExtent(extent);
		
		OutputObjectsSet outputs = alg.getOutputObjects();
		Output raster = outputs.getOutput(LineBufferDensityAlgorithm.RESULT);		
		if(monitor != null){
			monitor.report("create density grid");
		}
		alg.execute(null, outputFactory);		

		IRasterLayer result = (IRasterLayer)raster.getOutputObject();
		RasterImageLayer resultOJLayer = (RasterImageLayer)result.getBaseDataObject();
		return resultOJLayer;
	}
	
}
