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
 * created:  		08.Oct.2009
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import org.math.array.DoubleArray;
import org.math.array.StatisticSample;
import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plot.Plot2DPanelOJ;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.FeatureConversionUtils;
import ca.ucalgary.engg.moveantools.util.KernelDensityHRSplit;
import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
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
import es.unex.sextante.dataObjects.IVectorLayer;
import es.unex.sextante.openjump.core.OpenJUMPOutputFactory;
import es.unex.sextante.openjump.core.OpenJUMPRasterLayer;
import es.unex.sextante.openjump.core.OpenJUMPVectorLayer;
import es.unex.sextante.openjump.init.OJSextanteApiInitialiser;
import es.unex.sextante.outputs.Output;
import es.unex.sextante.vectorize.contourLines.ContourLinesAlgorithm;

/**
 * @description: Displays a Graph for Asymptote Analysis. If the data are track data, then the
    		points are used in order to create a geometry and calculate the area. Otherwise 10 random
    		geometries are created and the median area value is taken.
    		Information on Asymptote Analysis can be found in Harris et al. (1990) and Laver (2005).
 *	
 * @author sstein
 *
 **/
public class AsymptoteAnalysisPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays a Graph for Asymptote Analysis. If the data are track data, then the " +
    		"points are used in order to create a geometry and calculate the area. Otherwise 10 random " +
    		"geometries are created and the median area value is taken. Information on Asymptote Analysis can " +
    		"be found in Harris et al. (1990) and Laver (2005).";   
    private final String sLAYERPTS = "Layer with Points";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
    private JCheckBox trackDataBox = null;
    private JComboBox jcb_attributeA = null;
    private JRadioButton buttonMCP = null;
    private JRadioButton buttonLineBuffer = null;
    private JRadioButton buttonPointKDE = null;
    //private JRadioButton buttonPointKDEAdaptive = null;
    private String METHODGROUP = "METHOD GROUP";
    JCheckBox adaptiveBox = null;
    JTextField cellSizeField = null;
    ShowHrefPlot myhrefPlot = null;
    
    private String sChoseAnalysisMethod = "Select the HR estimator for the asymptote analysis:";
    
    boolean useMCP = true;
    private String sUseMCP= "use MCP method";
    boolean useLineBuffer = false;
    private String sUseLineBuffer= "use Line-Buffer method (only for tracking data)";
    public double negativeBufferFactor = 1.1;
    boolean usePointKDEFixed = false;
    boolean usePointKDEAdaptive = false;
    boolean useKDE = false;
    private String sUsePointKDE= "use Point KDE method (with 95% contour, h_ref, biweight kernel, 10 point steps)";
    //private String sUsePointKDEAdaptive= "use Point KDE method (with 95% contour, h_ref=adaptive, Biweight, cell=200m - 10 point steps)";
    public double propContour = 0.95;
    public double cellSize = 200;
    private int KERNEL_SEXTANTE = 0;
    
    boolean isGPSTrackData = true;
    private String sTrackData = "point data are track data";
	private String locAttribute = "";
    private String sATTRIBUTEA = "location attribute [for ordering of points]";
	private String dayAttribute = "";
	private String sATTRIBUTEDay = "Day Attribute (for median travel distance, no string fields, unqiue per year!)";
	private String sAdaptiveHref = "adaptive h_ref (calculates h_ref each time points are added)";
	private String sCellSize = "Raster cell size";
	private JComboBox jcb_attributeB = null;
	
    GeometryFactory gfactory = new GeometryFactory();
        
    public void initialize(PlugInContext context) throws Exception {

        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Asymptote Analysis...", null),
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
			if(OJSextanteApiInitialiser.isInitialized == false){
				OJSextanteApiInitialiser.initializeSextante(context);
			}
	    	FeatureCollection resultC = null;
	    	if(this.useMCP){
	    		resultC = this.calculateConvexHulls(this.points, this.isGPSTrackData, this.locAttribute, context, monitor);
	    	}
	    	if(this.useLineBuffer){
	    		resultC = this.calculateLineBuffer(this.points, this.locAttribute, this.dayAttribute, context, monitor);
	    	}
	    	if(this.usePointKDEFixed){
	    		//System.out.println("fixed KDE"); return;
	    		resultC = this.calculatePointKDE(this.points, this.isGPSTrackData, true, this.locAttribute, this.cellSize, context, monitor);
	    	}
	    	if(this.usePointKDEAdaptive){
	    		//System.out.println("adaptive KDE"); return;
	    		resultC = this.calculatePointKDE(this.points, this.isGPSTrackData, false, this.locAttribute, this.cellSize, context, monitor);
	    	}
	    	if((resultC != null) && (resultC.size() > 0)){
	        	context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-as_geoms", resultC);
	        }
	        
	        //-- get the areas
	        //double[] areas = retrieveAreas(resultC);

	    	//-- Display results in a plot
	    	//-- stuff for jmathplot

	    	ShowAsymptotePlot myScorePlot = new ShowAsymptotePlot(resultC);

	    	Plot2DPanelOJ plot = myScorePlot.getPlot();

	    	// FrameView fv = new FrameView(plot);
	    	// -- replace the upper line by:
	    	JInternalFrame frame = new JInternalFrame("Asymptote function plot");
	    	frame.setLayout(new BorderLayout());
	    	frame.add(plot, BorderLayout.CENTER);
	    	frame.setClosable(true);
	    	frame.setResizable(true);
	    	frame.setMaximizable(true);
	    	frame.setSize(450, 450);
	    	frame.setVisible(true);

	    	context.getWorkbenchFrame().addInternalFrame(frame);
	    	
	    	if(usePointKDEAdaptive){
		    	Plot2DPanelOJ plothref = this.myhrefPlot.getPlot();

		    	// FrameView fv = new FrameView(plot);
		    	// -- replace the upper line by:
		    	JInternalFrame frame2 = new JInternalFrame("h_ref plot");
		    	frame2.setLayout(new BorderLayout());
		    	frame2.add(plothref, BorderLayout.CENTER);
		    	frame2.setClosable(true);
		    	frame2.setResizable(true);
		    	frame2.setMaximizable(true);
		    	frame2.setSize(450, 450);
		    	frame2.setVisible(true);

		    	context.getWorkbenchFrame().addInternalFrame(frame2);
	    	}
	        
    	    System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Asymptote", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        
        trackDataBox = dialog.addCheckBox(this.sTrackData, this.isGPSTrackData, this.sTrackData);
        
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        Object valA = list.size()>0?list.iterator().next():null;
        Object valB = list.size()>0?list.iterator().next():null;
        jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
        if (list.size() == 0) jcb_attributeA.setEnabled(false);
        
        dialog.getComboBox(this.sLAYERPTS).addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                List list = getFieldsFromLayerWithoutGeometryAndString();
                if (list.size() == 0) {
                    jcb_attributeA.setModel(new DefaultComboBoxModel(new String[0]));
                    jcb_attributeA.setEnabled(false);
                }
                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
            }            
        });
	    if (trackDataBox.isSelected()){
	    	jcb_attributeA.setEnabled(true);
	    }
	    else{
	    	jcb_attributeA.setEnabled(false);
	    }
	    //
	    //dialog.addSeparator();
	    dialog.addLabel(sChoseAnalysisMethod);
	    dialog.addSeparator();
	    buttonMCP = dialog.addRadioButton(this.sUseMCP, METHODGROUP, this.useMCP, this.sUseMCP);
	    dialog.addSeparator();
	    buttonLineBuffer = dialog.addRadioButton(this.sUseLineBuffer, METHODGROUP, this.useLineBuffer, this.sUseLineBuffer);
	    jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
        if (list.size() == 0) jcb_attributeB.setEnabled(false);
	    dialog.addSeparator();
        buttonPointKDE = dialog.addRadioButton(this.sUsePointKDE, METHODGROUP, this.usePointKDEFixed, this.sUsePointKDE);
	    adaptiveBox = dialog.addCheckBox(sAdaptiveHref,false);
	    cellSizeField = dialog.addDoubleField(sCellSize, this.cellSize, 8);
	    dialog.addSeparator();
	    //buttonPointKDEAdaptive = dialog.addRadioButton(this.sUsePointKDEAdaptive, METHODGROUP, this.usePointKDEAdaptive, this.sUsePointKDEAdaptive);
	    //-- add listener
	    trackDataBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
	    buttonMCP.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateSubControls();
 	        }
 	    });
	    buttonLineBuffer.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateSubControls();
 	        }
 	    });
	    buttonPointKDE.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateSubControls();
 	        }
 	    });
		this.useLineBuffer = dialog.getBoolean(this.sUseLineBuffer);
		if(this.useLineBuffer){
			jcb_attributeB.setEnabled(true);	
		}
		else{
			jcb_attributeB.setEnabled(false);	
		}
		this.useKDE = dialog.getBoolean(this.sUsePointKDE);
		if(this.useKDE){
			adaptiveBox.setEnabled(true);
			cellSizeField.setEnabled(true);
		}
		else{
			adaptiveBox.setEnabled(false);
			cellSizeField.setEnabled(false);
		}
 	    //dialog.addDoubleField(T1, 20.0, 4);
        GUIUtil.centreOnWindow(dialog);
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper();
    	this.isGPSTrackData = dialog.getBoolean(this.sTrackData);
    	this.locAttribute = dialog.getText(this.sATTRIBUTEA); 
    	this.useMCP = dialog.getBoolean(this.sUseMCP);
    	this.useLineBuffer = dialog.getBoolean(this.sUseLineBuffer);
    	if(this.useLineBuffer){
    		this.dayAttribute = dialog.getText(this.sATTRIBUTEDay);
    	}
		this.useKDE = dialog.getBoolean(this.sUsePointKDE);
		if(useKDE){
			this.usePointKDEAdaptive = adaptiveBox.isSelected();
			this.usePointKDEFixed = !this.usePointKDEAdaptive;
			this.cellSize = dialog.getDouble(sCellSize);
		}
      }
    
	private void updateControls() {
	    if (trackDataBox.isSelected()){
	    	jcb_attributeA.setEnabled(true);
	    	buttonLineBuffer.setEnabled(true);
	    	buttonPointKDE.setEnabled(true);
			this.useKDE = dialog.getBoolean(this.sUsePointKDE);
			if(this.useKDE){
				adaptiveBox.setEnabled(true);
				cellSizeField.setEnabled(true);
			}
			else{
				adaptiveBox.setEnabled(false);
				cellSizeField.setEnabled(false);
			}
	    	//buttonPointKDEAdaptive.setEnabled(true);
	    }
	    else{
	    	jcb_attributeA.setEnabled(false);
	    	buttonLineBuffer.setEnabled(false);
	    	jcb_attributeB.setEnabled(false);	
	    	buttonPointKDE.setEnabled(false);
			adaptiveBox.setEnabled(false);
			cellSizeField.setEnabled(false);
	    	//buttonPointKDEAdaptive.setEnabled(false);
	    }
	}
	
	private void updateSubControls(){
		this.useLineBuffer = dialog.getBoolean(this.sUseLineBuffer);
		if(this.useLineBuffer){
			jcb_attributeB.setEnabled(true);	
		}
		else{
			jcb_attributeB.setEnabled(false);	
		}
		this.useKDE = dialog.getBoolean(this.sUsePointKDE);
		if(this.useKDE){
			adaptiveBox.setEnabled(true);
			cellSizeField.setEnabled(true);
		}
		else{
			adaptiveBox.setEnabled(false);
			cellSizeField.setEnabled(false);
		}
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
	
    private FeatureCollection calculateConvexHulls(FeatureCollection pointFeatures, boolean isTrackData, String locAttribute, 
    		PlugInContext context, TaskMonitor monitor) {
    	//-- featureschema for all points in one track
    	FeatureSchema fsNewAllInOne = new FeatureSchema();
    	fsNewAllInOne.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewAllInOne.addAttribute("numpoints", AttributeType.INTEGER);
    	fsNewAllInOne.addAttribute("type", AttributeType.STRING);
    	fsNewAllInOne.addAttribute("area", AttributeType.DOUBLE);
    	//--
    	FeatureCollection fcResult = null;
    	fcResult = new FeatureDataset(fsNewAllInOne);
    	//-- convert every point
    	Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
    	if(firstFeature.getGeometry() instanceof Point){
    		if(monitor != null){
    			monitor.report("generating MCPs");
    		}
    		if(isTrackData){
	    		//-- sort according to location 
	    		if (monitor != null){
	    			monitor.report("sort by location");
	    		}
	    		FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
	    		pointFeatures = sortedPoints;
    		}
    		//-- generate the hulls from the points
    		if (monitor != null){
    			monitor.report("generate convex hulls");
    		}
    		List<Feature> allPoints = pointFeatures.getFeatures();
    		int numPoints = pointFeatures.size();
    		ArrayList<Feature> points = new ArrayList<Feature>();
			if(isTrackData){
    			Feature ftemp = allPoints.get(0);
    			points.add(ftemp);
    			ftemp = allPoints.get(1);
    			points.add(ftemp);
    			//start from 2 because we will not have an area for less than 3 points
				for (int i = 2; i < numPoints; i++) {
		    		if (monitor != null){
		    			monitor.report("processing points");
		    			monitor.report(i , numPoints, "generate MCP Home Ranges");
		    			if(monitor.isCancelRequested()){
		    				return fcResult;
		    			}
		    		}
	    			ftemp = allPoints.get(i);
	    			points.add(ftemp);
	    			Geometry hull = FeatureConversionUtils.createConvexHullFromFeatures(points);
	    			if (hull != null){
	    				Feature fnew = new BasicFeature(fsNewAllInOne); 
	    				fnew.setGeometry(hull);
	    				fnew.setAttribute("numpoints", i+1);
	    				fnew.setAttribute("type", "track-in-order");
	    				fnew.setAttribute("area", hull.getArea());
	    				fcResult.add(fnew);
	    			}
	    			else{
	    				System.out.println("calculateConvexHulls: could not create hull");
	    			}
				}
			}
    		else{//no trackdata
    			//-- we start with 3 because we will have no ConvexHull for less than 3 points
				for (int i = 3; i <= numPoints; i++) {
		    		if (monitor != null){
		    			monitor.report("processing points");
		    			monitor.report(i , numPoints, "generate MCP Home Ranges");
		    			if(monitor.isCancelRequested()){
		    				return fcResult;
		    			}
		    		}
    				//-- do 10 random draws and get the feature of median size
    				ArrayList<Geometry> tempGeoms = new ArrayList<Geometry>();
    				for (int j = 0; j < 10; j++) {
						//-- random draw
    					int[] rndIds = new int[i]; // i is the number of points to be considered 
    											   // in this loop
    		    		ArrayList<Feature> noTrackPoints = new ArrayList<Feature>();
    					for (int k = 0; k < rndIds.length; k++) {
        					int rndNumber = (int)Math.floor(Math.random() * numPoints);
        					rndIds[k] = rndNumber; //save for the record
        	    			Feature ftempNoTrPt = allPoints.get(rndNumber);
        					noTrackPoints.add(ftempNoTrPt);
						}
    					//-- create the convexHull
    	    			Geometry hull = FeatureConversionUtils.createConvexHullFromFeatures(noTrackPoints);
    	    			if (hull instanceof Polygon){
    	    				tempGeoms.add(hull);
    	    			}
    				}
    				Geometry gmedian = getMedianSizeFeature(tempGeoms);
    				Feature fnew = new BasicFeature(fsNewAllInOne); 
    				fnew.setGeometry(gmedian);
    				fnew.setAttribute("numpoints", i);
    				fnew.setAttribute("type", "median of 10");
    				fnew.setAttribute("area", gmedian.getArea());
    				fcResult.add(fnew);
    			}
    		}
    	}	
    	else{
    		if (context != null){
    			context.getWorkbenchFrame().warnUser("calculateConvexHulls: first feature not a point");
    		}
    		System.out.println("calculateConvexHulls: first feature not a point");
    		return null;
    	}   
    	return fcResult;
	}
  
	private FeatureCollection calculateLineBuffer(FeatureCollection pointFeatures,
			String locAttribute, String dayAttribute, PlugInContext context, TaskMonitor monitor) {
    	//-- featureschema for all points in one track
    	FeatureSchema fsNewAllInOne = new FeatureSchema();
    	fsNewAllInOne.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewAllInOne.addAttribute("numpoints", AttributeType.INTEGER);
    	fsNewAllInOne.addAttribute("type", AttributeType.STRING);
    	fsNewAllInOne.addAttribute("area", AttributeType.DOUBLE);
    	//--
    	FeatureCollection fcResult = null;
    	fcResult = new FeatureDataset(fsNewAllInOne);
    	//-- convert every point
    	Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
    	if(firstFeature.getGeometry() instanceof Point){
    		//-- sort according to location 
    		if (monitor != null){
    			monitor.report("sort by location");
    		}
    		FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
    		pointFeatures = sortedPoints;
	    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedPoints, 
	    			locAttribute, dayAttribute, context, monitor);
	    	//-- get the buffer distance
	        double[] errorDists = TrackCalculationUtil.calcOneDayActivityRadiusDistances(sortedPoints, this.dayAttribute, 
	        		this.input.getName(), false, context);
    	    double[] sortvals = DoubleArray.sort(errorDists);
    	    int index = (int)Math.ceil(errorDists.length/2.0);	                
	        double bufferDist = sortvals[index-1];
	        double meanDist = StatisticSample.mean(errorDists);
	        context.getWorkbenchFrame().warnUser("median buffer radius: " + bufferDist);
    		//-- generate the buffers from the lines
    		if (monitor != null){
    			monitor.report("generate Buffers");
    		}
    		List<Feature> allLines = singleLines.getFeatures();
    		int numLines = allLines.size();
    		ArrayList<Feature> linesToBuffer = new ArrayList<Feature>();
    		for (int i = 0; i < numLines; i++) {
    			if(monitor != null){
	    			monitor.report("processing");
    				monitor.report(i, numLines, "segments processed");
    				if(monitor.isCancelRequested()){
    					return fcResult;
    				}
    			}
    			Feature ftemp = allLines.get(i);
    			linesToBuffer.add(ftemp);
    	        //-- apply positive buffer op
    	        FeatureCollection buffer1FC = runBuffer(bufferDist, linesToBuffer, true);
    	        //-- apply negative buffer op
    	        double bufferDistInward = -1*this.negativeBufferFactor*bufferDist;
    	        FeatureCollection buffer2FC = runBuffer(bufferDistInward, buffer1FC.getFeatures(), true);
    	        //-- apply positive buffer op
    	        // here we set the union parameter false, since we do a union afterwards anyway
    	        double bufferDistOutward = -1*bufferDistInward;
    	        FeatureCollection buffer3FC = runBuffer(bufferDistOutward, buffer2FC.getFeatures(), false);
    	        //FeatureCollection buffer3FC = buffer1FC;
    	        //-- union
    	        ArrayList<Geometry> geoms = new ArrayList();
    	        for (Iterator iterator = buffer3FC.iterator(); iterator.hasNext();) {
					Feature feature = (Feature) iterator.next();
					geoms.add(feature.getGeometry());
				}
    	        Geometry union = UnaryUnionOp.union(geoms);

    			if (union != null){
    				Feature fnew = new BasicFeature(fsNewAllInOne); 
    				fnew.setGeometry(union);
    				fnew.setAttribute("numpoints", i+2);
    				fnew.setAttribute("type", "track-in-order");
    				fnew.setAttribute("area", union.getArea());
    				fcResult.add(fnew);
    			}
    			else{
    				System.out.println("calculateLineBuffer: could not create buffer");
    			}
    		}
    	}
    	else{
    		if (context != null){
    			context.getWorkbenchFrame().warnUser("calculateLineBuffer: first feature not a point");
    		}
    		System.out.println("calculateLineBuffer: first feature not a point");
    		return null;
    	}   
    	return fcResult;
	}
	
    private FeatureCollection calculatePointKDE(FeatureCollection pointFeatures, boolean isTrackData, boolean fixedHref, String locAttribute, 
    		double cellSIZE, PlugInContext context, TaskMonitor monitor) throws Exception {
    	ArrayList<Double> hrefList = new ArrayList<Double>();
    	ArrayList<Double> numPointList = new ArrayList<Double>();
    	//-- featureschema for all points in one track
    	FeatureSchema fsNewAllInOne = new FeatureSchema();
    	fsNewAllInOne.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewAllInOne.addAttribute("numpoints", AttributeType.INTEGER);
    	fsNewAllInOne.addAttribute("type", AttributeType.STRING);
    	fsNewAllInOne.addAttribute("area", AttributeType.DOUBLE);
    	//--
    	FeatureCollection fcResult = null;
    	fcResult = new FeatureDataset(fsNewAllInOne);
    	//-- convert every point
    	Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
    	if(firstFeature.getGeometry() instanceof Point){
    		//-- calculate href (KernelSize)
    		double hrefNormal = KernelDensityUtil.calculateHref(this.input);
    		double hrefNormalRounded = Math.round(hrefNormal);
    		if(fixedHref){
    			context.getWorkbenchFrame().warnUser("h_ref(NormalKernel) = " + hrefNormalRounded);
    		}
    		//-- get the Extent of the grid
    		OpenJUMPVectorLayer layer = new OpenJUMPVectorLayer();
    		layer.create(this.input);
    		AnalysisExtent gExtent = new AnalysisExtent(layer);
			//-- we will use a cell size of x meters
			gExtent.setCellSize(cellSIZE);
			//-- enlarge by kernel size (otherwise the outer points are missed)
			int ntimes = (int)Math.ceil(hrefNormalRounded/cellSIZE) + 2; // plus two reserve
			for( int i = 0; i < ntimes; i++){
				gExtent.enlargeOneCell();
			}
    		if(isTrackData){
	    		//-- sort according to location 
	    		if (monitor != null){
	    			monitor.report("sort by location");
	    		}
	    		FeatureCollection sortedPoints = TrackCalculationUtil.sortPointsByLocationId(this.points, this.locAttribute);
	    		pointFeatures = sortedPoints;
    		}
    		//-- generate the hulls from the points
    		List<Feature> allPoints = pointFeatures.getFeatures();
    		int numPoints = pointFeatures.size();
    		ArrayList<Feature> points = new ArrayList<Feature>();
    		//---
			if(isTrackData){
    			Feature ftemp = allPoints.get(0);
    			points.add(ftemp);
    			ftemp = allPoints.get(1);
    			points.add(ftemp);
    			//start from 2 because we will not have an area for less than 3 points
				for (int i = 2; i < numPoints; i++) {
	    			ftemp = allPoints.get(i);
	    			points.add(ftemp);
	    			//-- do calculations only every 10 points
	    			if(Math.floor(i/10.0) == (i/10.0)){
			    		if (monitor != null){
			    			monitor.report("processing points");
			    			monitor.report(i , numPoints, "generate KD Home Ranges");
			    			if(monitor.isCancelRequested()){
			    				return fcResult;
			    			}
			    		}
			    		if(fixedHref == false){			    			
			    			hrefNormalRounded = KernelDensityUtil.calculateHref(points);
			    			hrefList.add(hrefNormalRounded);
			    			numPointList.add((double)points.size());
			    		}
		    			Geometry polys = this.calculatePointKdeHrPolygon(points, hrefNormalRounded, this.propContour, 
		    					gExtent, this.KERNEL_SEXTANTE, context);
		    			if (polys != null){
		    				Feature fnew = new BasicFeature(fsNewAllInOne); 
		    				fnew.setGeometry(polys);
		    				fnew.setAttribute("numpoints", i+1);
		    				fnew.setAttribute("type", "track-in-order");
		    				fnew.setAttribute("area", polys.getArea());
		    				fcResult.add(fnew);
		    			}
		    			else{
		    				System.out.println("calculatePointKDE: could not create hull");
		    			}
	    			}
				}
				if(fixedHref == false){
					context.getWorkbenchFrame().getOutputFrame().createNewDocument();
					context.getWorkbenchFrame().getOutputFrame().addText("KDE Asymptote Analysis with adpative href, Layer : " + input.getName());
					int j = 0;
					for (Iterator iterator = hrefList.iterator(); iterator.hasNext();) {
						double hrefValue = (Double) iterator.next();
						double ptsNum = (Double)numPointList.get(j);
						context.getWorkbenchFrame().getOutputFrame().addText("points: " + ptsNum + " , h_ref: " + hrefValue);
						j++;
					}
					myhrefPlot = new ShowHrefPlot(hrefList, numPointList);
				}
				else{
					context.getWorkbenchFrame().getOutputFrame().createNewDocument();
					context.getWorkbenchFrame().getOutputFrame().addText("KDE Asymptote Analysis with fixed href, Layer : " + input.getName());
					context.getWorkbenchFrame().getOutputFrame().addText("href: " + hrefNormalRounded);
				}
			}
    		else{//no trackdata
    			context.getWorkbenchFrame().warnUser("PointKDE for non-track data not implemented");
    			/*
    			//-- we start with 3 because we will have no ConvexHull for less than 3 points
				for (int i = 3; i <= numPoints; i++) {
    				//-- do 10 random draws and get the feature of median size
    				ArrayList<Geometry> tempGeoms = new ArrayList<Geometry>();
    				for (int j = 0; j < 10; j++) {
						//-- random draw
    					int[] rndIds = new int[i]; // i is the number of points to be considered 
    											   // in this loop
    		    		ArrayList<Feature> noTrackPoints = new ArrayList<Feature>();
    					for (int k = 0; k < rndIds.length; k++) {
        					int rndNumber = (int)Math.floor(Math.random() * numPoints);
        					rndIds[k] = rndNumber; //save for the record
        	    			Feature ftempNoTrPt = allPoints.get(rndNumber);
        					noTrackPoints.add(ftempNoTrPt);
						}
    					//-- create the convexHull
    	    			Geometry hull = FeatureConversionUtils.createConvexHullFromFeatures(noTrackPoints);
    	    			if (hull instanceof Polygon){
    	    				tempGeoms.add(hull);
    	    			}
    				}
    				Geometry gmedian = getMedianSizeFeature(tempGeoms);
    				Feature fnew = new BasicFeature(fsNewAllInOne); 
    				fnew.setGeometry(gmedian);
    				fnew.setAttribute("numpoints", i);
    				fnew.setAttribute("type", "median of 10");
    				fnew.setAttribute("area", gmedian.getArea());
    				fcResult.add(fnew);
    			}
    			*/
    		}
    	}	
    	else{
    		if (context != null){
    			context.getWorkbenchFrame().warnUser("calculateConvexHulls: first feature not a point");
    		}
    		System.out.println("calculateConvexHulls: first feature not a point");
    		return null;
    	}   
    	return fcResult;
	}
    
	private Geometry calculatePointKdeHrPolygon(ArrayList<Feature> points,
			double h, double propContour, AnalysisExtent gExtent, int KernelType, 
			PlugInContext context) throws Exception {
		
		Geometry resultGeom = null;
		OutputFactory outputFactory2 = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		//-- add new weight Attribute and set it to one
		ArrayList<Geometry> geoms = new ArrayList();
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			geoms.add(f.getGeometry());
		}
		FeatureCollection pointsFC = FeatureDatasetFactory.createFromGeometry(geoms);
		FeatureCollection pointswWeight = FeatureCollectionTools.addAttributeToFeatureCollection(pointsFC, "kdeweight", 
				AttributeType.DOUBLE, new Double(1.0));
		//-- create a SextanteOJVectorLayer
		Layer layerOJ = new Layer("pts", context.getLayerManager().generateLayerFillColor(),
				pointswWeight, context.getLayerManager());
		OpenJUMPVectorLayer layerX = new OpenJUMPVectorLayer();
		layerX.create(layerOJ);
		//-- create KDE raster 
	    OpenJUMPRasterLayer raster = KernelDensityHRSplit.calculateKernelDensity(layerX, "kdeweight", h, cellSize, 
	    		this.KERNEL_SEXTANTE, gExtent, false, context);
	    System.gc();
	    
		RasterImageLayer outputRaster = (RasterImageLayer)raster.getBaseDataObject();
		//context.getLayerManager().addLayerable("KDE rasters", outputRaster);
		
		//-- derive xx percent contour
		double[] mappingResult = KernelDensityUtil.calculateContourHeightValueFromProbabilityValue(raster, propContour);
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
		//-- do union (create MultiGeom)
		ArrayList<Geometry> polygeoms = new ArrayList();        
        for (Iterator iterator = polys.iterator(); iterator.hasNext();) {
			Feature feature = (Feature) iterator.next();
			polygeoms.add(feature.getGeometry());
		}
        resultGeom = UnaryUnionOp.union(polygeoms);
		return resultGeom;
	}
	
	private Geometry getMedianSizeFeature(ArrayList<Geometry> tempGeoms) {
		ArrayList<Geometry> geoms = new ArrayList<Geometry>();
		ArrayList<Double> areas = new ArrayList<Double>();
		//-- init
		Geometry firstGeom = tempGeoms.get(0);
		geoms.add(firstGeom);
		areas.add(firstGeom.getArea());
		//-- sort into the list start with smallest area
		for (int i = 1; i < tempGeoms.size(); i++) {
			Geometry g = tempGeoms.get(i);
			double area = g.getArea();
			int j = 0;
			Iterator iterator = areas.iterator();
			boolean found = false;
			while(found == false){ 
				Double valInList = (Double) iterator.next();
				if(area <= valInList){
					//sort before that
					geoms.add(j, g);
					areas.add(j, area);
					found = true;
				}
				else{
					//continue to search for the break where area < valInList
					if(iterator.hasNext()){
						found = false;
					}
					else{
						//we are at the end, so add it
						geoms.add(j+1, g);
						areas.add(j+1, area);
						found = true;
					}
				}
				j++;		
			}
		}
		//-- get the median value
		int numOfGeoms = geoms.size();
		int medianIdx = ((int)Math.floor(numOfGeoms / 2.0))-1;
		Geometry medianGeom = geoms.get(medianIdx); 
		return medianGeom;
	}

	private double[] retrieveAreas(FeatureCollection features) {
		double[] areas = new double[features.size()];
		int i=0;
		for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature ftemp = (Feature) iterator.next();
			Geometry g = ftemp.getGeometry();
			areas[i] = g.getArea();
			i++;
		}
		return areas;
	}
	
	/**
	 * buffers the features in a layer
	 * @param bufferDistance
	 * @param inputFC
	 * @param doUnion perform a union of all buffers?
	 * @return
	 */
	public static FeatureCollection runBuffer(double bufferDistance, List<Feature> inputFC, boolean doUnion){
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
}

final class ShowAsymptotePlot extends JFrame{
	
	Plot2DPanelOJ plot = null;

	public ShowAsymptotePlot(FeatureCollection hullsFc){
			
		int ptsIdx = hullsFc.getFeatureSchema().getAttributeIndex("numpoints");
		// Build a 2D data set	    
		List<Feature> hulls = hullsFc.getFeatures();
		double[][] datas1 = new double [hullsFc.size()][2];
		double[][] datas2Deriv = new double [hullsFc.size()][2];
		for (int j = 0; j < datas1.length; j++) {	
			Feature f = hulls.get(j);
			datas1[j][0] = f.getInteger(ptsIdx);
			datas1[j][1] = f.getGeometry().getArea();
			// calculate rate of change
			datas2Deriv[j][0] = f.getInteger(ptsIdx);
			if(j>0){
				datas2Deriv[j][1] = datas1[j][1] - datas1[j-1][1];
			}
			else{
				datas2Deriv[j][1] = 0;
			}
		}	
		// Build the 2D scatterplot of the datas in a Panel
		// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
		Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
		plot2dA.addLinePlot("area",datas1);
		plot2dA.addLinePlot("changeOfArea",datas2Deriv);
		//plot2dA.addScatterPlot("area dots",datas1);
		//====================
		plot2dA.setAxisLabel(0,"number of points");
		plot2dA.setAxisLabel(1,"area");
		// Display a Frame containing the plot panel
		//new FrameView(plot2dA);		
		this.plot = plot2dA;
		
	}   
	
	public Plot2DPanelOJ getPlot(){
		return this.plot;
	}
	
}

final class ShowHrefPlot extends JFrame{
		
		Plot2DPanelOJ plot = null;

		public ShowHrefPlot(ArrayList<Double> hrefVals, ArrayList<Double> numPointList){
				
			double[][] datas1 = new double [hrefVals.size()][2];
			for (int j = 0; j < datas1.length; j++) {	
				datas1[j][0] = numPointList.get(j);
				datas1[j][1] = hrefVals.get(j);
			}	
			// Build the 2D scatterplot of the datas in a Panel
			// LINE, SCATTER, BAR, QUANTILE, STAIRCASE, (HISTOGRAMM?)		
			Plot2DPanelOJ plot2dA = new Plot2DPanelOJ();
			plot2dA.addLinePlot("h_ref values",datas1);
			//plot2dA.addScatterPlot("area dots",datas1);
			//====================
			plot2dA.setAxisLabel(0,"number of points");
			plot2dA.setAxisLabel(1,"h_ref");
			// Display a Frame containing the plot panel
			//new FrameView(plot2dA);		
			this.plot = plot2dA;
			
		}   
		
		public Plot2DPanelOJ getPlot(){
			return this.plot;
		}
}