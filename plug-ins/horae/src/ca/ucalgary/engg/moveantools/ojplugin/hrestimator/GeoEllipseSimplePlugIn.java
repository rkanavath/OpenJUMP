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
 * created:  		25.Oct.2012
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description: Basic GeoEllipse approach after J. Long and T. Nelson  (2012)
 * 	
 *  
 *****************************************************/

package ca.ucalgary.engg.moveantools.ojplugin.hrestimator;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;
import javax.swing.JTextField;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.rasterimage.RasterImageLayer;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.LineBufferDensityAlgorithm;
import ca.ucalgary.engg.moveantools.util.TrackCalculationUtil;

import com.vividsolutions.jts.algorithm.Angle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.operation.union.UnaryUnionOp;
import com.vividsolutions.jts.util.GeometricShapeFactory;
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
 * @description: Creates geo-ellipses know from time-geography for GPS tracks.
 * This plugin implements the basic GeoEllipse approach after J.A. Long and T.A. Nelson  (JWM, 2012), which
 * they call Potential Path Area. Another similar algorithm exists by J.A. Downs (GIScience Conf., 2010).
 * 
 * Note: the raster density calculation re-uses LineBufferDensityAlgorithm.java.
 * @author sstein
 *
 **/
public class GeoEllipseSimplePlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Creates Geo-Ellipse polygons after Long & Nelson (2012), i.e. so-called Potential Path Areas.";   
    public double cellSize = 25.0;
    public double vMax = 5.0;
    public int numEllipsePoints = 20;
    final public String HOUR_ATTRIBUTE_NAME = "TimeInHours";
    
    private final String sLAYERPTS = "Layer with Points";
    private final String sDetermineVmax = "Calculate max. movement speed from data after Robson and Whitlock (1964).";
    private final String sVmax = "Maximum movement speed [km/h]";
    //private final String sUseMean = "use mean instead of median";
    private String sATTRIBUTEA = "Location Attribute (for sorting)";
    private String sATTRIBUTEDay = "Day Attribute (for time difference, no string fields, unqiue per year!)";
    private String sATTRIBUTEHour = "Hour Attribute (for time difference, no string fields)";
    private final String sDoUnion = "Merge all Geo-Ellipses";
    private final String sCreateDensityGrid = "Create usage density grid - so probability contours can be calculated later";
    private final String sCellSize = "Raster cell size for density grid (in m)";
    
    private boolean determineVmax = false;
    public boolean doUnion = true;
    //private boolean useMean = false;
    private boolean createDensityGrid = false;
	private String locAttribute = "";
	private String dayAttribute = "";	
	private String hourAttribute = "";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    //private JCheckBox useMeanBox = null;
    private JCheckBox determineVmaxBox = null;
    private JCheckBox doUnionBox = null;
    private JCheckBox createDensityBox = null;
    private JComboBox jcb_attributeA = null;
    private JComboBox jcb_attributeB = null;
    private JComboBox jcb_attributeC = null;
    JTextField vmaxField = null;
    JTextField densityField = null;
    private PlugInContext context = null;
    
    GeometryFactory gfactory = new GeometryFactory();
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                new String[] {"MOVEAN", "HRE"}, 	//menu path
                this,
                new JMenuItem("GeoEllipse...", null),
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
    	FeatureCollection sortedAndOneTimeAttribute = TrackCalculationUtil.aggregateDayAndHourForPoints(sortedPoints, this.dayAttribute, 
    			this.hourAttribute, HOUR_ATTRIBUTE_NAME);
    	//-- generate tracks
    	FeatureCollection singleLines = TrackCalculationUtil.convertToLines(sortedAndOneTimeAttribute, 
    			this.locAttribute, HOUR_ATTRIBUTE_NAME, context, monitor);
        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", singleLines);
    	    System.gc();    	
    	    
        double vMaxUsed = this.vMax;
        String sVMethod = "";
        //-- Determine vMax if the user wishes so
        if(this.determineVmax){
        	double[] maxSpeedsDetected = this.getMaxSpeed(singleLines, context.getWorkbenchContext());
        	if( maxSpeedsDetected[0] > maxSpeedsDetected[1]){
        		if(maxSpeedsDetected[1] > 0){
	        		vMaxUsed = maxSpeedsDetected[0] + (maxSpeedsDetected[0] - maxSpeedsDetected[1]);
	        		sVMethod = "using Robson and Whitlock (1964) estimate";
        		}
        		else{
            		vMaxUsed = maxSpeedsDetected[0] + 0.5;
            		sVMethod = "by adding 0.5 km/h - because v_(m-1) = 0";
        		}
        	}
        	else{ //note: this case should not be entered, since maxSpeedsDetected[0] == maxSpeedsDetected[1]
        		  //      is not possible the way the function is programmed. I.e. both values will be different.
        		vMaxUsed = maxSpeedsDetected[0] + 0.5;
        		sVMethod = "by adding 0.5 km/h - because v(m) = v_(m-1)";
        	}
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("Layer: " + input.getName());
			context.getWorkbenchFrame().getOutputFrame().addText("calculated maximum speed v(m): " + maxSpeedsDetected[0] + 
					" km/h, second fastest v(m-1): " + maxSpeedsDetected[1]);
			context.getWorkbenchFrame().getOutputFrame().addText("used maximum speed for GeoEllipse: " + vMaxUsed + " km/h, " + sVMethod);
        }
        
        //-- create GeoEllipse
    	if(vMaxUsed > 0){
	        monitor.report("creating ellipses with v_max = " + vMaxUsed);	
	        FeatureCollection geoEllipseFC = calcGeoEllipses(vMaxUsed, singleLines, doUnion, this.numEllipsePoints, context.getWorkbenchContext());
	        context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-ellipses", geoEllipseFC);
    	}
    	else{
    		context.getWorkbenchFrame().warnUser("v_max = 0! No results!");
    	}
        //-- do density raster if wished.
        if(createDensityGrid){
        	RasterImageLayer raster = calcDensityGrid(singleLines, vMaxUsed, cellSize, this.numEllipsePoints, 
        			context, monitor);
        	context.getLayerManager().addLayerable(StandardCategoryNames.RESULT, raster);
        }
    }

	private void initDialog(PlugInContext context) {
    	
	       dialog = new MultiInputDialog(context.getWorkbenchFrame(), "GeoEllipse HR", true);
	        dialog.setSideBarDescription(sSidebar);
	        try {
	        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
	        }
	        catch (IndexOutOfBoundsException e) {}
	        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
	        Object valA = list.size()>0?list.iterator().next():null;
	        jcb_attributeA = dialog.addComboBox(this.sATTRIBUTEA, valA, list,this.sATTRIBUTEA);
	        if (list.size() == 0) jcb_attributeA.setEnabled(false);
	        Object valB = list.size()>0?list.iterator().next():null;
	        jcb_attributeB = dialog.addComboBox(this.sATTRIBUTEDay, valB, list,this.sATTRIBUTEDay);
	        if (list.size() == 0) jcb_attributeB.setEnabled(false);
	        Object valC = list.size()>0?list.iterator().next():null;
	        jcb_attributeC = dialog.addComboBox(this.sATTRIBUTEHour, valC, list,this.sATTRIBUTEHour);
	        if (list.size() == 0) jcb_attributeC.setEnabled(false);
	        doUnionBox = dialog.addCheckBox(sDoUnion, doUnion);	        
	        determineVmaxBox = dialog.addCheckBox(sDetermineVmax, determineVmax);
	        dialog.addSeparator();
		    vmaxField = dialog.addDoubleField(sVmax, this.vMax, 8);
	        //useMeanBox = dialog.addCheckBox(sUseMean, useMean);
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
	                    jcb_attributeC.setModel(new DefaultComboBoxModel(new String[0]));
	                    jcb_attributeC.setEnabled(false);
	                }
	                jcb_attributeA.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
	                jcb_attributeB.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
	                jcb_attributeC.setModel(new DefaultComboBoxModel(list.toArray(new String[0])));
	            }            
	        });
	        this.updateControls();
	        this.updateControlsDensity();
	 	    //-- add listenerz
	        determineVmaxBox.addActionListener(new ActionListener() {
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
		    if (determineVmaxBox.isSelected()){
	        	//useMeanBox.setEnabled(true);
	        	jcb_attributeA.setEnabled(true);
		    	vmaxField.setEnabled(false);
		    }
		    else{
	        	//useMeanBox.setEnabled(false);
	        	jcb_attributeA.setEnabled(true);
		    	vmaxField.setEnabled(true);
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
    	this.determineVmax = determineVmaxBox.isSelected();
    	this.vMax = dialog.getDouble(sVmax);
    	this.doUnion = doUnionBox.isSelected();
    	//this.useMean = useMeanBox.isSelected();
    	this.createDensityGrid = createDensityBox.isSelected();
    	this.cellSize = dialog.getDouble(this.sCellSize);
        this.locAttribute = dialog.getText(this.sATTRIBUTEA);
        this.dayAttribute = dialog.getText(this.sATTRIBUTEDay);
        this.hourAttribute = dialog.getText(this.sATTRIBUTEHour);
      }
    
    /**
     * calculates the GeoEllipses for each track segment.
     * @param travelSpeed in km/h
     * @param inputFC
     * @param doUnion
     * @param numberOfEllipsePoints (more points means more processing, especially for union operation)
     * @param context can be null
     * @return FeatureCollection with the GeoEllipses
     */
	public static FeatureCollection calcGeoEllipses(double travelSpeed, FeatureCollection inputFC, boolean doUnion, int numberOfEllipsePoints, WorkbenchContext context){
	    FeatureSchema fSchema = new FeatureSchema();
	    fSchema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
	    fSchema.addAttribute("ellipseProcID", AttributeType.INTEGER);
	    FeatureCollection resultFC = new FeatureDataset(fSchema);
	    //-- create ellipses
	    Geometry firstGeom = ((Feature)(inputFC.getFeatures().get(0))).getGeometry();
	    GeometricShapeFactory gsf = new GeometricShapeFactory(firstGeom.getFactory());
	    
	    int timeDiffAttributeID = inputFC.getFeatureSchema().getAttributeIndex(TrackCalculationUtil.TIME_DIFFERENCE_ATTRIBUTE_NAME);
	    
	    gsf.setNumPoints(numberOfEllipsePoints);
	    ArrayList<Geometry> geoms = new ArrayList<Geometry>();
		for (Iterator iterator = inputFC.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			Geometry geom = f.getGeometry();
			Coordinate[] coords = geom.getCoordinates();
		    Geometry result = null;
		    try {
		    	// Calculate ellipse params
		    	double deltaT = f.getDouble(timeDiffAttributeID);
		    	double d = geom.getLength();
		    	double a = deltaT * (travelSpeed * 1000); //TODO: ensure that deltaT and travelSpeed are of same units
		    											  //multiplication with 1000 for km => m
		    											  //however, time is still a problem
		    	if(d > a){
		    		if(context != null){
		    			context.getWorkbench().getFrame().warnUser("Set speed is wrong, the animal moves faster!");
		    		}
		    	}
		    	else{
			    	double b = Math.sqrt((a*a) - (d*d));
			    	// set dimensions of ellipse box
			    	gsf.setWidth(a);
			    	gsf.setHeight(b);
			    	gsf.setCentre(geom.getCentroid().getCoordinate());
			    	double rot = Angle.angle(coords[0], coords[coords.length-1]);
			    	gsf.setRotation(rot);
			    	result = gsf.createEllipse();
		    	}
			    
		    }
		    catch (RuntimeException ex) {
		        // simply eat exceptions and report them by returning null
		    	System.out.println("Exeption: Could not generate Ellipse for line ID " +  f.getAttribute("lineProcID"));
		    }
		    if (result != null){
		    	 geoms.add(result);
		    	 Feature ftemp = new BasicFeature(fSchema);
		    	 ftemp.setGeometry(result);
		    	 ftemp.setAttribute("ellipseProcID",f.getAttribute("lineProcID"));
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

	/**
	 * Evaluates the maximum speed reached by the animal.
	 * This method assumes that "timeDiff" is given in hours.
	 * @param singleLines
	 * @param context can be null
	 * @return a double array[2] with the maximum speed at [0] and second largest speed at [1], whereby both speeds will not be equal.  
	 */
	private double[] getMaxSpeed(FeatureCollection singleLines, WorkbenchContext context) {
		double[] speeds = {0.0, 0.0};
		double maxSpeed = 0;
		double secondSpeed = 0;
		for (Iterator<Feature> iterator = singleLines.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			double length = f.getGeometry().getLength();
			double timeDiff = (Double)f.getAttribute(TrackCalculationUtil.TIME_DIFFERENCE_ATTRIBUTE_NAME);
			double v = ( length / 1000.0) / timeDiff; // divide by 1000 to have km/h
			if(v < 500){ //do a plausibility check with 500 km/h (the Peregrin Falcon is supposedly fastest with >300km/h)
				if (v > maxSpeed){
					secondSpeed = maxSpeed;
					maxSpeed = v;
				}
			}
			else{
				if(context != null){
					context.getWorkbench().getFrame().warnUser("Detected speed > 500 km/h");
				}
			}
		}
		speeds[0] = maxSpeed;
		speeds[1] = secondSpeed;
		return speeds;
	}
	
	/**
	 * Calculates a density raster for the Geo-Ellipses. Note that the geoEllipses will be
	 * recreated by this algorithm, to ensure that no union takes place.
	 * Important: this algorithm re-uses LineBufferDensityAlgorithm.java!
	 * @param singleLines
	 * @param maxSpeed
	 * @param cellSize
	 * @param numberOfEllipsePoints
	 * @param context can be null
	 * @param monitor can be null
	 * @return a raster/grid with density values 
	 * @throws GeoAlgorithmExecutionException
	 */
	private RasterImageLayer calcDensityGrid(FeatureCollection singleLines,
			double maxSpeed, double cellSize, int numberOfEllipsePoints, PlugInContext context, TaskMonitor monitor) 
			throws GeoAlgorithmExecutionException {
		
		//-- get the ellipses
        FeatureCollection geoEllipseFC = calcGeoEllipses(maxSpeed, singleLines, false, numberOfEllipsePoints, context.getWorkbenchContext());
        
        //Sextante.initialize();
        
		OutputFactory outputFactory = new OpenJUMPOutputFactory(context.getWorkbenchContext());
		if(OJSextanteApiInitialiser.isInitialized == false){
			OJSextanteApiInitialiser.initializeSextante(context);
		}
		OpenJUMPVectorLayer ojEllipseLayer = new OpenJUMPVectorLayer();
		
		Layer ellipseLayer = new Layer("usagegrid", context.getLayerManager().generateLayerFillColor(),
				geoEllipseFC, context.getLayerManager());
		//--
		ojEllipseLayer.create(ellipseLayer);
		
		// we use the same algorithm as for the LineBufferDensity
		LineBufferDensityAlgorithm alg = new LineBufferDensityAlgorithm();
		ParametersSet params = alg.getParameters();
		params.getParameter(LineBufferDensityAlgorithm.LAYER).setParameterValue(ojEllipseLayer);
		params.getParameter(LineBufferDensityAlgorithm.IDFIELD).setParameterValue(ojEllipseLayer.getFieldIndexByName("ellipseProcID"));
		
		//-- we will use a cell size of x meters
		AnalysisExtent extent = new AnalysisExtent(ojEllipseLayer);
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
