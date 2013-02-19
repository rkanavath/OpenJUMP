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
 * last modified:   12.Nov.2012								
 * 
 * @author sstein
 * 
 * description: Displays the MCP of animal observations (converts points to convex hull polygons)
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

import org.math.array.StatisticSample;
import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.FeatureConversionUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
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

/**
 * @description: Creates MCPs/Convex hulls for a point set. The number of points can be specified in percent,
 * and they can also be sorted into subsets using an attribute value.
 *
 * @author sstein
 *
 **/
public class CalculateMinimumConvexPolygonPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Displays the MCP of animals (converts points to convex hull polygons)";   
    private final String sLAYERPTS = "Layer with Points";
    private FeatureCollection points = null;        
    private Layer input = null;
    private MultiInputDialog dialog;
    private JCheckBox usePercentBox;
    private JTextField percentageField;
    private JComboBox centreMethodComboBox = null;
    private PlugInContext context = null;
	private String idAttribute = "";
    private String sATTRIBUTEA = "id Attribute";
    private String sGENERATELAYERS = "one output layer per indiviual";
    private boolean generateLayerPerObject = false;
    private boolean usePercentage = false;
    private double percentage = 0.95;
    private String sPERCENTAGEOPTION = "Calculate MCP for fraction of points (distance from centre)";
    private String sPERCENTAGE = "Fraction of points to be used [0...1.0]";
    private String sSELECTCENTRECALC = "Select centre calculation method";
    private List calcMethods;
    private int CALC_MEAN = 0;
    private int CALC_MEDIAN = 1;
    private String calcSelected;
    private String sCALC_MEAN = "use Fixed Mean of points locations";
    private String sCALC_MEDIAN = "use Fixed Median of points locations";
    private int selectedCentreCalculation = 0;
    
    GeometryFactory gfactory = new GeometryFactory();
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE"}, 	//menu path
                    this,
                    new JMenuItem("Calculate Minimum Convex Polygon...", null),
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
	    	FeatureCollection resultC = this.calculateConvexHulls(this.points, this.idAttribute, this.generateLayerPerObject, 
	    			this.usePercentage, this.percentage, this.selectedCentreCalculation, context, monitor);
	        if(this.generateLayerPerObject == false){
	        	context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-tracks", resultC);
	        }
    	    System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "MCP", true);
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
        dialog.addCheckBox(this.sGENERATELAYERS, this.generateLayerPerObject, this.sGENERATELAYERS);
        
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
        this.usePercentBox = dialog.addCheckBox(this.sPERCENTAGEOPTION, this.usePercentage, this.sPERCENTAGEOPTION);
        this.percentageField = dialog.addDoubleField(this.sPERCENTAGE, this.percentage, 4);
        this.percentageField.setEnabled(this.usePercentBox.isSelected());
 	    //-- add listener
 	   usePercentBox.addActionListener(new ActionListener() {
 	        public void actionPerformed(ActionEvent e) {
 	            updateControls();
 	        }
 	    });
 	    calcMethods = new ArrayList();
 	    calcMethods.add(this.sCALC_MEAN);
 	    calcMethods.add(this.sCALC_MEDIAN); 
 	    this.calcSelected = this.sCALC_MEAN;
	    this.centreMethodComboBox = dialog.addComboBox(sSELECTCENTRECALC, this.calcSelected, calcMethods, null);
		this.centreMethodComboBox.setEnabled(this.usePercentBox.isSelected());

        GUIUtil.centreOnWindow(dialog);
    }
	
	private void updateControls() {
		//System.out.print("process update method: ");
		if (this.usePercentBox.isSelected()){
			this.percentageField.setEnabled(true);
			this.centreMethodComboBox.setEnabled(true);
			//System.out.print(" switch on ");
		}
		else{
			this.percentageField.setEnabled(false);
			this.centreMethodComboBox.setEnabled(false);
			//System.out.print(" switch off ");
		}
	}
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
        this.idAttribute = dialog.getText(this.sATTRIBUTEA); 
        this.generateLayerPerObject = dialog.getBoolean(this.sGENERATELAYERS); 
        this.usePercentage = dialog.getBoolean(this.sPERCENTAGEOPTION);
        this.percentage = dialog.getDouble(this.sPERCENTAGE);
        
        this.calcSelected = dialog.getText(sSELECTCENTRECALC);
        this.selectedCentreCalculation = getcalcMethodCode(this.calcSelected);
      }
    
    private int getcalcMethodCode(String type)
    {
      if (type == sCALC_MEAN) return CALC_MEAN;
      if (type == sCALC_MEDIAN) return CALC_MEDIAN;
      return CALC_MEDIAN;
    }
	/**
	 * Creates convex hulls for subsets of points, sorted by the idAttribute. Points further away from the
	 * centre can be excluded with the percentage parameter.  
	 * @param pointFeatures
	 * @param idAttribute
	 * @param timeAttributeNames[yearField, monthField, dayField]
	 * @param usePercentage true if only a percentage of the points should be used
	 * @param percentage the subset of points in percent [0..100] to be used.
	 * @param percentageMethod method used for the centre calculation to retrieve subsets 0: Mean or 1: Median
	 * @param context used to draw several result layers.
	 * @param monitor can be null
	 * @return the convex hull polygon(s) in a FeatureCollection. Result can also be null 
	 * for usePercentage = true and the method defined for percentageMethod calculation is not known 
	 */
    private FeatureCollection calculateConvexHulls(FeatureCollection pointFeatures, String idAttribute, boolean individualLayers, boolean usePercentage, 
    		double percentage, int percentageMethod, PlugInContext context, TaskMonitor monitor) {
    	//-- featureschema for all points in one track
    	FeatureSchema fsNewAllInOne = new FeatureSchema();
    	fsNewAllInOne.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewAllInOne.addAttribute("individual_id", AttributeType.INTEGER);
    	List<Geometry> centrePts = new ArrayList(); 
    	//--
    	FeatureCollection fcResult = null;
    	fcResult = new FeatureDataset(fsNewAllInOne);
    	//-- convert every point
    	Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
    	if(firstFeature.getGeometry() instanceof Point){
    		if(monitor != null){
    			monitor.report("generating MCP");
    		}	
    		//-- sort according to individual id
    		if (monitor != null){
    			monitor.report("sort by individual");
    		}
    		ArrayList<Feature>[] individualPts = null;
    		Object[] myReturns = FeatureCollectionTools.sortFeaturesIntoListsByAttributeValue(pointFeatures, idAttribute); 
    		individualPts = (ArrayList[])myReturns[0]; 
    		int[] uniqueValues = (int[])myReturns[1];

    		//-- generate the hulls from the points
    		if (monitor != null){
    			monitor.report("generate convex hulls");
    		}
			boolean htmlFrameInit = false;
    		for (int i = 0; i < individualPts.length; i++) {
    			List<Feature> points = null;
    			if(usePercentage){
    				if(htmlFrameInit == false){
    					context.getWorkbenchFrame().getOutputFrame().createNewDocument();
    					context.getWorkbenchFrame().getOutputFrame().addText("MCP for Layer: " + input.getName());
    					context.getWorkbenchFrame().getOutputFrame().addText("--- Subset 1");
    					htmlFrameInit = true;
    				}
    				else{			
    					context.getWorkbenchFrame().getOutputFrame().addText("--- Subset " + i);
    				}
    				points = new ArrayList<Feature>();
    				List<Feature> pointsToEvaluate = individualPts[i];
    				int numPts = pointsToEvaluate.size();
    				double[] locX = new double[numPts];
    				double[] locY = new double[numPts];
    				Feature[] features = new Feature[numPts];
    				double[] dists = new double[numPts];
    				// calculate the centre
    				String methodUsed = "";
    				int r = 0;
    				for (Iterator iterator = pointsToEvaluate.iterator(); iterator
							.hasNext();) {
						Feature feature = (Feature) iterator.next();
						Point pt = (Point)feature.getGeometry();
						locX[r] = pt.getX();
						locY[r] = pt.getY();
						features[r] = feature;
						r++; 
					}
    				Point centrePt = null;
    				if(percentageMethod == 0){ //use mean
    					methodUsed = "mean";
	    				double meanx = StatisticSample.mean(locX);
	    				double meany = StatisticSample.mean(locY);
	    				Coordinate meanCo = new Coordinate(meanx, meany);
	    				centrePt = new GeometryFactory().createPoint(meanCo);
    				}
	    			else if(percentageMethod == 1){ //use median
	    				methodUsed = "median";
	    				int posMedian = (int) Math.floor(0.5 * numPts);  
	    				double[] sortedX = StatisticSample.sort(locX);
	    				double[] sortedY = StatisticSample.sort(locY);
	    				double medianx = sortedX[posMedian];
	    				double mediany = sortedY[posMedian];
	    				Coordinate medianCo = new Coordinate(medianx, mediany);
	    				centrePt = new GeometryFactory().createPoint(medianCo);
	    			}
	    			else{
	    				return null;
	    			}
    				centrePts.add(centrePt);
    				// evaluate distance to meanCo
    				for (int j = 0; j < features.length; j++) {
						Geometry geom = features[j].getGeometry();
						dists[j] = geom.distance(centrePt);
					}
    				// get the distance that splits the set according to the percentage
    				double[] sortedDist = StatisticSample.sort(dists); //sorts in ascending order
    				int pos = (int)Math.floor(percentage * numPts);
    				double splitDist = sortedDist[pos];
    				for (int j = 0; j < features.length; j++) {
						Feature ft = features[j];
						if (dists[j] <= splitDist){
							points.add(ft);
						}
					}
    				// some output
    				context.getWorkbenchFrame().getOutputFrame().addText("points in subset: " + numPts);
    				context.getWorkbenchFrame().getOutputFrame().addText("percentage set: " + percentage);
    				context.getWorkbenchFrame().getOutputFrame().addText("centre calculation method: " + methodUsed);
    				context.getWorkbenchFrame().getOutputFrame().addText("split distance to centre: " + splitDist + " (sorting position: " + pos + " / " + (numPts-1) + ")");
    			}
    			else{
    				points = individualPts[i];
    			}
    			Geometry hull = FeatureConversionUtils.createConvexHullFromFeatures(points);
    			if (hull != null){
    				Feature fnew = new BasicFeature(fsNewAllInOne); 
    				fnew.setGeometry(hull);
    				fnew.setAttribute("individual_id", uniqueValues[i]);
    				fcResult.add(fnew);
    				if (individualLayers){
    					//-- display
    					FeatureCollection fcIndiv = new FeatureDataset(fsNewAllInOne);
    					fcIndiv.add(fnew.clone(true));
    					if (context != null){
    						context.addLayer(StandardCategoryNames.RESULT, 
    								this.input.getName() + "_MCP_" + uniqueValues[i], fcIndiv);
    					}
    				}
    			}
    			else{
    				System.out.println("calculateConvexHulls: could not create LineString");
    			}
    		}//end loop over all individuals
			if((context != null) && (centrePts.size() > 0)){
				FeatureCollection fcCentre = FeatureDatasetFactory.createFromGeometry(centrePts);
				context.addLayer(StandardCategoryNames.RESULT, 
						this.input.getName() + "_MCP_centres", fcCentre);
			}
    	}//check if geometries are points	
    	else{
    		if (context != null){
    			context.getWorkbenchFrame().warnUser("calculateConvexHulls: first feature not a point");
    		}
    		System.out.println("calculateConvexHulls: first feature not a point");
    		return null;
    	}   
    	return fcResult;
	}
  
}
