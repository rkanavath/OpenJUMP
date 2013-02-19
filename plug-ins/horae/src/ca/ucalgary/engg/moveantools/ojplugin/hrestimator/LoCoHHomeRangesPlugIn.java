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
 * created:  		07.May.2010
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JMenuItem;
import javax.swing.JRadioButton;

import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.FeatureConversionUtils;
import ca.ucalgary.engg.moveantools.util.KernelDensityUtil;
import ca.ucalgary.engg.moveantools.util.LoCoHUtil;
import ca.ucalgary.engg.moveantools.util.geom.SecondGeodeticTask2d;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
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

/**
 * @description: Home range analysis with the Local Convex Hull Approach (LoCoH)
 * described in WM Getz et al (2007), and Getz and Wilmers (2004)
 *	
 * @author sstein
 *
 **/
public class LoCoHHomeRangesPlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Creates home ranges based on LoCoH (Getz 2007). " +
    		"LoCoH-R : union of convex hulls of points within circle of radius r [meter]. " +
    		"LoCoH-K : union of convex hulls of k-1 neighbour points. " +
    		"LoCoH-a : union of convex hulls of neighbour points those sum of distances <= a [meter].";   
    private final String sLAYERPTS = "Layer with Points";
    private String sValue = "Parameter Value";
    
    public double negativeBufferFactor = 1.1;
    private boolean isLoCoHR = true;
    private boolean isLoCoHk = false;
    private boolean isLoCoHa = false;
    private String sLoCoHr= "LoCoH-R [radius in meter]";
    private String sLoCoHk= "LoCoH-k [number of points]";
    private String sLoCoHa= "LoCoH-a [distance in meter]";
    private String sCalcIsopleths = "check to calc 10%, 30%, 50%, 70%, 90% isopleths";
    
    private FeatureCollection points = null;        
    private Layer input = null;
    private double paramValue = 0;
    private boolean calcIsopleths = false; 
    private MultiInputDialog dialog;
    private PlugInContext context = null;
    private JRadioButton buttonLocohr = null;
    private JRadioButton buttonLocohk = null;
    private JRadioButton buttonLocoha = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HRE"}, 	//menu path
                    this,
                    new JMenuItem("LoCoH Home Ranges...", null),
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
	    	//-- put all points in a tree/index
	    	STRtree rtree = KernelDensityUtil.createSTRreeofPoints(this.points);
	    	//-- select the LoCoH approach: LoCoH-k, LoCoH-R, LoCoH-A
	    	FeatureCollection result = null;
	    	String sLocohMethod = "";
	    	if(this.isLoCoHR){
	    		sLocohMethod = "R";
	    		result = getLocohR(this.points, this.paramValue, rtree, this.calcIsopleths, context, monitor);
	    	}
	    	else if(this.isLoCoHk){
	    		sLocohMethod = "k";
	    		result = getLocohK(this.points, (int)this.paramValue, rtree, this.calcIsopleths, context, monitor);
	    	}
	    	else if(this.isLoCoHa){
	    		sLocohMethod = "a";
	    		result = getLocohAlpha(this.points, this.paramValue, rtree, this.calcIsopleths, context, monitor);
	    	}
	    	if(result != null){
	    		context.addLayer(StandardCategoryNames.RESULT, this.input.getName() + "-LoCoH-" + sLocohMethod, result);
	    	}
	    	System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "LoCoH Home Ranges", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERPTS, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        List list = FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(context.getCandidateLayer(0));
        final String LOCOHMETHODGROUP = "LoCoH method";
        buttonLocohr = dialog.addRadioButton(sLoCoHr, LOCOHMETHODGROUP, this.isLoCoHR, "LocoH-R: creates local convex hull for all neighbour points within circle of radius r [in meters].");
        buttonLocohk = dialog.addRadioButton(sLoCoHk, LOCOHMETHODGROUP, this.isLoCoHk, "LocoH-K: creates local convex hull for all k-1 neighbour points.");
        buttonLocoha = dialog.addRadioButton(sLoCoHa, LOCOHMETHODGROUP, this.isLoCoHa, "LocoH-a: creates local convex hull for all neighbour points of which the sum of the distances <= alpha.");
        dialog.addDoubleField(sValue, this.paramValue, 8);
        dialog.addCheckBox(sCalcIsopleths, this.calcIsopleths);
        GUIUtil.centreOnWindow(dialog);
    }
	
    private List getFieldsFromLayerWithoutGeometryAndString() {
        return FeatureSchemaTools.getFieldsFromLayerWithoutGeometryAndString(dialog.getLayer(this.sLAYERPTS));
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.input =  dialog.getLayer(this.sLAYERPTS);
    	this.points= this.input.getFeatureCollectionWrapper(); 
    	this.paramValue = dialog.getDouble(this.sValue);
    	this.isLoCoHR = dialog.getBoolean(this.sLoCoHr);
    	this.isLoCoHk = dialog.getBoolean(this.sLoCoHk);
    	this.isLoCoHa = dialog.getBoolean(this.sLoCoHa);
    	this.calcIsopleths = dialog.getBoolean(this.sCalcIsopleths);
      }
	
    /**
     * calculates the home range regions with the LoCoH-R method, i.e. calculation of local convex 
     * hulls for all neighbors within a circle of radius R.
     * @param points
     * @param radius
     * @param tree
     * @param calcIsopleths
     * @param context
     * @param monitor
     * @return
     */
    public static FeatureCollection getLocohR(FeatureCollection points, double radius, STRtree tree,
    		boolean calcIsopleths, PlugInContext context, TaskMonitor monitor){
    	FeatureSchema fs = new FeatureSchema();
    	fs.addAttribute("geometry", AttributeType.GEOMETRY);
    	fs.addAttribute("percent-cover-hull", AttributeType.DOUBLE);
    	fs.addAttribute("param", AttributeType.DOUBLE);
    	FeatureCollection fcRes = new FeatureDataset(fs);
    	
    	ArrayList<Geometry> hulls = new ArrayList<Geometry>();
    	//-- create the local hull for each point
    	for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature ptf = (Feature) iterator.next();
			Geometry circle = ptf.getGeometry().buffer(radius); 
			List candidates = tree.query(circle.getEnvelopeInternal());
			ArrayList<Feature> hullPoints = new ArrayList();
			//-- get the points that are within the radius
			for (Iterator iterator2 = candidates.iterator(); iterator2
					.hasNext();) {
				Feature pt2test = (Feature) iterator2.next();
				double dist = SecondGeodeticTask2d.calcDistancePoints((Point)ptf.getGeometry(), (Point)pt2test.getGeometry());
				if (dist <= radius){
					hullPoints.add(pt2test);
				}
			}
			//-- don't need to add the point itself, since it will accepted anyway since dist = 0
			//-- get the convex hull if there are at least 3 points (i.e. we should have a polygon)
			if(hullPoints.size() >= 3){ 
				Geometry cHull = FeatureConversionUtils.createConvexHullFromFeatures(hullPoints);
				//-- store the number of points for each hull
				cHull.setUserData(new Double(hullPoints.size()));
				hulls.add(cHull);
			}
    	}
    	//-- calculate for each hull the density and create a Feature so we can use the featureSorting method
    	FeatureSchema fsDens = new FeatureSchema();
    	fsDens.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsDens.addAttribute("density", AttributeType.DOUBLE);
    	fsDens.addAttribute("pts", AttributeType.DOUBLE);
    	fsDens.addAttribute("area", AttributeType.DOUBLE);
    	ArrayList<Feature> hullsDensity = new ArrayList<Feature>();
    	for (Iterator iterator = hulls.iterator(); iterator.hasNext();) {
			Geometry geom = (Geometry) iterator.next();
			double area = geom.getArea();
			double numPts = ((Double)geom.getUserData()).doubleValue();
			double density = numPts/(area / 1000000.0);
			Feature ft = new BasicFeature(fsDens);
			ft.setGeometry(geom);
			ft.setAttribute("pts", new Double(numPts));
			ft.setAttribute("area", new Double(area));
			ft.setAttribute("density", new Double(density));
			hullsDensity.add(ft);
		}
    	if(context != null){
    		FeatureCollection fcSingleHulls = new FeatureDataset(fsDens);
    		fcSingleHulls.addAll(hullsDensity);
    		context.addLayer(StandardCategoryNames.RESULT, "single hulls", fcSingleHulls);
    	}
    	//-- get the 100 percent isopleth/hull
    	if(monitor != null){
    		monitor.report("calculating 100% hull");
    	}
    	ArrayList<Feature> hullsF = LoCoHUtil.getIsoplethsByDensity(hullsDensity, 100, radius, 
    			points.size(), tree, fs);
    	fcRes.addAll(hullsF);
    	//-- generate other isopleths
    	if(calcIsopleths){
	    	int percent = 10;
	    	while(percent < 100){ // we generate the 100% hull anyway
	        	if(monitor != null){
	        		monitor.report("calculating isopleth (%): " + percent);
	        	}
	        	//System.out.println("calculating isopleth (%): " + percent);
	        	ArrayList<Feature> hullsFPerc = LoCoHUtil.getIsoplethsByDensity(hullsDensity, percent, 
	        	radius, points.size(), tree, fs);
				percent = percent + 20; 
				fcRes.addAll(hullsFPerc);
			}
    	}
    	return fcRes;
    }
    
    /**
     * calculates the home range regions with the LoCoH-k method, i.e. calculation of local convex 
     * hulls for all k-1 neighbors.
     * @param points
     * @param k
     * @param tree
     * @param calcIsopleths
     * @param context
     * @param monitor
     * @return
     */
    public static FeatureCollection getLocohK(FeatureCollection points, int k, STRtree tree,
    		boolean calcIsopleths, PlugInContext context, TaskMonitor monitor){
    	FeatureSchema fs = new FeatureSchema();
    	fs.addAttribute("geometry", AttributeType.GEOMETRY);
    	fs.addAttribute("percent-cover-hull", AttributeType.DOUBLE);
    	fs.addAttribute("param", AttributeType.DOUBLE);
    	FeatureCollection fcRes = new FeatureDataset(fs);
    	if(k >= points.size()){
    		k = points.size() - 1;
    	}
    	//-- get an initial distance - it is the smallest distance 
    	// out of 5 random distances as we grow the search envelope anyway
    	double initialDist = LoCoHUtil.calcSmallestDistanceOfFirstFiveFeatures(points);
    	if(monitor != null){
    		monitor.report("creating hulls. Initial distance: " + initialDist);
    	}
    	//--
    	ArrayList<Geometry> hulls = new ArrayList<Geometry>();
    	//-- create the local hull for each point
    	for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature ptf = (Feature) iterator.next();
			//-- get the k-1 points plus the point itself
			ArrayList<Feature> hullPoints = LoCoHUtil.getKNearestPoints(ptf, tree, k, initialDist);
			//-- add the point itself
			hullPoints.add(ptf);
			//-- get the convex hull if there are at least 3 points (i.e. we should have a polygon)
			if(hullPoints.size() >= 3){ 
				Geometry cHull = FeatureConversionUtils.createConvexHullFromFeatures(hullPoints);
				//-- store the number of points for each hull
				cHull.setUserData(new Double(hullPoints.size()));
				hulls.add(cHull);
			}
    	}
    	//-- calculate for each hull the density and create a Feature so we can use the featureSorting method
    	FeatureSchema fsDens = new FeatureSchema();
    	fsDens.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsDens.addAttribute("density", AttributeType.DOUBLE);
    	fsDens.addAttribute("pts", AttributeType.DOUBLE);
    	fsDens.addAttribute("area", AttributeType.DOUBLE);
    	ArrayList<Feature> hullsDensity = new ArrayList<Feature>();
    	for (Iterator iterator = hulls.iterator(); iterator.hasNext();) {
			Geometry geom = (Geometry) iterator.next();
			double area = geom.getArea();
			double numPts = ((Double)geom.getUserData()).doubleValue();
			double density = numPts/(area / 1000000.0);
			Feature ft = new BasicFeature(fsDens);
			ft.setGeometry(geom);
			ft.setAttribute("pts", new Double(numPts));
			ft.setAttribute("area", new Double(area));
			ft.setAttribute("density", new Double(density));
			hullsDensity.add(ft);
		}
    	if(context != null){
    		FeatureCollection fcSingleHulls = new FeatureDataset(fsDens);
    		fcSingleHulls.addAll(hullsDensity);
    		context.addLayer(StandardCategoryNames.RESULT, "single hulls", fcSingleHulls);
    	}
    	//-- get the 100 percent isopleth/hull
    	if(monitor != null){
    		monitor.report("calculating 100% hull");
    	}
    	ArrayList<Feature> hullsF = LoCoHUtil.getIsoplethsByDensity(hullsDensity, 100, k, 
    			points.size(), tree, fs);
    	fcRes.addAll(hullsF);
    	//-- generate other isopleths
    	if(calcIsopleths){
	    	int percent = 10;
	    	while(percent < 100){ // we generate the 100% hull anyway
	        	if(monitor != null){
	        		monitor.report("calculating isopleth (%): " + percent);
	        	}
	        	//System.out.println("calculating isopleth (%): " + percent);
	        	ArrayList<Feature> hullsFPerc = LoCoHUtil.getIsoplethsByDensity(hullsDensity, percent, 
	        	k, points.size(), tree, fs);
				percent = percent + 20; 
				fcRes.addAll(hullsFPerc);
			}
    	}
    	return fcRes;
    }
    
    /**
     * calculates the home range regions with the LoCoH-alpha method, i.e. calculation of local convex 
     * hulls for all points of which the sum of the distances is smaller than alpha.
     * @param points
     * @param distAplha
     * @param tree
     * @param calcIsopleths
     * @param context
     * @param monitor
     * @return
     */
    public static FeatureCollection getLocohAlpha(FeatureCollection points, double distAplha, STRtree tree,
    		boolean calcIsopleths, PlugInContext context, TaskMonitor monitor){
    	FeatureSchema fs = new FeatureSchema();
    	fs.addAttribute("geometry", AttributeType.GEOMETRY);
    	fs.addAttribute("percent-cover-hull", AttributeType.DOUBLE);
    	fs.addAttribute("param", AttributeType.DOUBLE);
    	FeatureCollection fcRes = new FeatureDataset(fs);
    	//-- get an initial distance - it is the smallest distance 
    	// out of 5 random distances as we grow the search envelope anyway
    	double initialDist = LoCoHUtil.calcSmallestDistanceOfFirstFiveFeatures(points);
    	if(monitor != null){
    		monitor.report("creating hulls. Initial distance: " + initialDist);
    	}
    	//--
    	ArrayList<Geometry> hulls = new ArrayList<Geometry>();
    	//-- create the local hull for each point
    	for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature ptf = (Feature) iterator.next();
			//-- get the k-1 points plus the point itself
			ArrayList<Feature> hullPoints = LoCoHUtil.getAlphaNearestPoints(ptf, tree, distAplha, initialDist);
			//-- add the point itself
			hullPoints.add(ptf);
			//-- get the convex hull if there are at least 3 points (i.e. we should have a polygon)
			if(hullPoints.size() >= 3){ 
				Geometry cHull = FeatureConversionUtils.createConvexHullFromFeatures(hullPoints);
				//-- store the number of points for each hull
				cHull.setUserData(new Double(hullPoints.size()));
				hulls.add(cHull);
			}
    	}
    	//-- calculate for each hull the density and create a Feature so we can use the featureSorting method
    	FeatureSchema fsDens = new FeatureSchema();
    	fsDens.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsDens.addAttribute("density", AttributeType.DOUBLE);
    	fsDens.addAttribute("pts", AttributeType.DOUBLE);
    	fsDens.addAttribute("area", AttributeType.DOUBLE);
    	ArrayList<Feature> hullsDensity = new ArrayList<Feature>();
    	for (Iterator iterator = hulls.iterator(); iterator.hasNext();) {
			Geometry geom = (Geometry) iterator.next();
			double area = geom.getArea();
			double numPts = ((Double)geom.getUserData()).doubleValue();
			double density = numPts/(area / 1000000.0);
			Feature ft = new BasicFeature(fsDens);
			ft.setGeometry(geom);
			ft.setAttribute("pts", new Double(numPts));
			ft.setAttribute("area", new Double(area));
			ft.setAttribute("density", new Double(density));
			hullsDensity.add(ft);
		}
    	if(context != null){
    		FeatureCollection fcSingleHulls = new FeatureDataset(fsDens);
    		fcSingleHulls.addAll(hullsDensity);
    		context.addLayer(StandardCategoryNames.RESULT, "single hulls", fcSingleHulls);
    	}
    	//-- get the 100 percent isopleth/hull
    	if(monitor != null){
    		monitor.report("calculating 100% hull");
    	}
    	ArrayList<Feature> hullsF = LoCoHUtil.getIsoplethsByDensity(hullsDensity, 100, distAplha, 
    			points.size(), tree, fs);
    	fcRes.addAll(hullsF);
    	//-- generate other isopleths
    	if(calcIsopleths){
	    	int percent = 10;
	    	while(percent < 100){ // we generate the 100% hull anyway
	        	if(monitor != null){
	        		monitor.report("calculating isopleth (%): " + percent);
	        	}
	        	//System.out.println("calculating isopleth (%): " + percent);
	        	ArrayList<Feature> hullsFPerc = LoCoHUtil.getIsoplethsByDensity(hullsDensity, percent, 
	        			distAplha, points.size(), tree, fs);
				percent = percent + 20; 
				fcRes.addAll(hullsFPerc);
			}
    	}
    	return fcRes;
    }
    
}
