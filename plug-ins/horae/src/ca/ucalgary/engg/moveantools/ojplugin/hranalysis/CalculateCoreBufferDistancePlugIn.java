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
 * created:  		10.June.2010
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JMenuItem;

import org.math.array.StatisticSample;
import org.openjump.core.ui.plugin.AbstractThreadedUiPlugIn;

import ca.ucalgary.engg.moveantools.util.geom.PointLineDistance;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.distance.DistanceOp;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDatasetFactory;
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
 * @description: Calculates the distance between HR core and HR edge
 *	
 * @author sstein
 *
 **/
public class CalculateCoreBufferDistancePlugIn extends AbstractThreadedUiPlugIn{

    private String sSidebar ="Calculates the average buffer distance for the home range polygons that should be classified later, based on the core areas";   
    private final String sLAYERHR = "Layer with Home Range(s) to classify";
    private final String sLAYERCORE = "Layer with Core Area";
    private final String sOutputLines = "display measurement lines";
    
    ArrayList<Geometry> geoms = null;
    boolean displayLines = false;
    private FeatureCollection hrPolyFC = null;  
    private FeatureCollection corePolyFC = null;   
    private Layer inputHR = null;
    private Layer inputCore = null;
    private MultiInputDialog dialog;
    private PlugInContext context = null;
        
    public void initialize(PlugInContext context) throws Exception {
    				
        	context.getFeatureInstaller().addMainMenuItem(
                    new String[] {"MOVEAN", "HR Analysis"}, 	//menu path
                    this,
                    new JMenuItem("Calculate Buffer Distance for HR Classification...", null),
                    createEnableCheck(context.getWorkbenchContext()), -1); 
    }

    public static MultiEnableCheck createEnableCheck(WorkbenchContext workbenchContext) {
        EnableCheckFactory checkFactory = new EnableCheckFactory(workbenchContext);

        return new MultiEnableCheck()
                        .add(checkFactory.createAtLeastNLayersMustExistCheck(2));
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
	    	this.geoms = new ArrayList<Geometry>();
	    	//-- put all objects from the HR layer in a tree
	    	STRtree geomTree = new STRtree();
	    	for (Iterator iterator = this.hrPolyFC.iterator(); iterator.hasNext();) {
				Feature ft = (Feature) iterator.next();
				geomTree.insert(ft.getGeometry().getEnvelopeInternal(), ft.getGeometry());
			}
	    	ArrayList<Double> randomDistances = new ArrayList<Double>();
	    	//-- query the parent for each item and calculate some distances
	    	for (Iterator iterator = this.corePolyFC.iterator(); iterator.hasNext();) {
				Feature ftemp = (Feature) iterator.next();
				Geometry coreGeom = ftemp.getGeometry();
				if(coreGeom instanceof Polygon){
					//everything is fine
				}
				else{
					context.getWorkbenchFrame().warnUser("core layer does not contain polygons (only)");
				}
				List candidates = geomTree.query(coreGeom.getEnvelopeInternal());
				if(candidates.size() == 0){
					context.getWorkbenchFrame().warnUser("no matching candidates found");
				}
				for (Iterator iterator2 = candidates.iterator(); iterator2.hasNext();) {
					Geometry gtemp = (Geometry) iterator2.next();
					if(gtemp instanceof Polygon){
						//everything is fine
					}
					else{
						context.getWorkbenchFrame().warnUser("home range layer does not contain polygons (only)");
					}
					if(coreGeom.intersects(gtemp)){
						/*
						//--calc some distances
						double[] someDistances = calculateSomeBoundaryDistances((Polygon)gtemp, (Polygon)coreGeom);						
						//for (int i = 0; i < someDistances.length; i++) {
						//	randomDistances.add(someDistances[i]);
						//}						
						//-- get the minDistance of those and add it
						double minDist = DoubleArray.min(someDistances);
						*/
						double minDist = calculateArbitraryPointBoundaryDistances((Polygon)gtemp, (Polygon)coreGeom, context, false);
						if (minDist>0){
							// we don't want zero distances
							randomDistances.add(minDist);
						}
					}
				}
			}
	    	//get the minimum distance
	    	double[] rndDists = new double[randomDistances.size()];
	    	int i=0;
	    	for (Iterator iterator = randomDistances.iterator(); iterator
					.hasNext();) {
				Double val = (Double) iterator.next();
				rndDists[i] = val;
				i++;
			}
			double meanDistFinal = StatisticSample.mean(rndDists);
	    	
			context.getWorkbenchFrame().getOutputFrame().createNewDocument();
			context.getWorkbenchFrame().getOutputFrame().addText("HR layer: " + inputHR.getName());
			context.getWorkbenchFrame().getOutputFrame().addText("Core layer: " + inputCore.getName());
			context.getWorkbenchFrame().getOutputFrame().addText("calculated average buffer distance for classification: " + meanDistFinal);
			//context.getWorkbenchFrame().getOutputFrame().surface();
			
			//-- do some output
			if((context != null) && (displayLines)){
				 FeatureCollection geomFC = FeatureDatasetFactory.createFromGeometry(geoms);
				 context.addLayer(StandardCategoryNames.SYSTEM, "point-distances", geomFC);
			}
	        System.gc();    		
    	}

	private void initDialog(PlugInContext context) {
    	
        dialog = new MultiInputDialog(context.getWorkbenchFrame(), "Calculate Buffer Distance", true);
        dialog.setSideBarDescription(sSidebar);
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERHR, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        try {
        	JComboBox addLayerComboBoxRegions = dialog.addLayerComboBox(this.sLAYERCORE, context.getCandidateLayer(0), null, context.getLayerManager());
        }
        catch (IndexOutOfBoundsException e) {}
        dialog.addCheckBox(sOutputLines, displayLines);
        GUIUtil.centreOnWindow(dialog);
    }
    
    private void getDialogValues(MultiInputDialog dialog) {
    	this.inputHR =  dialog.getLayer(this.sLAYERHR);
    	this.inputCore = dialog.getLayer(this.sLAYERCORE);
    	this.hrPolyFC= this.inputHR.getFeatureCollectionWrapper(); 
    	this.corePolyFC = this.inputCore.getFeatureCollectionWrapper();
    	this.displayLines = dialog.getBoolean(sOutputLines);
      }

    /**
     * 
     * @param hrGeom
     * @param coreGeom
     * @param context can be null
     * @return
     */
	private double calculateArbitraryPointBoundaryDistances(Polygon hrGeom, Polygon coreGeom, PlugInContext context, boolean displayDists) {
		//-- get the outlines
		Geometry hrBoundary = hrGeom.getBoundary();
		Geometry coreBoundary = coreGeom.getBoundary();
		//-- get an arbitrary point in the core polygon (i.e. something 
		//   like a centroid but inside)
		Point pt = coreGeom.getInteriorPoint();
		// get the shortest distance between those
		DistanceOp distHR = new DistanceOp(pt, hrBoundary);
		DistanceOp distCore = new DistanceOp(pt, coreBoundary);
		double diffDist = Math.abs(distHR.distance() - distCore.distance());
		//-- do some output
			 ArrayList<Geometry> geomss = new ArrayList();
			 geomss.add(pt);
			 GeometryFactory gf = new GeometryFactory();
			 //--
			 Coordinate[] ptsHR = distHR.closestPoints();
			 LineString lsHR = gf.createLineString(ptsHR); 
			 geomss.add(lsHR);
			 //--
			 Coordinate[] ptsCore = distCore.closestPoints();
			 LineString lsCore = gf.createLineString(ptsCore); 
			 geomss.add(lsCore);
			 //--
			 geoms.addAll(geomss);
			 //--
		if((context != null) && (displayDists)){
			 FeatureCollection geomsFC = FeatureDatasetFactory.createFromGeometry(geomss);
			 context.addLayer(StandardCategoryNames.SYSTEM, "point-distances", geomsFC);
		}
		return diffDist;
	}
	
    /**
     * this was some first idea, but I think another option is to get an arbitrary point
     * in the core polygon and calculate the min-distance to the boundaries and get the
     * difference 
     * @param hrGeom
     * @param coreGeom
     * @return
     */
	private double[] calculateSomeBoundaryDistances(Polygon hrGeom, Polygon coreGeom) {
		double[] dists = new double[10];
		// explore the dimensions by calculating the distance between the centroid
		// of the core object and the boundary of the HR polygon
		Geometry hrBoundary = hrGeom.getBoundary();
		Geometry coreBoundary = coreGeom.getBoundary();
		Geometry coreCentroid = coreBoundary.getCentroid(); 
		double coreDistance =  coreCentroid.distance(hrBoundary);
		
		Coordinate[] bcoords = coreBoundary.getCoordinates();
		int numVertices = bcoords.length;				
		for (int i = 0; i < dists.length; i++) {
			//-- get a random vertex on the boundary of the coreGeom
			int rndIdx = (int)Math.floor(Math.random() * numVertices);
			Coordinate pC = bcoords[rndIdx];
			// create a line from the vertex before and after			
			int indexBefore = rndIdx - 1;
			if(rndIdx == 0){
				indexBefore =  numVertices - 2; //-2 because first [0] and last [n-1] coord should be the same
			}
			Coordinate a = bcoords[indexBefore];
			int indexAfter = rndIdx + 1;
			if(rndIdx == numVertices-1){
				indexAfter =  1;
			}
			Coordinate b = bcoords[indexAfter];
			// get the shortest distance point between the random vertex and that line
			PointLineDistance pdistA = new PointLineDistance(pC, a, b);
			Point p = pdistA.getClosestPoint();
			// extend this shortest distance line until it hits the 
			// hrGeom / create the intersection point
				//TODO
			// get the distance between the intersection point and the current point  
				//TODO
		}
		return dists;
	}
	
}
