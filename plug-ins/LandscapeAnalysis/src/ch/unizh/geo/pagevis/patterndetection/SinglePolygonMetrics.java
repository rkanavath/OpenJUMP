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
 * created:  		19.Jan.2009
 * last modified:   					
 * 					
 * 
 * @author sstein
 * 
 * description: Calculates several single polygon metrics.
 * 	
 *  
 *****************************************************/

package ch.unizh.geo.pagevis.patterndetection;

import java.util.ArrayList;
import java.util.Iterator;

import org.openjump.core.apitools.FeatureCollectionTools;

import ch.unizh.geo.measures.OrientationMBR;
import ch.unizh.geo.measures.PolygonShapeMeasures;
import ch.unizh.geo.measures.Squareness;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;

public class SinglePolygonMetrics {

    public static String CENTROID_EAST = "CEAS - x centroid east";
    public static String CENTROID_NORTH = "CNOR - y centroid north";       
    public static String CORNERS = "CORN - polygon corners";
    public static String HOLES = "HOLE - number of holes in polygon";    
    public static String AREA = "AREA - area"; 
    public static String PERIMETER = "PERI - perimeter";
    public static String FRACTALDIM = "FDIM - fractal dimension";
    public static String SHAPEINDEX = "SHPI - shape index";    
    public static String SHUMMSHAPE = "SSPI - Schumm's shape index";
    public static String CONCACVITY = "CNCV - concavity";    
    public static String COMPACTNESS = "CMPC - compactness (equals shape index)";
    public static String ELONGATION_MBR = "ELON - elongation of MBR";    
    public static String ORIENTATION_MBR = "OMBR - orientation of MBR";
    public static String ORIENTATION_STAT = "OAVG - average orientation of sides";
    public static String SQUARENESS = "CSQR - squareness of corners";  
    
    /**
     * Calculates a set of metrics for a set of individual polygons. The calculated values 
     * are attached as attributes to the feature collection. The method utilizes 
     * SinglePolygonMetrics.calcSingleMetric(), hence the calculation is more time consuming 
     * since certain help objects may be calculated multiple times for one geometry when several
     * metrics are requested. If the attribute name exists already it will be overwritten.
     * @param inputPolygons need to be single polygons, no multipolygons
     * @param singlePolyMetricNames
     * @param calcSingleMetrics set value corresponding to order in singlePolyMetricNames 
     * 			to true if metric should be calculated
     * @param monitor can be null, used for GUI feedback
     * @return
     */
    public static FeatureCollection characterizeSinglePolygons(FeatureCollection inputPolygons, ArrayList<String> singlePolyMetricNames, ArrayList<Boolean> calcSingleMetrics, TaskMonitor monitor){
    	//-- create new FeatureDataset
    	FeatureSchema oldFs = inputPolygons.getFeatureSchema();
    	FeatureSchema newFs = (FeatureSchema)oldFs.clone();
    	ArrayList<String> attrNames = new ArrayList<String>();
    	for (int i=0; i < calcSingleMetrics.size(); i++) {
			Boolean calc = (Boolean)calcSingleMetrics.get(i);
			if(calc){
				String name = singlePolyMetricNames.get(i);
				if(newFs.hasAttribute(name.substring(0, 4))== false){
					newFs.addAttribute(name.substring(0, 4), AttributeType.DOUBLE);
					attrNames.add(name.substring(0, 4));
				}
				else{
					//-- ensure that the type is correct
					//   otherwise add a new attribute with modified name
					AttributeType at = newFs.getAttributeType(name.substring(0, 4));
					if ((at.equals(AttributeType.DOUBLE) == false)){
						attrNames.add(name.substring(0, 4) + "new");
						newFs.addAttribute(name.substring(0, 4) + "new", AttributeType.DOUBLE);						
					}
					else{
						attrNames.add(name.substring(0, 4));
					}
				}				
			}
		}
    	FeatureDataset fd = new FeatureDataset(newFs);
    	int fcount = 0; int fmax = inputPolygons.size();
    	for (Iterator iterator = inputPolygons.iterator(); iterator.hasNext();) {
        	fcount++;
    		if(monitor != null){
        		monitor.report(fcount, fmax, "polygons processed with metrics");
        	}
			Feature f = (Feature) iterator.next();
			Feature fNew = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(f, newFs);
			//-- calculate values
			int j=0;
	    	for (int i=0; i < calcSingleMetrics.size(); i++) {
				Boolean calc = (Boolean)calcSingleMetrics.get(i);
				if(calc){
					String name = singlePolyMetricNames.get(i);
					double val = SinglePolygonMetrics.calcSingleMetric(name, fNew.getGeometry());
					String attName = attrNames.get(j);					
					fNew.setAttribute(attName, val);
					j++;
				}
			}			
			fd.add(fNew);
		}
    	return fd;
    }
    
    /**
     * Calculates the metric value for single polygon.
     * for refs on metrics calculation see also Steiniger et al. 2008, TGIS 
     * @param metricName : taking a static string provided by this class
     * @param geom : needs to be a (simple) polygon 
     * @return NaN if metric does not exist or geometry is not of type polygon
     */
    public static double calcSingleMetric(String metricName, Geometry geom){
    	double retVal = Double.NaN;
    	if (geom instanceof Polygon){
            // CENTROID_EAST = "CEAS - x centroid east";
        	if (metricName.equals(SinglePolygonMetrics.CENTROID_EAST)){
        		retVal = geom.getCentroid().getX();
        	}
            // CENTROID_NORTH = "CNOR - y centroid north";
        	if (metricName.equals(SinglePolygonMetrics.CENTROID_NORTH)){
        		retVal = geom.getCentroid().getY();
        	}    	
            // VERTICES = "VERT - vertices";
        	// a four corner building should have 4 vertices 
        	if (metricName.equals(SinglePolygonMetrics.CORNERS)){
        		Polygon p = (Polygon)geom;
        		retVal = p.getExteriorRing().getNumPoints() - 1;
        	}    	
            // HOLES = "HOLE - number of holes in polygon";
        	if (metricName.equals(SinglePolygonMetrics.HOLES)){
        		Polygon p = (Polygon)geom;
        		retVal = p.getNumInteriorRing();
        	}    	        	
            // AREA = "AREA - area";
        	if (metricName.equals(SinglePolygonMetrics.AREA)){
        		retVal = geom.getArea();
        	}    	        	
            // PERIMETER = "PERI - perimeter";
        	if (metricName.equals(SinglePolygonMetrics.PERIMETER)){
        		Polygon p = (Polygon)geom;
        		retVal = p.getLength();
        	}    	        	        	
            // FRACTALDIM = "FDIM - fractal dimension";
        	if (metricName.equals(SinglePolygonMetrics.FRACTALDIM)){
        		Polygon p = (Polygon)geom;
        		double area = p.getArea();
        		double peri = p.getLength();
        		retVal = PolygonShapeMeasures.calcFractalDim(peri, area);
        	}    	        	        	        	
            // SHAPEINDEX = "SHPI - shape index";
        	if (metricName.equals(SinglePolygonMetrics.SHAPEINDEX)){
        		Polygon p = (Polygon)geom;
        		double area = p.getArea();
        		double peri = p.getLength();
        		retVal = PolygonShapeMeasures.calcShapeIndex(peri, area);
        	}    	        	        	        	        	
            // SHUMMSHAPE = "SSPI - Schumm's shape index"; (MacEachren 1985, Geogr. Annaler)
        	if (metricName.equals(SinglePolygonMetrics.SHUMMSHAPE)){
        		Polygon p = (Polygon)geom;
        		double area = p.getArea();
        	    OrientationMBR myMbr = new OrientationMBR(geom);
        		double longestAxis = myMbr.getMbrLength();
        		retVal = PolygonShapeMeasures.calcSchummShapeIndex(longestAxis, area);
        	}    	        	        	        	        	
            // CONCACVITY = "CNCV - concavity";
        	if (metricName.equals(SinglePolygonMetrics.CONCACVITY)){
        		Polygon p = (Polygon)geom;
        		double area = p.getArea();
        		Geometry chull = geom.convexHull();
        		double convexHullArea = chull.getArea();
        		retVal = PolygonShapeMeasures.calcBuildingConcavity(area, convexHullArea);
        	}
            // COMPACTNESS = "CMPC - compactness (equals shape index)";
        	if (metricName.equals(SinglePolygonMetrics.COMPACTNESS)){
        		Polygon p = (Polygon)geom;
        		double area = p.getArea();
        		double peri = p.getLength();
        		retVal = PolygonShapeMeasures.calcCompactness(peri, area);
        	}        	
            // ELONGATION_MBR = "ELON - elongation of MBR"; (Bard 2004, TGIS)
        	if (metricName.equals(SinglePolygonMetrics.ELONGATION_MBR)){
        		Polygon p = (Polygon)geom;
        	    OrientationMBR myMbr = new OrientationMBR(geom);
        	    double width = myMbr.getMbrWidth();
        	    double length = myMbr.getMbrLength();	 
        	    retVal = PolygonShapeMeasures.calcBuildingElongation(width, length);
        	}        	
            // ORIENTATION_MBR = "OMBR - orientation of MBR"; (Duchene et al. 2003, ICA Gen. Workshop)
        	if (metricName.equals(SinglePolygonMetrics.ORIENTATION_MBR)){
        		Polygon p = (Polygon)geom;
        	    OrientationMBR myMbr = new OrientationMBR(geom);
        	    retVal = myMbr.getMbrOrientation();
        	}        	        	
            // ORIENTATION_STAT = "OAVG - average orientation of sides"; (Duchene et al. 2003, ICA Gen. Workshop)
        	if (metricName.equals(SinglePolygonMetrics.ORIENTATION_STAT)){
        		Polygon p = (Polygon)geom;
        	    OrientationMBR myMbr = new OrientationMBR(geom);
        	    retVal = myMbr.getStatOrientation();
        	}        	        	        	
            // SQUARENESS = "SSQR - squareness of sides"; Bader (1999)
        	if (metricName.equals(SinglePolygonMetrics.SQUARENESS)){
        		Polygon p = (Polygon)geom;
        	    Squareness sqrn = new Squareness(p);
        	    retVal = sqrn.getSquareness();
        	}        	        	        	        	
    	}
    	return retVal;
    }
}
