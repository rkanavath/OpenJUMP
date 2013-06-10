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
import ch.unizh.geo.measures.PolygonAreaDensity;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;

public class MultiPolygonMetrics {

    public static String NUM_POLYS = "NBPL - Number of Polygons in Buffer";
    public static String CONVEXHULL_DENSITY = "CHAD - Convex Hull area density";       
    public static String BUFFERHULL_DENSITY = "BHAD - Buffer Hull area density";
    public static String R_INDEX = "RIDX - R-Index";
    
    /**
     * Calculates a set of metrics for a set of individual polygons. The calculated values 
     * are attached as attributes to the feature collection. The method utilizes 
     * SinglePolygonMetrics.calcSingleMetric(), hence the calculation is more time consuming 
     * since certain help objects may be calculated multiple times for one geometry when several
     * metrics are requested. If the attribute name exists already it will be overwritten.
     * @param inputPolygons need to be single polygons, no multi-polygons
     * @param singlePolyMetricNames
     * @param calcDensityMetrics set value corresponding to order in singlePolyMetricNames 
     * 			to true if metric should be calculated
     * @param bufferValue
     * @param bufferType : 1-absolute, 2-relational (MBR-Length-based)      
     * @param bufferGeoms : if true feature geometries are replaced by buffer geometries
     * @param monitor can be null, used for GUI feedback
     * @return
     */
    public static FeatureCollection characterizeSinglePolygons(FeatureCollection inputPolygons, 
    		ArrayList<String> singlePolyMetricNames, ArrayList<Boolean> calcDensityMetrics, 
    		double bufferValue, int bufferType, boolean bufferGeoms, TaskMonitor monitor){
    	//-- create new FeatureDataset
    	FeatureSchema oldFs = inputPolygons.getFeatureSchema();
    	FeatureSchema newFs = (FeatureSchema)oldFs.clone();
    	ArrayList<String> attrNames = new ArrayList<String>();
    	for (int i=0; i < calcDensityMetrics.size(); i++) {
			Boolean calc = (Boolean)calcDensityMetrics.get(i);
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
    	//-- create qtree
    	Quadtree qTree = new Quadtree();
    	for (Iterator iterator = inputPolygons.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			qTree.insert(f.getGeometry().getEnvelopeInternal(), f.getGeometry());
		}
    	//-- calculate
    	FeatureDataset fd = new FeatureDataset(newFs);
    	int fcount = 0; int fmax = inputPolygons.size();
    	for (Iterator iterator = inputPolygons.iterator(); iterator.hasNext();) {
        	fcount++;
    		if(monitor != null){
        		monitor.report(fcount, fmax, "polygons processed with NH metrics");
        	}
			Feature f = (Feature) iterator.next();
			Feature fNew = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(f, newFs);
			double bufferValueFinal = 0;
			if (bufferType == 2){
				Geometry geom = fNew.getGeometry();
				if(geom instanceof Polygon){
					OrientationMBR myMBR = new OrientationMBR(geom); 
					bufferValueFinal = myMBR.getMbrLength() * bufferValue;
				}
			}			
			else{
				bufferValueFinal = bufferValue;
			}
			//-- calculate values
			int j = 0;
	    	for (int i=0; i < calcDensityMetrics.size(); i++) {
				Boolean calc = (Boolean)calcDensityMetrics.get(i);
				if(calc){
					String name = singlePolyMetricNames.get(i);
					double val = MultiPolygonMetrics.calcNeighborhoodMetric(name, fNew.getGeometry(), bufferValueFinal, qTree);
					String attName = attrNames.get(j);					
					fNew.setAttribute(attName, val);
					j++;
				}
			}
			if(bufferGeoms){
				Geometry buffer = fNew.getGeometry().buffer(bufferValueFinal);
				fNew.setGeometry(buffer);
			}
			fd.add(fNew);
		}
    	return fd;
    }
    
    /**
     * Calculates the metric value for a polygon neighborhood.
     * for refs on metrics calculation see Steiniger et al. 2008, TGIS 
     * @param metricName : taking a static string provided by this class
     * @param geom : needs to be a (simple) polygon
     * @param bufferValue : metric value 
     * @param qTree : the polygons to query in a quadtree index structure
     * @return NaN if metric does not exist or geometry is not of type polygon
     */
    public static double calcNeighborhoodMetric(String metricName, Geometry geom, double bufferValue, Quadtree qTree){
    	double retVal = Double.NaN;
    	if (geom instanceof Polygon){
            // NBPL - Number of Polygons in Buffer";
        	if (metricName.equals(MultiPolygonMetrics.NUM_POLYS)){
        		PolygonAreaDensity pad = new PolygonAreaDensity(geom, qTree, bufferValue, 8); 
        		retVal = pad.getNoOfObjects();
        	}
        	if (metricName.equals(MultiPolygonMetrics.CONVEXHULL_DENSITY)){
        		PolygonAreaDensity pad = new PolygonAreaDensity(geom, qTree, bufferValue, 8); 
        		retVal = pad.getPolyAreaHullDensity();
        	}               	        	
        	if (metricName.equals(MultiPolygonMetrics.BUFFERHULL_DENSITY)){
        		PolygonAreaDensity pad = new PolygonAreaDensity(geom, qTree, bufferValue, 8); 
        		retVal = pad.getPolyAreaBufferDensity();
        	}
        	if (metricName.equals(MultiPolygonMetrics.R_INDEX)){
        		PolygonAreaDensity pad = new PolygonAreaDensity(geom, qTree, bufferValue, 8); 
        		retVal = pad.getRIndex();
        	}        	
    	}
    	return retVal;
    }
}
