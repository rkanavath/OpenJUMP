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
/***********************************************
 * created on 		15.03.2005
 * last modified: 	08.01.2006 (R Index)
 * 
 * author:			sstein
 * 
 * description:
 * calculates for a buffer around a polygon <p>
 * 	- the nummer of polygons in a buffer <p>
 *  - the density (area of polys overlaped by buffer)/(area of convex hull around polys)<p>
 *  - the density (area of polys in buffer) / (area of buffer);
 *  - the R-nearest neighbour Index according to pinder and witherick (1973) * 
 * 
 ***********************************************/
package ch.unizh.geo.measures;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import ch.unizh.geo.geomutilities.SecondGeodeticTask2d;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.quadtree.Quadtree;


/**
 * @author sstein
 *
 * @description:
 * calculates for a buffer around a polygon <p>
 * 	- the nummer of polygons in a buffer <p>
 *  - the density (area of polys overlaped by buffer)/(area of convex hull around polys)<p>
 *  - the density (area of polys in buffer) / (area of buffer);
 *  - the R-nearest neighbour Index according to pinder and witherick (1973)
 */
public class PolygonAreaDensity{
    
    private int quadCircleSegments = 8; //deafult by jts is 8 segments
    private double bRadius = 50.0;
    private Geometry geom = null;
    private Quadtree geomTree = null;
    
    private int noOfObjects = 0;
    private double polyAreaHullDensity = 0;
    private double polyAreaBufferDensity = 0;  
    private double rIndex = 0;
    private List intersections = new ArrayList();
   	private List objectsInBuffer = new ArrayList();
   	private Geometry convexHull = null;
   	private Geometry buffer = null;
   	
   	/**
   	 * 
   	 * @param polygon
   	 * @param geometriesToSearch
   	 * @param bufferRadius (in metres)
   	 * @param bufQuadCircleSegments (standard in jts is 8 segments)
   	 */
    public PolygonAreaDensity(Geometry polygon, ArrayList geometriesToSearch, 
            		double bufferRadius, int bufQuadCircleSegments){
        this.bRadius = bufferRadius;
        this.quadCircleSegments = bufQuadCircleSegments;
        this.geom = polygon;
        this.geomTree = new Quadtree();
       	for (Iterator iter = geometriesToSearch.iterator(); iter.hasNext();) {
            Geometry buildingX = (Geometry) iter.next();
            this.geomTree.insert(buildingX.getEnvelopeInternal(), buildingX);
       	}
       	
       	this.calculate();
    }

    /**
     * 
     * @param polygon
     * @param geometryQtree (made of jts geometry objects)
     * @param bufferRadius (in metres)
     * @param bufQuadCircleSegments (standard in jts is 8 segments) 
     */
    public PolygonAreaDensity(Geometry polygon, Quadtree geometryQtree, 
    		double bufferRadius, int bufQuadCircleSegments){
        this.bRadius = bufferRadius;
        this.quadCircleSegments = bufQuadCircleSegments;
        this.geom = polygon;
        this.geomTree = geometryQtree;
        
        this.calculate();
    }
        
    private void calculate(){
        this.buffer = this.geom.buffer(this.bRadius, this.quadCircleSegments);
        //-- rough check using the tree
        List candidates = this.geomTree.query(this.buffer.getEnvelopeInternal());
        //-- detailed check:       
        double areaIntersectionSum = 0;
        double areaBuildSum = 0;
        int counter = 0;
       	for (Iterator iter = candidates.iterator(); iter.hasNext();) {
            Geometry candidate = (Geometry) iter.next();
            Geometry intersection = this.buffer.intersection(candidate);
            if(intersection.getArea() > 0){                  
                this.objectsInBuffer.add(candidate);
                this.intersections.add(intersection);
                areaIntersectionSum += intersection.getArea();
                areaBuildSum += candidate.getArea();
                counter++;
            }
        } 
        this.noOfObjects = counter;

       	//-- create multi-polygon for convex hull      	
       	ArrayList<Polygon> polys = new ArrayList<Polygon>();
       	int j = 0;
       	Geometry uniong = null;
       	for (Iterator iter = objectsInBuffer.iterator(); iter.hasNext();) {
       		Geometry g = (Geometry)iter.next();
       		if(j == 0){
       			uniong = g;
       		}
       		else{
       			uniong = uniong.union(g);
       		}
        }
       	this.convexHull = uniong.convexHull();
        //calculate ratios;       	
       	double areaHull = this.convexHull.getArea(); 
       	this.polyAreaHullDensity = areaBuildSum/areaHull;
       	this.polyAreaBufferDensity = areaIntersectionSum/buffer.getArea();
       	//calc R-Index
        //Geometry circle = this.geom.getCentroid().buffer(this.bRadius, this.quadCircleSegments);
       	//this.rIndex = this.calcRIndex(this.geom, this.objectsInBuffer, circle); 
       	this.rIndex = this.calcRIndex(this.objectsInBuffer, this.buffer);
    }
    
    /**
     * R-index according to pinder and witherick (1973) in V. Mesev (2005)
     * @param objectsInBuffer
     * @param buffer (attention: buffer must not be a circle, but it could be better)
     * @return
     */
    private double calcRIndex(List objectsInBuffer, Geometry buffer){
        double rvalue = 0;
        double sumLength = 0;
        int count = 0;
        //ArrayList dists = new ArrayList();
        if(objectsInBuffer.size() > 1){
	        for (int i = 0; i < objectsInBuffer.size(); i++) {
	            count++;
	            Geometry g1 = (Geometry)objectsInBuffer.get(i);
	            Point centroid = g1.getCentroid();            
	            //-- init Dist
	            Geometry element = null;
	            if (i==0){
	                element = (Geometry)objectsInBuffer.get(1);
	            }
	            else{
	                element = (Geometry)objectsInBuffer.get(0);
	            }
	            double minDist = SecondGeodeticTask2d.calcDistancePoints(centroid, element.getCentroid());
	            //-- search minDist
	            for (int j = 1; j < objectsInBuffer.size(); j++) {
	                element = (Geometry)objectsInBuffer.get(j);
	                if(i != j){//avoid self-checks (zero dists)
	                    double dist = SecondGeodeticTask2d.calcDistancePoints(centroid, element.getCentroid());
	                    if (dist < minDist){
		                    minDist = dist;
		                }
	                }
	            }            
	            sumLength = sumLength + minDist;
	        }      
	        double eumerator = 0; double deno=0;
	
	        if (count > 1){
                eumerator = sumLength/(count-1); //mean(smallest-dist)
	            deno = 0.5*1.0/Math.sqrt((count-1)/buffer.getArea());
	            rvalue = eumerator/deno;
		        }
    	}
        return rvalue;
    }

    /********************** getters and setters ****************/
    /**
     * 
     * @return numer of objects in buffer
     */
    public int getNoOfObjects() {
        return noOfObjects;
    }
    /**
     * 
     * @return geometrys of objects in buffer
     */
    public List getObjectsInBuffer() {
        return objectsInBuffer;
    }
    /**
     * 
     * @return ratio: (area of polys in buffer) / (area of buffer),
     *  it ranges from 0..1
     */
    public double getPolyAreaBufferDensity() {
        return polyAreaBufferDensity;
    }
    /**
     * this might be good to detect single rural buildings (ratio = 1)
     * @return ratio: (area of polys overlaped by buffer)/(area of convex hull around polys)
     */
    public double getPolyAreaHullDensity() {
        return polyAreaHullDensity;
    }
    /**
     * 
     * @return intersection geometrys of polgyons with buffer 
     */
    public int getQuadCircleSegments() {
        return quadCircleSegments;
    }
    
    public Geometry getBuffer() {
        return buffer;
    }
    public List getIntersections() {
        return intersections;
    }
    /**
     * 
     * @return R-Index : nearest neighbour index from Pinder and Witherick (1973)<p>
     * values for point distribution: 0 : clustered, 1: random, 2.15: uniformtity
     */
    public double getRIndex() {
        return rIndex;
    }
    public Geometry getConvexHull() {
        return convexHull;
    }
}
