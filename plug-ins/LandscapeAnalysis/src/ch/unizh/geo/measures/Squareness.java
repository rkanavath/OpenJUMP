/***********************************************
 * created on 		01.10.2004
 * last modified: 	04.10.2004
 * 					11.08.2006 added #convertAngleToRectangularDeviation()					
 * 
 * author:			sstein
 * 
 * description:
 *  calculates an index of squareness of a building/polygon
 *  using mean deviation;
 *  algorithm by UniED (eg. Alistair Edwards);
 *  output: 0: squared 1: not squared
 * 
 * using the RefractionAngles class to calculate the angles
 *  
 ***********************************************/
package ch.unizh.geo.measures;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 *
 * @description:
 *  calculates Squareness of the angles of a polygon or LineString
 *  using mean deviation;
 *  formula given by A. Edwards (ZRH-meeting 1998 or S. Airault(1996);
 *  output: 0: squared 1: not squared,
 *  
 * using the RefractionAngles class to calculate the angles
 * 
 * @author sstein 
 */
public class Squareness {

    private double squareness = 0;
    
    /**
     * 
     * @param myGeometry is LineString or Polygon
     */
    public Squareness (Geometry myGeometry){        
        
        if(myGeometry instanceof Polygon){
            Polygon myPoly = (Polygon)myGeometry;
            LineString exteriorRing = myPoly.getExteriorRing();
            this.calcSquareness(exteriorRing);
        }
        else if(myGeometry instanceof LineString){
            LineString myLine =(LineString)myGeometry; 
            this.calcSquareness(myLine);
        }
        else{
            //do something if point
        }                              
        
    }
    
    /**
     * calculate squareness using mean deviation from pi/2 
     * @param myLine
     */
    private void calcSquareness(LineString myLine){
        
        // calculate Angles using other measure function 
        RefractionAngles myAngles = new RefractionAngles(myLine);
        double[] angles = myAngles.getAngles();
        // test if ring - yes : calculate squarness using angles 1..n-1
        //				  no : calculate squarness using angles 1..n
        int lastElement = angles.length;
        if (angles[0] == angles[lastElement-1]){
             lastElement = lastElement-1;
        }
        double sum = 0;
        for(int i=0; i < lastElement; i++){
            // calculate rest = mod(angle-pi/2; pi/2)            
            double modResult = Math.floor( Math.abs(angles[i]-Math.PI/2)/ (Math.PI/2) );
            double rest = Math.abs(angles[i]-Math.PI/2) - modResult;
            sum = sum + rest;
        }
        double meanDev = sum/lastElement * (1/ (Math.PI/4));
        this.setSquareness(meanDev);
    }
    
    /**
     * squareness value for the exterior ring of polygon (or line)
     * @return double value between 0 and 1;
     * 		   0: is squared;
     * 		   1: is not squared
     */
    public double getSquareness() {
        return squareness;
    }
    private void setSquareness(double squareness) {
        this.squareness = squareness;
    }
    
    /**
     * calculates mod(angle; pi/2)
     * @param angleInRad
     * @return the deviation from 0..pi/4
     */
    public static double convertAngleToRectangularDeviation(double angleInRad){
        double newAngle = 0;
        double modResult = Math.floor(Math.abs(angleInRad)/ (Math.PI/2.0) );
        double rest = Math.abs(angleInRad) - (modResult*Math.PI/2.0);
        if (rest > (Math.PI/4.0)){
            newAngle=Math.PI/2.0 - rest;
        }
        else{
            newAngle=rest;
        }
        return newAngle;
    }
}
