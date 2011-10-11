/***********************************************
 * created on 		23.08.2005
 * last modified: 	
 * 
 * author:			sstein
 * 
 * description:
 * 
 * 
 ***********************************************/
package mapgen.geomutilities;

import java.util.ArrayList;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author sstein
 *
 */
public class InterpolateLinePoints{

    private ArrayList orgPtPositions = new ArrayList();
    private LineString inputLine = null;
    private LineString outLine = null;
    
    /**
     * Adds points to a given line whereby the point distance has to be specified.
     * The original points are kept. 
     * @param line the line to which the points should be added
     * @param distance the distance between the vertices which should be added
     */
    public InterpolateLinePoints(LineString line, double distance){
        this.inputLine = (LineString)line.clone();
        Coordinate[] pts = line.getCoordinates();
        ArrayList newCoords = new ArrayList();
        
	    int n= pts.length; int ptsCount = 0; double distsum = 0;
	    int pointsAdded = 0; double prevDistSum = 0;
   		for(int i=0; i < n-1; i++){   		    
   			double dist = SecondGeodeticTask2d.calcDistanceCoord(pts[i],pts[i+1]);   			
   			double angle = SecondGeodeticTask2d.calcAngle2Coords(pts[i],pts[i+1]);
   			//-- full distance
   			prevDistSum = distsum;
   			distsum = distsum + dist;
   			//-- add starting point
   			newCoords.add(pts[i]);
   			this.orgPtPositions.add(new Integer(ptsCount));
   			ptsCount++;
   			//-- calc no of points to add for segement
   			int allPtsToAdd = (int)Math.ceil(distsum/distance);
   			int ptsToAdd = allPtsToAdd-pointsAdded-1; //subtract one 
   			//-- calc firstDist   			
   			double firstDist = ((pointsAdded+1)*distance)-prevDistSum;
			//-- add interpolated points
   			double ptDist = 0;
   			for (int j = 0; j < ptsToAdd; j++) {
   			    ptDist=firstDist+ j * distance;
   	   			Coordinate middlePt = FirstGeodeticTask2d.getCoordinate(pts[i],angle,ptDist);
   	   			newCoords.add(middlePt);
   	   			pointsAdded = pointsAdded +1 ;
   	   			ptsCount++;                
            }			
   		}
        //-- add last point
   		newCoords.add(pts[n-1]);
   		this.orgPtPositions.add(new Integer(ptsCount));
   		//-- create LineString
        Coordinate[] coords = new Coordinate[newCoords.size()];
        for (int i = 0; i < newCoords.size(); i++) {
            Coordinate pt = (Coordinate)newCoords.get(i);
            coords[i] = pt;
        }
        this.outLine = new GeometryFactory().createLineString(coords);
    }

    public InterpolateLinePoints(LineString line, int noPoints){
        noPoints = noPoints+1; //add one to get the correct number
        this.inputLine = (LineString)line.clone();
        Coordinate[] pts = line.getCoordinates();
        ArrayList newCoords = new ArrayList();
        
	    int n= pts.length; int ptsCount = 0; double distsum = 0;
	    int pointsAdded = 0; double prevDistSum = 0;
   		for(int i=0; i < n-1; i++){   		    
   			double dist = SecondGeodeticTask2d.calcDistanceCoord(pts[i],pts[i+1]);   			
   			double angle = SecondGeodeticTask2d.calcAngle2Coords(pts[i],pts[i+1]);
   			//-- add starting point
   			newCoords.add(pts[i]);
   			this.orgPtPositions.add(new Integer(ptsCount));
   			ptsCount++;
   			//-- calc no of points to add for segement
   			double interpolDistance = dist/noPoints;
			//-- add interpolated points   			
   			for (int j = 1; j < noPoints; j++) {
   	   			Coordinate middlePt = FirstGeodeticTask2d.getCoordinate(pts[i],angle,j*interpolDistance);
   	   			newCoords.add(middlePt);
   	   			pointsAdded = pointsAdded +1 ;
   	   			ptsCount++;                
            }			
   		}
        //-- add last point
   		newCoords.add(pts[n-1]);
   		this.orgPtPositions.add(new Integer(ptsCount));
   		//-- create LineString
        Coordinate[] coords = new Coordinate[newCoords.size()];
        for (int i = 0; i < newCoords.size(); i++) {
            Coordinate pt = (Coordinate)newCoords.get(i);
            coords[i] = pt;
        }
        this.outLine = new GeometryFactory().createLineString(coords);        
    }

    
	/**
	 * calculates the middle points between existing vertices of a line
	 * and returns a new linestring containing that points. <p>
	 * The number of new vertices is 2*n-1. 
	 * @param line
	 * @return
	 */
	public static LineString addMiddlePoints(LineString line){
   		Coordinate[] pts = line.getCoordinates();
	    int n= pts.length;
	    int n2= n*2-1;
	    Coordinate[] newCoords = new Coordinate[n2];
	    int count = 0;
   		for(int i=0; i < n-1; i++){
   			double dist = SecondGeodeticTask2d.calcDistanceCoord(pts[i],pts[i+1]);
   			double angle = SecondGeodeticTask2d.calcAngle2Coords(pts[i],pts[i+1]);
   			//-- add starting point
   			newCoords[count]=pts[i];
   			count++;
			//-- calc new middle point
   			Coordinate middlePt = FirstGeodeticTask2d.getCoordinate(pts[i],angle,0.5*dist);
			newCoords[count]=middlePt;
			count++;
   		}
   		//-- add last coordinate
   		newCoords[n2-1] = pts[n-1]; 
	    LineString newLine = new GeometryFactory().createLineString(newCoords);
	    
	    return newLine;
	}
	    
    public LineString getInputLine() {
        return inputLine;
    }
    
    /**
     * 
     * @return line with the introduced/interpolated points
     */
    public LineString getOutLine() {
        return outLine;
    }
    
    /**
     * 
     * @return an integer array containing the positions of the 
     *         original input points 
     */
    public int[] getOrgPtPositions() {
        int[] positions = new int[this.orgPtPositions.size()];
        for (int i = 0; i < this.orgPtPositions.size(); i++) {
            Integer pos = (Integer)this.orgPtPositions.get(i);
            positions[i] = pos.intValue();
        }
        return positions;
    }
}
