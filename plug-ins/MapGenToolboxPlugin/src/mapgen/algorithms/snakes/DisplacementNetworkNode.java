/***********************************************
 * created on 		20.08.2005
 * last modified: 						
 * 
 * author:			sstein
 * 
 * description:
 *  contains the points of different lines which form a node in the network   
 ***********************************************/

package mapgen.algorithms.snakes;

import java.util.ArrayList;

import mapgen.geomutilities.FirstGeodeticTask2d;
import mapgen.geomutilities.SecondGeodeticTask2d;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.LineString;


/**
 * @description
 *  contains the points of different lines which form a node in the network <p>
 *  pointList is an ArrayList built up of JTS point geometries<p>
 *  lineIdForPointList is an ArrayList containing the corresponding LineID as Integer Object<p>  
 * @author sstein
 *	
 */
public class DisplacementNetworkNode {
		
	private ArrayList averageNodePoints = new ArrayList(); 
	private ArrayList pointList = new ArrayList();
	public ArrayList linePosList = new ArrayList();
	public ArrayList lineIdForPoints = new ArrayList();
 
	/**
	 * contains the points of different lines which form a node in the network. <p>
	 * Constructor initializes to empty Lists: pointList and lineIdForPointList
	 */
	public DisplacementNetworkNode(){
		
	}
	
	/**
	 * @return Returns the averageCoord.
	 */
	public Point calcAveragePoint() {
		Point pt = null;
		double x=0;
		double y=0;
		for(int i=0; i < pointList.size(); i++){
			Point ptTemp = (Point)pointList.get(i);
			x = x + ptTemp.getX();
			y = y + ptTemp.getY();
		}
		double newx=x/pointList.size();
		double newy=y/pointList.size();
		Coordinate coord = new Coordinate(newx,newy); 
		pt = new GeometryFactory().createPoint(coord);
		return pt;
	}
	
	/**
	 * calls calcAveragePoint() and sets the coordinates 
	 * of all points in pointList to the average coordinates
	 *
	 */
	public void setPointsToAverage(){
		Point pt= this.calcAveragePoint();
		for(int i=0; i < pointList.size(); i++){
			Point ptTemp = (Point)pointList.get(i);
			Coordinate coord = ptTemp.getCoordinate();
			coord.x = pt.getX();
			coord.y = pt.getY();
		}		
	}
	
	
	public void receivePointsFromLineIndizes(ArrayList lineToDisplaceList){
		/********
		 * attention we will only tranfer links .. not objects! so we can modify the points afterwards!
		 *******/
		//--clear if new coordinates are obtained
		this.pointList.clear();
		int n=this.lineIdForPoints.size();
		for(int i=0; i < n; i++){
			Integer id = (Integer)this.lineIdForPoints.get(i);
			LineToDisplace line = this.getPointFromLineToDispList(id.intValue(), lineToDisplaceList);
			if(line != null){
				Integer pos = (Integer)this.linePosList.get(i);
				if (pos.intValue()==1){
					LineString ls = (LineString)line.getTempGeometry();
					Point pt = ls.getStartPoint();
					this.pointList.add(pt);
				}
				else if(pos.intValue()==2){
					LineString ls = (LineString)line.getTempGeometry();
					Point pt = ls.getEndPoint();
					this.pointList.add(pt);				
				}
				else{
					System.out.println("DisplacementNetworkNode.reveicePointsFrom..: unknown position value");
				}
			}
		}
		Point avpt = this.calcAveragePoint();
		this.averageNodePoints.add(avpt);
	}
	
	private LineToDisplace getPointFromLineToDispList(int lineID, ArrayList lineToDisplaceList){
		/********
		 * attention we will only tranfer links .. not objects! so we can modify the original lines afterwards!
		 *******/
		LineToDisplace line = null;
		int n = lineToDisplaceList.size();
		boolean notFound = true; int i=0;
		while(notFound){
			LineToDisplace ltd = (LineToDisplace)lineToDisplaceList.get(i);		
			int fid=ltd.getFeatureID();			
			if(ltd.getFeatureID() == lineID){
				line = ltd;
				notFound = false;
			}
			if ((i == n-1)&&(notFound == true)){
				notFound = false;
				System.out.println("DisplacementNetworkNode.getPointFromLine..: unknown FID");
			}
			i++;
		}
		return line;
	}
	/**
	 * @return Returns the averageNodePoints.
	 * the list is filled every time #receivePointsFromLineIndizes()
	 * is proceed
	 */
	public ArrayList getAverageNodePoints() {
		return averageNodePoints;
	}
	
	public Point getOriginalAverageNode(){
		return (Point)this.averageNodePoints.get(0);
	}
	
	/**
	 * 
	 * @param maxDist
	 * @return true: new node position, because dist > maxDist 
	 */
	public boolean checkAndSetActualAverageNodeToMaxDist(double maxDist){
		boolean changed=false;
		int n=this.averageNodePoints.size();
		Point actPoint = (Point)this.averageNodePoints.get(n-1);
		Point orgPoint = this.getOriginalAverageNode();
		double dist = SecondGeodeticTask2d.calcDistancePoints(orgPoint,actPoint);
		double angle = SecondGeodeticTask2d.calcAngle2Points(orgPoint,actPoint);
		if (dist > maxDist){
			changed = true;
			Point newpos = FirstGeodeticTask2d.getPoint(orgPoint,angle,maxDist);
			for(int i=0; i < pointList.size(); i++){
				Point ptTemp = (Point)pointList.get(i);
				Coordinate coord = ptTemp.getCoordinate();
				coord.x = newpos.getX();
				coord.y = newpos.getY();
			}					
		}
		return changed;
	}
}
