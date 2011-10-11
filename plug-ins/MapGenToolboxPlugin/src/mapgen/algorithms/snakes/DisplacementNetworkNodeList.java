/***********************************************
 * created on 		20.08.2005
 * last modified: 						
 * 
 * author:			sstein
 * 
 * description:
 *  contains the network nodes for displacement algorithm   
 ***********************************************/
package mapgen.algorithms.snakes;

import java.util.ArrayList;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Point;

/**
 * @author sstein
 *
 */
public class DisplacementNetworkNodeList {

    ArrayList networkNodes;
    
    /**
     * Constructs a new, empty NetworkNodes list.
     * 
     * All elements inserted into the List must implement the Comparable interface. Furthermore, 
     * all such elements must be mutually comparable: e1.compareTo(e2) must not throw a ClassCastException 
     * for any elements e1 and e2 in the List.
     */    
    public DisplacementNetworkNodeList(){
        networkNodes = new ArrayList();
    }
    
    /**
     * Adds the specified element to this List.
     * 
     * All elements inserted into the PriorityQueue must implement the Comparable interface. Furthermore, 
     * all such elements must be mutually comparable: e1.compareTo(e2) must not throw a ClassCastException 
     * for any elements e1 and e2 in the List.
     * 
     * @see java.util.Collection#add(java.lang.Object)
     */
    public boolean add(DisplacementNetworkNode node){
        return this.networkNodes.add(node);
    }
    
    /** 
     * Removes all elements from the priority queue.
     * @see java.util.Collection#clear()
     */
    public void clear(){
    	networkNodes.clear();
    }    
    
    public int size(){
        return networkNodes.size();
    }
    

    public boolean remove(DisplacementNetworkNode node){
        return networkNodes.remove(node);
    }

    public void removeByIndex(int listIndex){
    	networkNodes.remove(listIndex);
    }

    public ArrayList getList(){        
        return networkNodes;
    }    

    public DisplacementNetworkNode get(int index){
        return (DisplacementNetworkNode)networkNodes.get(index);        
    }
 
    public Iterator iterator(){
        return networkNodes.iterator();
    }
    
    public Object clone(){
    	DisplacementNetworkNodeList nl = new DisplacementNetworkNodeList();
    	for (Iterator iter = this.networkNodes.iterator(); iter.hasNext();) {
			DisplacementNetworkNode element = (DisplacementNetworkNode) iter.next();
			nl.add(element);
		}
		
    	return nl;
    }

    /**
     * This function needs to proceed #allocatePoints() first.
     * @return List of JTS points, point cooridnates are averaged
     */
    public ArrayList getAverageNodePoints(){
    	//this.allocatePoints(lineToDisplaceList);
    	ArrayList points = new ArrayList();
		for (int i = 0; i < this.networkNodes.size(); i++) {
			DisplacementNetworkNode node = (DisplacementNetworkNode)this.networkNodes.get(i);
			Point pt = node.calcAveragePoint();
			points.add(pt);
		}
		return points;
    }
    
    /**
     * The function allocates the point coordinates (obtained from the Point ID) to the nodes.
     * That means it creates a pointList of a node. 
     * @param lineToDisplaceList the list musst contains Jump Features of two attributes
     * a geometry of type LineString and a unique LineID of type Integer
     */
    public void allocatePoints(ArrayList lineToDisplaceList){
		for (int i = 0; i < this.networkNodes.size(); i++) {
			DisplacementNetworkNode node = (DisplacementNetworkNode)this.networkNodes.get(i);
			node.receivePointsFromLineIndizes(lineToDisplaceList);
		}    	
    }
    
    /**
     * this function needs to proceed #allocatePoints() first
     * It sets all points of a node to the same average coordinate, 
     * thus re-creates the network connection. 
     */
    public void setNodePointsToAverage(){
		for (int i = 0; i < this.networkNodes.size(); i++) {
			DisplacementNetworkNode node = (DisplacementNetworkNode)this.networkNodes.get(i);
			node.setPointsToAverage();
		}    	    	
    }
    
    /**
     * 
     * @param maxDist
     * @return ArrayList containing the index of reset nodes 
     * where dist(original Point,actual pointO was greater than maxDist 
     */
	public ArrayList checkAndSetActualAverageNodeToMaxDist(double maxDist){
		ArrayList changedNodeId = new ArrayList();
		for (int i = 0; i < this.networkNodes.size(); i++) {
			boolean changed = false;
			DisplacementNetworkNode node = (DisplacementNetworkNode)this.networkNodes.get(i);
			changed=node.checkAndSetActualAverageNodeToMaxDist(maxDist);
			if (changed){
				changedNodeId.add(new Integer(i));
			}
		}    	    			
		return changedNodeId;
	}

}
