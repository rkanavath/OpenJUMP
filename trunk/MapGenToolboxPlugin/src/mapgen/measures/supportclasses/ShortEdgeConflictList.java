/***********************************************
 * created on 		07.06.2005
 * last modified: 	03.07.2005 (method clone() added)
 * 
 * author:			sstein
 * 
 * description:
 *  Special ArrayList for objects of type ShortEdgeConflict
 * 
 ***********************************************/
package mapgen.measures.supportclasses;

import java.util.ArrayList;
import java.util.Iterator;


/**
 * @author sstein
 *
 * Special ArrayList for objects of type ShortEdgeConflict
 * 
 */
public class ShortEdgeConflictList{

    ArrayList conflicts;
    
    /**
     * Constructs a new, empty ShortEdgeConflict list.
     * 
     * All elements inserted into the List must implement the Comparable interface. Furthermore, 
     * all such elements must be mutually comparable: e1.compareTo(e2) must not throw a ClassCastException 
     * for any elements e1 and e2 in the List.
     */    
    public ShortEdgeConflictList(){
        conflicts = new ArrayList();
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
    public boolean add(ShortEdgeConflict conflict){
        return conflicts.add(conflict);
    }
    
    /** 
     * Removes all elements from the priority queue.
     * @see java.util.Collection#clear()
     */
    public void clear(){
        conflicts.clear();
    }    
    
    public int size(){
        return conflicts.size();
    }
    

    public boolean remove(ShortEdgeConflict conflict){
        return conflicts.remove(conflict);
    }

    public void removeByIndex(int listIndex){
        conflicts.remove(listIndex);
    }

    public ArrayList getList(){        
        return conflicts;
    }    

    public ShortEdgeConflict get(int index){
        return (ShortEdgeConflict)conflicts.get(index);        
    }
 
    public Iterator iterator(){
        return conflicts.iterator();
    }

    /**
     * 
     * @return strongest conflict, means smallest edge
     */
    public ShortEdgeConflict getStrongestConflict(){
    	ShortEdgeConflict smallestE = null;
    	try{
	        //--init
	    	smallestE = (ShortEdgeConflict)this.conflicts.get(0);
	        double smallestDist = smallestE.length;
	        //--search
	        for (int i = 1; i < this.conflicts.size(); i++) {
	        	ShortEdgeConflict sec = (ShortEdgeConflict)this.conflicts.get(i);
	            if(sec.length < smallestDist){
	                smallestDist = sec.length;
	                smallestE = (ShortEdgeConflict)sec.clone();
	            }
	        }        	        
    	}
    	catch(Exception e){
    		System.out.println("ShortEdgeConflictList: getStrongestConflict .. not possible => no conflicts");
    	}
    	return smallestE;
    }
    
    public int getShortestEdgeConflictListIndex(){
    	ShortEdgeConflict smallestE = null;
    	int index = 0;
    	try{
	        //--init
	    	smallestE = (ShortEdgeConflict)this.conflicts.get(0);
	    	index=0;
	        double smallestDist = smallestE.length;
	        //--search
	        for (int i = 1; i < this.conflicts.size(); i++) {
	        	ShortEdgeConflict sec = (ShortEdgeConflict)this.conflicts.get(i);
	            if(sec.length < smallestDist){
	                smallestDist = sec.length;
	                smallestE = (ShortEdgeConflict)sec.clone();
	                index=i;
	            }
	        }        	        
    	}
    	catch(Exception e){
    		System.out.println("ShortEdgeConflictList: getStrongestConflict .. not possible => no conflicts");
    	}
    	return index;
    }
    
    public Object clone(){
    	ShortEdgeConflictList secl = new ShortEdgeConflictList();
    	for (Iterator iter = this.conflicts.iterator(); iter.hasNext();) {
			ShortEdgeConflict element = (ShortEdgeConflict) iter.next();
			secl.add(element);
		}
		
    	return secl;
    }
}
