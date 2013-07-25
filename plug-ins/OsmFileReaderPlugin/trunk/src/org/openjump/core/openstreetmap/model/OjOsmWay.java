/*
 *  License: GPL. See License file for details.
 *  Adapted from JOSM by Stefan Steiniger for use in OpenJUMP [25.July.2013] 
 */
package org.openjump.core.openstreetmap.model;

import java.util.ArrayList;

public class OjOsmWay extends OjOsmPrimitive {

	private ArrayList<Long> nodeIds = null;
	/** whether or not the first node's ID and last nodes ID are the same. Tested when the nodes are set **/
	private boolean closed = false;
	
	public OjOsmWay() {
		super();
		this.setOsmType(OjOsmPrimitive.OSM_PRIMITIVE_WAY);
	}
	
	public ArrayList<Long> getNodeIds() {
		return nodeIds;
	}

	public void setNodeIds(ArrayList<Long> nodeIds) {
		this.nodeIds = nodeIds;
		if(nodeIds.size() > 1){
			long firstNodeID = nodeIds.get(0);
			long lastNodeID = nodeIds.get(nodeIds.size()-1);
			if (firstNodeID == lastNodeID){
				this.closed = true;
			}
		}
					
	}

	public boolean isClosed() {
		return closed;
	}
	
		
}
