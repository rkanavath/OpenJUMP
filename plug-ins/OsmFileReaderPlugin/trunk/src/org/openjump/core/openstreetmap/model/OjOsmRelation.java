/*
 *  License: GPL. See LICENSE file for details.
 *  Adapted from JOSM by Stefan Steiniger for use in OpenJUMP [25.July.2013] 
 */
package org.openjump.core.openstreetmap.model;

import java.util.ArrayList;

public class OjOsmRelation extends OjOsmPrimitive {

	private ArrayList<OjOsmRelationMember> members = new ArrayList<OjOsmRelationMember>();
	
	/**
	 * Whether or not some members (e.g. way IDs) were not found in the current file.
	 * @return
	 */
	public boolean isMissingMembers() {
		return missingMembers;
	}

	public void setMissingMembers(boolean missingMembers) {
		this.missingMembers = missingMembers;
	}

	private boolean missingMembers = false;
	
	public OjOsmRelation() {
		super();
		this.setOsmType(OjOsmPrimitive.OSM_PRIMITIVE_RELATION);
	}
	
	public ArrayList<OjOsmRelationMember> getMembers() {
		return members;
	}

	public void setMembers(ArrayList<OjOsmRelationMember> members) {
		this.members = members;			
	}
	
	public String getRelationType(){
		String type = "";
		if(this.hasKey("type")){
			type = this.get("type");
		}
		return type;
	}
	
	public boolean isMultiPolygon(){
		boolean isMultiPoly = false;
		if(this.hasKey("type")){
			if(this.get("type").equalsIgnoreCase("multipolygon")){
				isMultiPoly = true;
			}
		}
		return isMultiPoly;
	}
			
}
