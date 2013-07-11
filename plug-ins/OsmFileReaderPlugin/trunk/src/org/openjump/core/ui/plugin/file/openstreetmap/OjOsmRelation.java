package org.openjump.core.ui.plugin.file.openstreetmap;

import java.util.ArrayList;

public class OjOsmRelation extends OjOsmPrimitive {

	private ArrayList<OjOsmRelationMember> members = new ArrayList<OjOsmRelationMember>();
	
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
