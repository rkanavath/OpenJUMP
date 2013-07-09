package org.openjump.core.ui.plugin.file;

public class OjOsmRelationMember {
	
	private final String role;
    private final long memberId;
    private final int osmPrimitiveType;
    
    public OjOsmRelationMember(String role, int type, long id) {
        this.role = role == null?"":role;
        this.osmPrimitiveType = type;
        this.memberId = id;
    }
    
	public String getRole() {
		return role;
	}
	public long getMemberId() {
		return memberId;
	}
	public int getOsmPrimitiveType() {
		return osmPrimitiveType;
	}
    
	/**
	 * TODO check if this method works.
	 * @return
	 */
	public boolean hasRole(){
		if(role.length() > 0){
			return true;
		}
		else{
			return false;
		}
	}
	
    static int getOsmPrimitiveTypeFromParsedString(String parsedTypeValue){
    	int type = -1;
    	if(parsedTypeValue.equalsIgnoreCase("node")){
    		type = OjOsmPrimitive.OSM_PRIMITIVE_NODE;
    	}
    	else if(parsedTypeValue.equalsIgnoreCase("way")){
    		type =  OjOsmPrimitive.OSM_PRIMITIVE_WAY;
    	}
    	else if(parsedTypeValue.equalsIgnoreCase("relation")){
    		type = OjOsmPrimitive.OSM_PRIMITIVE_RELATION;
    	}
    	return type;
    }
    
    static boolean isInnerWay(String role){
    	boolean isInner = false;
    	if(role.equalsIgnoreCase("inner")){
    		isInner = true;
    	}
    	return isInner;
    }
}
