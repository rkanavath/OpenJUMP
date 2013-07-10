package org.openjump.core.ui.plugin.file.openstreetmap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

public class OjOsmNode extends OjOsmPrimitive {
	private Coordinate coord = null;
	private boolean usedInWay = false;
	private boolean usedInRelation = false;

	public OjOsmNode() {
		super();
		this.setOsmType(OjOsmPrimitive.OSM_PRIMITIVE_NODE);
	}

	public Coordinate getCoord() {
		return coord;
	}

	public void setCoord(Coordinate coord) {
		this.coord = coord;
		GeometryFactory gf = new GeometryFactory();
		this.setGeom(gf.createPoint(coord));
	}
	
	public boolean isUsedInARelation(){
		return this.usedInRelation;
	}
	
	public void setUsedInARelation(boolean usedInRelation){
		this.usedInRelation = usedInRelation;
	}
	
	public boolean isUsedInAWay(){
		return this.usedInWay;
	}
	
	public void setUsedInAWay(boolean usedInWay){
		this.usedInRelation = usedInWay;
	}
		
}
