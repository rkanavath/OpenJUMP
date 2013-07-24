package org.openjump.core.openstreetmap.model;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

public class OjOsmNode extends OjOsmPrimitive {
	private Coordinate coord = null;
	private boolean usedInWay = false;

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
	
	public boolean isUsedInAWay(){
		return this.usedInWay;
	}
	
	public void setUsedInAWay(boolean usedInWay){
		this.usedInWay = usedInWay;
	}
		
}
