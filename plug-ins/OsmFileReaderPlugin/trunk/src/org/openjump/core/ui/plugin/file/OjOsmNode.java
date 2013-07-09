package org.openjump.core.ui.plugin.file;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

public class OjOsmNode extends OjOsmPrimitive {
	private Coordinate coord = null;

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
		
}
