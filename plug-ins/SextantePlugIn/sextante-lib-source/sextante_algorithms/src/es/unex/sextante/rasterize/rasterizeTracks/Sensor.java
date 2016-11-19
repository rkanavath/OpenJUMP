package es.unex.sextante.rasterize.rasterizeTracks;

import com.vividsolutions.jts.geom.Coordinate;

import es.unex.sextante.core.AnalysisExtent;
import es.unex.sextante.rasterWrappers.GridCell;

public class Sensor extends Coordinate{;
	public double value;
	
	public Sensor(Coordinate c, double value) {
		super(c);
		this.value 	= 	value;
	}
	
	public Sensor(double x, double y, double value) {
		super(x,y);
		this.value 	= 	value;
	}
	
	@Override
	public String toString() {
		return super.toString() + " value: " +value; 
	}
}
