/**
 * @(#)CoordinateTransformFilter.java	28.06.2004
 *
 * Copyright 2004 Edgar Soldin
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package de.soldin.jump.cts;

import org.geotools.cs.CoordinateSystem;
import org.geotools.cs.CoordinateSystemFactory;
import org.geotools.cs.FactoryException;
import org.geotools.ct.CannotCreateTransformException;
import org.geotools.ct.CoordinateTransformation;
import org.geotools.ct.CoordinateTransformationFactory;
import org.geotools.ct.MathTransform;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;

/**
 * Class <code>CoordinateTransformFilter</code> is a utility for transforming
 * coordinates between {@link org.geotools.cs.CoordinateSystem coordinatesystems}.
 */
public class CoordinateTransformFilter implements CoordinateFilter{
	private CoordinateSystem sourceCSWKT, targetCSWKT;
	private MathTransform transform;
	private boolean yx = false;
	
	/**
	 * Creates a new Filter.
	 * 
	 * @param src_wkt the source cs as WKT-string
	 * @param trg_wkt the target cs as WKT-string
	 * @throws CannotCreateTransformException
	 * @throws FactoryException
	 */
	public CoordinateTransformFilter(String src_wkt, String trg_wkt) 
	 throws CannotCreateTransformException, FactoryException{
		this( 
			CoordinateSystemFactory.getDefault().createFromWKT(src_wkt),
			CoordinateSystemFactory.getDefault().createFromWKT(trg_wkt) 
			);
	}

	/**
	 * Creates a new Filter.
	 * 
	 * @param src the source cs
	 * @param trg the target cs
	 * @throws CannotCreateTransformException
	 */
	public CoordinateTransformFilter(CoordinateSystem src, CoordinateSystem trg) 
	 throws CannotCreateTransformException{
		CoordinateTransformationFactory trFactory =
			CoordinateTransformationFactory.getDefault();
		CoordinateTransformation transformation =
			trFactory.createFromCoordinateSystems(src, trg);

		this.transform = transformation.getMathTransform();
	}
	
	/**
	 * Actually modifies the given coordinate
	 * 
	 * @see com.vividsolutions.jts.geom.CoordinateFilter#filter(com.vividsolutions.jts.geom.Coordinate)
	 */
	public void filter(Coordinate coord){
		double[] ord = new double[2];
		if (this.yx){ 
			ord[0] = coord.y; ord[1] = coord.x; 
		}else{ 
			ord[0] = coord.x; ord[1] = coord.y; 	
		}
		try {
			this.transform.transform( ord, 0, ord, 0, ord.length/this.transform.getDimSource() );
			if (this.yx){
				coord.setCoordinate( new Coordinate( ord[1],ord[0] ) );
			}else{
				coord.setCoordinate( new Coordinate( ord[0],ord[1] ) );
			}	
					
		} catch (Exception e) {
			System.err.println(e.toString());
		}
	}

	/**
	 * This is just a workaround. Better modify your definitions in
	 * the <code>cs.conf</code> file.
	 * 
	 * @return whether the axes are exchanged. 
	 */
	public boolean isYx() {
		return this.yx;
	}

	/**
	 * Activate the workaround {@link #isYx()}.
	 * 
	 * @param b
	 */
	public void setYx(boolean b) {
		this.yx = b;
	}
	
}
