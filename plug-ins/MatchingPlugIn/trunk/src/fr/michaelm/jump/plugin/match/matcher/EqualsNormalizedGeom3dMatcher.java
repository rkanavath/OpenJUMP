/*
 * (C) 2017 Michaël Michaud
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * For more information, contact:
 *
 * m.michael.michaud@orange.fr
 */

package fr.michaelm.jump.plugin.match.matcher;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;


/**
 * Matcher checking if two normalized 3D geometries match exactly.
 * Normalization set components and coordinates of both geometries in the same
 * order.
 *
 * @author Michaël Michaud
 */
public class EqualsNormalizedGeom3dMatcher extends GeometryMatcher {
    
    private static final EqualsNormalizedGeom3dMatcher EQUALS_NORMALIZED_GEOM3D =
        new EqualsNormalizedGeom3dMatcher();
    
    public static EqualsNormalizedGeom3dMatcher instance() {
        return EQUALS_NORMALIZED_GEOM3D;
    }
    
    public EqualsNormalizedGeom3dMatcher() {
    }
    
   /**
    * {@inheritDoc}.
    */
    public double match(Geometry source, Geometry target, Object context) throws Exception {
        source = (Geometry)source.clone();
        source.normalize();
        target = (Geometry)target.clone();
        target.normalize();
        if (source.equalsExact(target)) {
            Coordinate[] cc1 = source.getCoordinates();
            Coordinate[] cc2 = target.getCoordinates();
            for (int i = 0 ; i < cc1.length ; i++) {
                // Converting to long manage NaN values as we want
                if (Double.doubleToLongBits(cc1[i].z) 
                    != Double.doubleToLongBits(cc2[i].z)) return 0.0;
            }
            return 1.0;
        }
        return 0.0;
    }
    
}
