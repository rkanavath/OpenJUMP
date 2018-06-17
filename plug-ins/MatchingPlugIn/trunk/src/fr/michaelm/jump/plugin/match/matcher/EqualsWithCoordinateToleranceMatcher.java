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

import com.vividsolutions.jts.geom.Geometry;


/**
 * Matcher checking if two 2D geometries match exactly, including
 * component and point ordering.
 *
 * @author Michaël Michaud
 */
public class EqualsWithCoordinateToleranceMatcher extends GeometryMatcher {
    
    private static final EqualsWithCoordinateToleranceMatcher EQUALS_WITH_COORDINATE_TOLERANCE =
        new EqualsWithCoordinateToleranceMatcher(1.0);
    
    public static EqualsWithCoordinateToleranceMatcher instance() {
        return EQUALS_WITH_COORDINATE_TOLERANCE;
    }

    public EqualsWithCoordinateToleranceMatcher(double max_dist) {
        this.max_dist = max_dist;
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(Geometry source, Geometry target, Object context) throws Exception {
        source = (Geometry)source.clone();
        source.normalize();
        target = (Geometry)target.clone();
        target.normalize();
        return source.equalsExact(target, max_dist)? 1.0 : 0.0;
    }
    
    /**
     * Sets the maximum distance returning between normalized geometry points.
     * @see #getMaximumDistance
     */
    public void setMaximumDistance(double max_dist) {
        this.max_dist = max_dist;
    }
    
}
