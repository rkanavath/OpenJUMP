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
import com.vividsolutions.jts.geom.util.AffineTransformation;


/**
 * ShapeMatcher compares two geometries :
 * - centroid of first geometry is moved to the centroid of second geometry
 * - intersection of aligned geometries is computed
 *   (for lineal geometries, a buffer of half the max_dist is computed first)
 * - worst ratio of intersection and both geometry area is computed
 * - matching value is evaluated from 0 (50% overlapping) to 1 (100% overlapping)
 *
 * @author Michaël Michaud
 */
public class ShapeMatcher extends GeometryMatcher {
    
    private static final ShapeMatcher SHAPE =
        new ShapeMatcher(1.0, 50.0);
    
    public static ShapeMatcher instance() {
        return SHAPE;
    }

    public ShapeMatcher(double max_dist, double min_overlap) {
        this.max_dist = max_dist;
        this.min_overlap = min_overlap;
    }
    
    public double match(Geometry source, Geometry target, Object context) 
                                                              throws Exception {
        Coordinate c1 = source.getCentroid().getCoordinate();
        Coordinate c2 = target.getCentroid().getCoordinate();
        if (c1.distance(c2) > max_dist) return 0.0; // short-circuit
        AffineTransformation trans = AffineTransformation.translationInstance(-c1.x, -c1.y);
        trans.translate(c2.x, c2.y);
        source = (Geometry)source.clone();
        source.apply(trans);
        if (source.getDimension() == 1) source = source.buffer(max_dist/2);
        if (target.getDimension() == 1) target = target.buffer(max_dist/2);
        double overlappingArea = source.intersection(target).getArea();
        double overlapping = 100.0 * Math.min(overlappingArea/source.getArea(),
                                              overlappingArea/target.getArea());
        return (overlapping-min_overlap)/(100.0-min_overlap);
    }
    
    /**
     * Sets the maximum distance returning a non null match value.
     * @see #getMaximumDistance
     */
    public void setMaximumDistance(double max_dist) {
        this.max_dist = max_dist;
    }
    
    
    /**
     * Sets the minimum overlapping returning a non null match value.
     * @see #getMinimumOverlapping
     */
    public void setMinimumOverlapping(double min_overlap) {
        this.min_overlap = min_overlap;
    }
    
}
