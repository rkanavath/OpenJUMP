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

import com.vividsolutions.jts.algorithm.distance.DiscreteHausdorffDistance;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;


/**
 * Matcher measuring hausdorff distance between geometries.
 *
 * @author Michaël Michaud
 */
public class HausdorffDistanceMatcher extends GeometryMatcher {
    
    private static final HausdorffDistanceMatcher HAUSDORFF_DISTANCE =
        new HausdorffDistanceMatcher(1.0);
    
    public static HausdorffDistanceMatcher instance() {
        return HAUSDORFF_DISTANCE;
    }

    public HausdorffDistanceMatcher(double max_dist) {
        this.max_dist = max_dist;
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(Geometry source, Geometry target, Object context)
                                                              throws Exception {
        double maxDxDy = 1.414 * Math.max(maxDxDy(source), maxDxDy(target));
        DiscreteHausdorffDistance D = new DiscreteHausdorffDistance(source, target);
        // If maxDxDy is greater than max_dist densify the geometry
        if (max_dist < 0.75*maxDxDy) D.setDensifyFraction(max_dist/maxDxDy);
        double dist = D.distance();
        if (dist > max_dist) return 0.0;
        else return 1.0 - dist / max_dist;
    }
    
    /**
     * Return the maximum length of a segment. 
     * To avoid the calculation of a SquareRoot, the max distance along one of
     * the axis is computed.
     * As a consequence, the true max length of a segment is comprised between
     * maxDxDy and 1.414*maxDxDy
     */
    protected double maxDxDy(Geometry g) throws Exception {
        if (g.getNumGeometries() > 1) return maxDxDy((GeometryCollection)g);
        else if (g.getDimension() == 2) return maxDxDy((Polygon)g);
        else if (g.getDimension() == 1) return maxDxDy((LineString)g);
        else if (g.getDimension() == 0) return 0;
        else throw new Exception("Cannot compute maxDxDy on unknown geometry type " + g);
    }
    
    protected double maxDxDy(GeometryCollection g) throws Exception {
        double max = 0;
        for (int i = 0 ; i < g.getNumGeometries() ; i++) {
            max = Math.max(max, maxDxDy(g.getGeometryN(i)));
        }
        return max;
    }
    
    protected double maxDxDy(Polygon g) throws Exception {
        double max = maxDxDy(g.getExteriorRing());
        for (int i = 0 ; i < g.getNumInteriorRing() ; i++) {
            max = Math.max(max, maxDxDy(g.getInteriorRingN(i)));
        }
        return max;
    }
    
    protected double maxDxDy(LineString g) throws Exception {
        double max = 0;
        Coordinate[] cc = g.getCoordinates();
        for (int i = 0, nbc = cc.length-1 ; i < nbc ; i++) {
            double dx = Math.abs(cc[i].x-cc[i+1].x);
            double dy = Math.abs(cc[i].y-cc[i+1].y);
            if (dx > max) max = dx;
            if (dy > max) max = dy;
        }
        return max;
    }
    
    protected double maxDxDy(Point g) throws Exception {
        return 0;
    }
    
    /**
     * Sets the maximum Hausdorff distance accepted between two geometries.
     * @see #getMaximumDistance
     */
    public void setMaximumDistance(double max_dist) {
        this.max_dist = max_dist;
    }
    
}
