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
 * Matcher measuring minimum distance between geometries and setting a match 
 * value of 0 for distances over the max.
 *
 * @author Michaël Michaud
 */
public class MinimumDistanceMatcher extends GeometryMatcher {
    
    private static final MinimumDistanceMatcher MINIMUM_DISTANCE =
        new MinimumDistanceMatcher(1.0);
    
    public static MinimumDistanceMatcher instance() {
        return MINIMUM_DISTANCE;
    }

    public MinimumDistanceMatcher(double max_dist) {
        this.max_dist = max_dist;
    }
    
    public double match(Geometry source, Geometry target, Object context) 
                                                              throws Exception {
        double dist = source.distance(target);
        if (dist > max_dist) return 0.0;
        else return 1.0 - dist / max_dist;
    }
    
    /**
     * Sets the maximum Hausdorff distance accepted between two geometries.
     * @see #getMaximumDistance
     */
    public void setMaximumDistance(double max_dist) {
        this.max_dist = max_dist;
    }
    
}
