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
import com.vividsolutions.jts.geom.Geometry;


/**
 * Matcher measuring hausdorff distance between geometries.
 *
 * @author Michaël Michaud
 */
public class SemiHausdorffDistanceMatcher extends HausdorffDistanceMatcher {
    
    private static final SemiHausdorffDistanceMatcher SEMI_HAUSDORFF_DISTANCE =
        new SemiHausdorffDistanceMatcher(1.0);
    
    public static SemiHausdorffDistanceMatcher instance() {
        return SEMI_HAUSDORFF_DISTANCE;
    }

    public SemiHausdorffDistanceMatcher(double max_dist) {
        super(max_dist);
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(Geometry source, Geometry target, Object context) 
                                                              throws Exception {
        DiscreteHausdorffDistance D = new DiscreteHausdorffDistance(source, target);
        double maxDxDy = 1.414 * maxDxDy(source);
        // If maxDxDy is greater than max_dist densify the geometry
        if (max_dist < 0.75*maxDxDy) D.setDensifyFraction(max_dist/maxDxDy);
        double dist = D.orientedDistance();
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
