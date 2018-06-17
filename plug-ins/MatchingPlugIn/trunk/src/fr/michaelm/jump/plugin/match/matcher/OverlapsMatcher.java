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
 * Matcher checking if source geometry overlaps more than xx% of target geometry.
 *
 * @author Michaël Michaud
 */
public class OverlapsMatcher extends GeometryMatcher {
    
    private static final OverlapsMatcher OVERLAPS =
        new OverlapsMatcher(50.0);
    
    public static OverlapsMatcher instance() {
        return OVERLAPS;
    }
    
    public OverlapsMatcher(double min_overlap) {
        this.min_overlap = min_overlap;
    }
    
    public double match(Geometry g, Geometry ref, Object context) throws Exception {
        Geometry intersection = g.intersection(ref);
        double score = 0.0;
        if (ref.getDimension() == 2) {
            score = (100.0 * intersection.getArea() / ref.getArea() - min_overlap) / (100 - min_overlap);
        } else if (ref.getDimension() == 1) {
            score = (100.0 * intersection.getLength() / ref.getLength() - min_overlap) / (100 - min_overlap);
        }
        return Math.max(0, score);
    }
    
    /**
     * Sets the minimum overlapping returning a non null match value.
     * @see #getMinimumOverlapping
     */
    public void setMinimumOverlapping(double min_overlap) {
        this.min_overlap = min_overlap;
    }
    
}
