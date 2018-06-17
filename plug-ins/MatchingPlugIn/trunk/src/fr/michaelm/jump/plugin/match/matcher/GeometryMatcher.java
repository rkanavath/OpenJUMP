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
import com.vividsolutions.jump.feature.Feature;

/**
 * Interface for all simple geometry based matchers.
 * Inherits Matcher methods.
 *
 * @see fr.michaelm.jump.plugin.match.Matcher
 *
 * @author Michaël Michaud
 */
public abstract class GeometryMatcher extends AbstractMatcher {
    
    /**
     * Returns a distance measuring the match quality of Geometry g with a 
     * reference Geometry ref.
     * The method returns 0 if g and ref do not match at all, and 1 if they
     * match perfectly.
     * If (g == ref), match should always return 1, but the return value of
     * match(g, ref, context) if g.equals(ref) depends of the exact semantic
     * of the matcher.
     * It is not required that match(g, ref, context) = match(ref, g, context). 
     *
     * @param source Geometry to match from
     * @param target Geometry to match to
     * @param context object containing useful information to check if
     * Geometry g effectively matches Geometry ref
     *
     * @return a double in the range [0-1] representative of the match quality
     * between g and ref. 
     *
     * @throws Exception if input data cannot be processed.
     */
    public abstract double match(Geometry source, Geometry target, Object context) 
                                                               throws Exception;
    
    /**
     * {@inheritDoc}.
     */
     public double match(Feature source, Feature target, Object context) throws Exception {
         return match(source.getGeometry(), target.getGeometry(), context);
     }

}
