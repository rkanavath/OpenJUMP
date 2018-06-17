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

import com.vividsolutions.jump.feature.Feature;
import fr.michaelm.jump.plugin.match.I18NPlug;
import fr.michaelm.jump.plugin.match.Matcher;

/**
 * Abstract Matcher implementing common methods
 *
 * @author Michaël Michaud
 */
public abstract class AbstractMatcher implements Matcher {
    
    protected double max_dist = Double.NaN;
    
    protected double min_overlap = Double.NaN;
    
    /**
     * {@inheritDoc}.
     */
    public abstract double match(Feature f, Feature ref, Object context) throws Exception;
    
    /**
     * The {@link #match(Feature,Feature,Object)} method without context.
     */
     public double match(Feature f, Feature ref) throws Exception {
         return match(f, ref, null);
     }
    
    /**
     * Returns the name of this Matcher
     */
     public String toString() {
         return I18NPlug.getI18N("matcher." + getClass().getSimpleName());
     }
     
    /**
     * Returns NaN means that this criteria has no meaning for this matcher.
     */
     public double getMaximumDistance() {
         return max_dist;
     }
    
    /**
     * Returns NaN means that this criteria has no meaning for this matcher.
     */
     public double getMinimumOverlapping() {
         return min_overlap;
     }
     
    /**
     * In this main abstract implementation, setMaximumDistance has no effect.
     */
     public void setMaximumDistance(double max_dist) {
         this.max_dist = Double.NaN;
     }
     
    /**
     * In this main abstract implementation, setMinimumOverlapping has no effect.
     */
     public void setMinimumOverlapping(double min_overlap) {
         this.min_overlap = Double.NaN;
     }

}
