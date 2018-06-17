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

package fr.michaelm.jump.plugin.match;

import com.vividsolutions.jump.feature.Feature;

/**
 * Interface for all simple matchers able to evaluate if a feature f matches
 * a reference feature ref and how well it matches.
 * Matcher is not symmetric. For example, IncludeMatcher will return 1 for
 * match(f, ref) if f is included in ref, and 0 for match(ref, f).
 * On the other hand, minimum distance should be symmetric.
 * @author Michaël Michaud
 */
public interface Matcher {
    
    //String MAXIMUM_DISTANCE = I18NPlug.getI18N("Maximum-distance");
    
    //String MINIMUM_OVERLAPPING = I18NPlug.getI18N("Minimum-overlapping");
    
    /**
     * Returns a distance measuring the match quality of Feature f with a 
     * reference Feature ref.
     * The method returns 0 if f and ref do not match at all, and 1 if they
     * match perfectly.
     * If (f == ref), match should always return 1, but the return value of
     * match(f, ref, context) if f.equals(ref) depends of the exact semantic
     * of the matcher.
     * It is not required that match(f, ref, context) = match(ref, f, context). 
     *
     * @param f Feature to match from
     * @param ref reference Feature to match to
     * @param context object containing useful information to check if
     * Feature f effectively matches Feature ref
     *
     * @return a double in the range [0-1] representative of the match quality
     * between f and ref. 
     *
     * @throws Exception if input data cannot be processed.
     */
    double match(Feature f, Feature ref, Object context) throws Exception;
    
    
    /**
     * Returns the maximum distance accepted between f1 and ref
     * Exact meaning highly depends on what distance is measured, but whatever 
     * the definition is (minimum distance, hausdorff distance, levenshtein
     * distance...), if distance between f and ref is over the maximum
     * distance, the value returned by match method will be 0.
     * <ul>
     * <li>
     * If 0 is returned, match will always return 0 except for two
     * identical features (identical meaning depends on matcher definition)
     * </li>
     * <li>
     * Returning NaN means that using a tolerance has no meaning for this 
     * matcher (example, getMaximumDistance of equals matchers returns NaN).
     * </li>
     * <li>
     * If Double.POSITIVE_INFINITY is returned, matches between f and ref will
     * alway return a non null value.
     * </li>
     * </ul>
     */
    double getMaximumDistance();
    
    
    /**
     * Returns the minimum overlapping between f and ref, where overlapping is
     * generally expressed as a percentage, but not necessarily.
     * Overlapping may have different meanings as the ratio between common area
     * and f area or between the length of the longest common substring of two
     * attributes values and the length of the full string.<br>
     * Depending on the Matcher, overlapping between f and ref may be
     * directional (ex. common area / f area) or symmetric (intersection area
     * / union area).
     * <ul>
     * <li>
     * If 0 is returned, any pair of f and ref which intersects will return a
     * non null value.
     * </li>
     * <li>
     * NaN means that this criteria has no meaning for this matcher.
     * </li>
     * <li>
     * 100.0 generally means that f and ref must be equal, but the precise
     * definition may bary from a matcher to another.
     * </li>
     * </ul>
     */
    double getMinimumOverlapping();
    
    /**
     * Sets the maximum distance returning a non null value.
     * @see #getMaximumDistance
     */
    void setMaximumDistance(double max_dist);
    
    /**
     * Sets the minimum overlapping ratio returning a non null value.
     * @see #getMinimumOverlapping
     */
    void setMinimumOverlapping(double min_overlap);

}
