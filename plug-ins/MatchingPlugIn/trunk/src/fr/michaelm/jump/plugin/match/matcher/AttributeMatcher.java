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
import fr.michaelm.jump.plugin.match.Index;
import java.util.Collection;


/**
 * Interface for all attribute matchers.
 * Inherits Matcher methods.
 *
 * @see fr.michaelm.jump.plugin.match.Matcher
 *
 * @author Michaël Michaud
 */
public abstract class AttributeMatcher extends AbstractMatcher {
    
    String sourceAttribute;
    String targetAttribute;
    
    AttributeMatcher(String sourceAttribute, String targetAttribute) {
        this.sourceAttribute = sourceAttribute;
        this.targetAttribute = targetAttribute;
    }
    
    /**
     * Returns a distance measuring the match score between the attribute of the
     * source and the attribute of the target.
     * The method returns 0 if a and ref do not match at all, and 1 if they
     * match perfectly.
     * If (source == target), match should always return 1, but the return value
     * of match(source, target, context) if source.equals(target) depends of the
     * exact semantic of the matcher.
     * It is not required that 
     * match(source, target, context) = match(target, source, context). 
     *
     * @param source feature to match from
     * @param target feature to match to
     * @param context object containing useful information to check if
     * Attribute source effectively matches Attribute target
     *
     * @return a double in the range [0-1] representative of the match quality
     * between a and ref. 
     *
     * @throws Exception if input data cannot be processed.
     */
    public abstract double match(Object source, Object target, Object context)
                                                               throws Exception;
    
    /**
     * {@inheritDoc}.
     */
    public double match(Feature source, Feature target, Object context) 
                                                              throws Exception {
        return match(source.getAttribute(sourceAttribute), 
                      target.getAttribute(targetAttribute), context);
    }
     
    public void setAttributes(String sourceAttribute, String targetAttribute) {
       this.sourceAttribute = sourceAttribute;
       this.targetAttribute = targetAttribute;
    }
     
    public String getSourceAttribute() {
        return sourceAttribute;
    }
     
    public String getTargetAttribute() {
        return targetAttribute;
    }
    
    /**
     * The default index for AttributeMatcher maps each possible target 
     * attribute value to the features having this value.
     * @param features features to index
     */
    abstract public Index createIndex(final Collection<Feature> features) throws Exception;

}
