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
import fr.michaelm.util.text.Rule;
import fr.michaelm.util.text.RuleRegistry;
import fr.michaelm.util.text.TransformationException;
import java.text.Collator;
import java.util.Collection;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.TreeMap;

/**
 * Interface for all attribute matchers.
 * Inherits Matcher methods.
 *
 * @see fr.michaelm.jump.plugin.match.Matcher
 *
 * @author Michaël Michaud
 */
public abstract class StringMatcher extends AttributeMatcher {
    
    Collator collator = Collator.getInstance(Locale.getDefault());
    Rule sourceRule = RuleRegistry.NEUTRAL;
    Rule targetRule = RuleRegistry.NEUTRAL;
    
    StringMatcher(String sourceAttribute, String targetAttribute) {
        super(sourceAttribute, targetAttribute);
        collator.setStrength(Collator.IDENTICAL);
    }
    
    /**
     * Returns a distance measuring the match between two character strings.
     * The method returns 0 if source and target do not match at all, and 1 if 
     * they match perfectly.
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
    public abstract double match(String source, String target, Object context)
                                                               throws Exception;
    
    /**
     * {@inheritDoc}.
     */
    public double match(Object source, Object target, Object context) 
                                                              throws Exception {
        return match(source.toString(), target.toString(), context);
    }
    
    /**
     * {@inheritDoc}.
     */
    public double match(Feature source, Feature target, Object context) 
                                                              throws Exception {
        return match(sourceRule.transform(source.getString(sourceAttribute)), 
                      targetRule.transform(target.getString(targetAttribute)), context);
    }
    
    public void setSourceRule(Rule sourceRule) {
        this.sourceRule = sourceRule;
    }
     
    public void setTargetRule(Rule targetRule) {
        this.targetRule = targetRule;
    }
    
    public Rule getSourceRule() {
        return sourceRule;
    }
     
    public Rule getTargetRule() {
        return targetRule;
    }
    
    /**
     * The default index for StringMatcher maps each possible target 
     * attribute value to the features having this value.
     * @param features features to index
     * @throws TransformationException if the targetRule could not be applied
     *         to target features to build the index.
     */
    public Index createIndex(final Collection<Feature> features) 
                                                 throws TransformationException {
        final TreeMap<String,Set<Feature>> index = 
            new TreeMap<>(collator);
        for (Feature f : features) {
            String value = targetRule.transform(f.getString(getTargetAttribute()));
            Set<Feature> set = index.get(value);
            if (set == null) {
                set = new HashSet<>();
                index.put(value, set);
            }
            set.add(f);
        }
        return new Index() {
            public Set<Feature> query(Object value) {
                return index.get(value.toString());
            }
        };
    }

}
