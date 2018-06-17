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
import fr.michaelm.util.text.TransformationException;
import fr.michaelm.util.text.algo.BKTree;
import fr.michaelm.util.text.algo.EditDistance;
import fr.michaelm.util.text.algo.DamarauLevenshteinDistance;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;

/**
 * A matcher based on the Damarau Levenshtein distance metric.
 * Damarau Leveshtein metric counts the number of edit operation necessary to
 * transform a string S1 into a String S2. An edit operation can be one of
 * "add a letter", "remove a letter", "change a letter" or "switch two 
 * contiguous letters". Note that the switch operation is counted as 2 distinct 
 * operations in the more classic Levenshtein distance. 
 *
 * @see fr.michaelm.jump.plugin.match.Matcher
 *
 * @author Michaël Michaud
 */
public class DamarauLevenshteinDistanceMatcher extends StringMatcher {
    
    private static final DamarauLevenshteinDistanceMatcher DAMARAU_LEVENSHTEIN =
        new DamarauLevenshteinDistanceMatcher("","", 2.0);
    
    public static DamarauLevenshteinDistanceMatcher instance() {
        return DAMARAU_LEVENSHTEIN;
    }

    public DamarauLevenshteinDistanceMatcher(String sourceAttribute, String targetAttribute, double max_dist) {
        super(sourceAttribute, targetAttribute);
        this.max_dist = max_dist;
    }
    
    /**
     * Returns 1.0 if source.equals(target), 0.0 if Levenstein distance
     * between source and target is more than max_dist.
     * @param source attribute to match from
     * @param target attribute to match to
     * @param context object containing useful information to check if
     * Attribute a effectively matches Attribute ref
     *
     * @return a double in the range [0-1] representative of the match quality
     * between a and ref. 
     *
     * @throws Exception if input data cannot be processed.
     */
     public double match(String source, String target, Object context) 
                                                              throws Exception {
         if (source == null || target == null) return 0.0;
         // fixed on 2013-07-30
         double distance = (double)DamarauLevenshteinDistance
                 .damarauLevenshtein(source, target, (int)max_dist+1);
         // Ex. if max_dist = 4
         // score for 5 = 0.0
         // score for 4 = 0.2 (1.0 - 4/5)
         // ...
         // score for 1 = 0.8 (1.0 - 1/5)
         // score for 0 = 1.0 (1.0 - 1/5)
         return distance > max_dist ? 0.0 : (1.0 - (distance/(max_dist+1)));
     }
     
     /**
     * Sets the maximum Levenshtein distance accepted between two strings.
     * @see #getMaximumDistance
     */
    public void setMaximumDistance(double max_dist) {
        this.max_dist = max_dist;
    }
    
    /**
     * Create an index following the Index interface.
     * This index does not accelearate queries as it returns the whole set for
     * any query.
     * @param features features to index
     * @throws TransformationException if the targetRule could not be applied
     *         to target features to build the index.
     */
    public Index createIndex(final Collection<Feature> features) 
                                                throws TransformationException {
        // As BKTree contains Strings, not Features, we build a normal index
        // along with the BKTree to retrieve features from attribute values
        final int limit = (int)max_dist;
        final BKTree tree = new BKTree(new EditDistance() {
            public int editDistance(String s, String t) {
                return DamarauLevenshteinDistance
                    .damarauLevenshtein(s, t, limit+1);
            }
        });
        final TreeMap<String,Set<Feature>> index = new TreeMap<>(collator);
        for (Feature f : features) {
            String value = targetRule.transform(f.getString(getTargetAttribute()));
            tree.add(value);
            Set<Feature> set = index.get(value);
            if (set == null) {
                set = new HashSet<>();
                index.put(value, set);
            }
            set.add(f);
        }
        return new Index() {
            public Set<Feature> query(Object value) {
                Set<Feature> candidates = new HashSet<>();
                // Get candidate strings from the BKTree
                HashMap<String,Integer> map = 
                    tree.query(value.toString(), (int)getMaximumDistance());
                // Get candidate features from the index
                for (String s : map.keySet()) {
                    candidates.addAll(index.get(s));
                }
                return candidates;
            }
        };
    }

}
