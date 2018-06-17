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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A Map accumulating information about matches between two sets of features.
 *
 * The MatchMap will store every single Match in a Tree Map ordering all 
 * possible matches from the best score to the worst score. For matches
 * returning the same score, ordering is determined by the 
 * {@link Match#compareTo(Match other)} method.
 *
 * @author Michaël Michaud
 */
public class MatchMap {
    
    private final SortedSet<Match> EMPTY_SET = Collections.unmodifiableSortedSet(new TreeSet<Match>());
        
    private final Map<Feature,TreeSet<Match>> sourceMap = new HashMap<Feature,TreeSet<Match>>();
    private final Map<Feature,TreeSet<Match>> targetMap = new HashMap<Feature,TreeSet<Match>>();
    
    //TODO : to improve performance, instead of maintaining 3 ordered map during 
    // the feeding (at each add call), sort the map on demand, keeping track of 
    // the ordering state (sorted after a get* or a filter call, unsorted after 
    // a add call.
    boolean sorted;

    /**
     * Construct a new MatchMap.
     */
    public MatchMap() {}
    
    /**
     * Add a match to this MatchMap.
     * In version 0.6.0+, we assume that feature 1 and feature 2 can have have 
     * one match only (so that the test to keep the best match only is removed) 
     */
    public void add(Match m) {
        TreeSet<Match> set = sourceMap.get(m.getSource());
        if (set == null) {
            set = new TreeSet<>();
            sourceMap.put(m.getSource(), set);
        }
        set.add(m);
        set = targetMap.get(m.getTarget());
        if (set == null) {
            set = new TreeSet<>();
            targetMap.put(m.getTarget(), set);
        }
        set.add(m);
    }
    
    /**
     * Get the whole match Set.
     */
    public Set<Match> getAllMatches() {
        Set<Match> matches = new HashSet<Match>();
        for (Feature feature : sourceMap.keySet()) matches.addAll(sourceMap.get(feature));
        return matches;
    }
    
    /**
     * Get the set of features matching one or more features.
     */
    public Set<Feature> getSourceFeatures() {
        return sourceMap.keySet();
    }
    
    /**
     * Get the set of features being matched by one or more features.
     */
    public Set<Feature> getTargetFeatures() {
        return targetMap.keySet();
    }
    
    /**
     * Get Matches recorded for this source Feature.
     */
    public SortedSet<Match> getMatchesForSourceFeature(Feature f) {
        SortedSet<Match> matches = sourceMap.get(f);
        return matches == null ? EMPTY_SET : matches;
    }
    
    /**
     * Get Matches recorded for this target Feature.
     */
    public SortedSet<Match> getMatchesForTargetFeature(Feature f) {
        SortedSet<Match> matches = targetMap.get(f);
        return matches == null ? EMPTY_SET : matches;
    }
    
    /**
     * Get Features matching source Feature f.
     */
    public List<Feature> getMatchedFeaturesFromSource(Feature f) {
        TreeSet<Match> matchedFeatures = sourceMap.get(f);
        List<Feature> list = new ArrayList<>();
        if (matchedFeatures == null) return list;
        for (Match m : matchedFeatures) {
            list.add(m.getTarget());
        }
        return list;
    }
    
    /**
     * Get Features matching target Feature f.
     */
    public List<Feature> getMatchedFeaturesFromTarget(Feature f) {
        TreeSet<Match> matchedFeatures = targetMap.get(f);
        List<Feature> list = new ArrayList<>();
        if (matchedFeatures == null) return list;
        for (Match m : matchedFeatures) {
            list.add(m.getSource());
        }
        return list;
    }
    
    /**
     * Return Match from source to target. Usually, the result contains 0
     * or 1 Match, but nothing prevent insertion of several matches per couple
     * of features.
     */
    public SortedSet<Match> getMatches(Feature source, Feature target) {
        // Set of matches from f1
        TreeSet<Match> set1 = sourceMap.get(source);
        // Set of matches to f2
        TreeSet<Match> set2 = targetMap.get(target);
        // Intersection of both sets = Match:f1->f2
        if (set1 != null && set2 != null) {
            SortedSet<Match> set = (TreeSet<Match>)set1.clone();
            set.retainAll(set2);
            return set;
        }
        else return EMPTY_SET;
    }
    
    private boolean removeMatchesForSourceFeature(Feature f) {
        TreeSet<Match> set = sourceMap.remove(f);
        return /*matches.removeAll(set)*/ true;
    }
    
    private boolean removeMatchesForTargetFeature(Feature f) {
        TreeSet<Match> set = targetMap.remove(f);
        return /*matches.removeAll(set)*/ true;
    }
    
    /**
     * Remove a match from the map.
     */
    public void removeMatch(Match m, boolean singleSource, boolean singleTarget) {
        if (singleTarget) removeMatchesForSourceFeature(m.getSource());
        if (singleSource) removeMatchesForTargetFeature(m.getTarget());
    }
    
    /**
     * Remove a match from the map.
     */
    public void removeMatch(Match m) {
        // remove match from sourceMap
        sourceMap.get(m.getSource()).remove(m);
        // if sourceMap has no more match for this source, remove source feature
        if (sourceMap.get(m.getSource()).size() == 0) sourceMap.remove(m.getSource());
        // remove match from targetMap
        targetMap.get(m.getTarget()).remove(m);
        // if targetMap has no more match for this target, remove target feature
        if (targetMap.get(m.getTarget()).size() == 0) targetMap.remove(m.getTarget());
    }
    
    /**
     * Filter the matchMap so that each source feature has only one target match
     * and/or each target feature has only one source match.
     */
    public MatchMap filter(boolean singleSource, boolean singleTarget) {
        if (!singleSource && !singleTarget) return this;
        //TreeSet<Match> filteredMatches = new TreeSet<Match>();
        // new code
        MatchMap matchMap = new MatchMap();
        TreeSet<Match> matches = new TreeSet<Match>();
        for (Feature feature : sourceMap.keySet()) matches.addAll(sourceMap.get(feature));
        for (Match match : matches) {
            Feature source = match.getSource();
            Feature target = match.getTarget();
            // Check if matchMap already has target features for this source
            SortedSet<Match> matchesForSource = matchMap.getMatchesForSourceFeature(source);
            // Check if matchMap already has source features for this target
            SortedSet<Match> matchesForTarget = matchMap.getMatchesForTargetFeature(target);
            if (singleTarget && matchesForSource.size() > 0) continue;
            else if (singleSource && matchesForTarget.size() > 0) continue;
            else matchMap.add(match);
        }
        return matchMap;
    }
    
    public void clear() {
        sourceMap.clear();
        targetMap.clear();
    }
    
}
