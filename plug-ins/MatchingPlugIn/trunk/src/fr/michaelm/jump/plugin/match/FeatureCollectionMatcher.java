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

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.union.UnaryUnionOp;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.task.TaskMonitor;

import fr.michaelm.jump.plugin.match.matcher.*;
import fr.michaelm.util.text.Rule;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Matcher iterating through two FeatureCollection to find matching features.
 *
 * @author Michaël Michaud
 */
public class FeatureCollectionMatcher {
    
    private Collection<Feature> source;
    private Collection<Feature> target;
    private GeometryMatcher geometryMatcher;
    private StringMatcher attributeMatcher;
    private MatchMap matchMap;
    private TaskMonitor monitor;
    public boolean interrupted = false;
    
    // set n_m = true to try to match source features to several target 
    // features in one shot.
    private boolean n_m = false;
    private static final Matcher OVERLAP = OverlapsMatcher.instance();
    
    /**
     * A high level matcher able to compare features from two feature 
     * collections. It is able to compare pairs of features or to pre-process
     * the feature collection in order to find N-M matches.
     * @param source the source feature collection
     * @param target the target feature collection
     * @param geometryMatcher the Matcher to evaluate geometric similarity
     * @param attributeMatcher the matcher to evaluate semantic similarity
     */
    public FeatureCollectionMatcher(Collection<Feature> source,
                                    Collection<Feature> target,
                                    GeometryMatcher geometryMatcher,
                                    StringMatcher attributeMatcher,
                                    TaskMonitor monitor) {
        if (geometryMatcher == MatchAllMatcher.MATCH_ALL) {
            geometryMatcher = null;
        }
        if (attributeMatcher == MatchAllStringsMatcher.MATCH_ALL) {
            attributeMatcher = null;
        }
        assert geometryMatcher != null || attributeMatcher != null :
           "A FeatureCollectionMatcher must have at least one Matcher";
        this.source = source;
        this.target = target;
        this.geometryMatcher = geometryMatcher;
        this.attributeMatcher = attributeMatcher;
        this.monitor = monitor;
        matchMap = new MatchMap();
    }
    
    /**
     * Main method trying to match all features from both input feature
     * collections and returning the set of source features matching one or
     * several target features.
     * @param singleSource whether a target Feature can be matched by several
     * source features or not.
     * @param singleTarget whether a source feature can match several target 
     * features or not.
     */
    public Collection<Feature> matchAll(boolean singleSource, 
                                        boolean singleTarget) throws Exception {
        long t0 = System.currentTimeMillis();
        if (geometryMatcher != null) {
            System.out.println("Geometry Matching");
            monitor.report("Geometry matching");
            matchMap = geometryMatching(singleSource, singleTarget);
        }
        if (attributeMatcher != null) {
            System.out.println("Semantic Matching");
            monitor.report("Attribute matching");
            matchMap = attributeMatching(singleSource, singleTarget);
        }
        if (geometryMatcher == null && attributeMatcher == null) {
            throw new Exception("Invalid params (both geometric and attribute matchers are null !)");
        }
        //System.out.println("MatchMap before filter : \n" + matchMap.toString().replaceAll(",","\n"));
        monitor.report("Filtering results");
        matchMap = matchMap.filter(singleSource, singleTarget);
        //System.out.println("MatchMap after filter : \n" + matchMap.toString().replaceAll(",","\n"));
        System.out.println("Match performed in " + (System.currentTimeMillis()-t0) + " ms");
        return matchMap.getSourceFeatures();
    }
    
    public MatchMap getMatchMap() {
        return matchMap;
    }
    
    public void clearMatchMap() {
        matchMap.clear();
    }
    
    
    /**
     * Returns a MatchMap representing all the match scores obtained by 
     * comparing source feature geometries with target feature geometries with
     * the GeometryMatcher.
     * @param singleSource whether a target Feature can be matched by several
     * source features or not.
     * @param singleTarget whether a source feature can match several target 
     * features or not.
     */
    public MatchMap geometryMatching(boolean singleSource, boolean singleTarget) throws Exception {
        double maxDistance = geometryMatcher.getMaximumDistance();
        if (Double.isNaN(maxDistance)) maxDistance = 0.0;
        //System.out.println("Geometry Matching " + geometryMatcher + " " + maxDistance);
        long t0 = System.currentTimeMillis();
        double minOverlapping = geometryMatcher.getMinimumOverlapping();
        //System.out.println("geometryMatcher.minOverlapping = " + minOverlapping);
        monitor.report("Geometry matching : indexing features");
        STRtree index = indexFeatureCollection(target);
        // For each feature of the source collection
        monitor.report("Geometry matching : matching feature geometries");
        int countf1 = 0;
        int total = source.size();
        for (Feature f1 : source) {
            //System.out.println("Feature " + f1.getID());
            Geometry g1 = f1.getGeometry();
            Envelope env = new Envelope(g1.getEnvelopeInternal());
            env.expandBy(maxDistance);
            List<Feature> candidates = index.query(env);
            // if matching_layer = reference_layer don't try to match f1 with itself
            candidates.remove(f1);
            // This loop can select several target features for one source
            // feature, a singleTarget filter must be applied afterwards
            int countf2 = 0;
            // if multiple targets are authorized, a oneOneMatches map is built
            // during the one-to-one match phase in order to be used and optimize
            // the phase where we try to match source with union of candidates. 
            Map<Feature,Match> oneOneMatches = null;
            if (!singleTarget) oneOneMatches = new HashMap<Feature,Match>();
            for (Feature f2 : candidates) {
                double score = geometryMatcher.match(f1, f2, null);
                if (score > 0.0) {
                    Match match = new Match(f1, f2, score);
                    matchMap.add(match);
                    if (!singleTarget) oneOneMatches.put(f2, match);
                    countf2++;
                }
            }
            
            // If one source can match multiple target 
            // and several target candidates are available
            // and some candidates have not been individually matched
            if (!singleTarget && candidates.size() > 1 && !(countf2 == candidates.size())) {
                Geometry globalTarget = union(candidates);
                // if g1 matches the union of candidates, we try to attribute 
                // a score to each g1/candidate pair
                if (geometryMatcher.match(g1, globalTarget, null) > 0) {
                    Geometry g1Buffer = g1.buffer(maxDistance, 4);
                    // if g1 matches union of g2, we put all g1/g2 matches 
                    // in a temporary structure ordered by match scores
                    Set<Match> partialMatches = new TreeSet<Match>();
                    for (Feature f2 : candidates) {
                        Geometry g2Buffer = f2.getGeometry().buffer(maxDistance, 4);
                        Geometry intersection = g1Buffer.intersection(g2Buffer);
                        if (intersection.isEmpty()) continue;
                        double ratio1 = intersection.getArea()/g1Buffer.getArea();
                        double ratio2 = intersection.getArea()/g2Buffer.getArea();
                        if (ratio1 > 0.01) {
                            // we set the ratio of the temporary match to the
                            // max of ratio1 and ratio 2 (match is good if f1
                            // buffer covers a lrage part of f2 or if f2 buffer
                            // covers a large part of f1
                            partialMatches.add(new Match(f1, f2, Math.max(ratio1, ratio2)));
                        }
                    }
                    int countPartialMatches = 0;
                    // Test temporary matches from the best score to the worst,
                    // and add them to the final matchMap until f1 is completely 
                    // covered by f2 buffers
                    SortedSet<Match> previousMatches = matchMap.getMatchesForSourceFeature(f1);
                    for (Match match : partialMatches) {
                        Match oneOneMatch = oneOneMatches.get(match.getTarget());
                        if (oneOneMatch != null) {
                            if (oneOneMatch.getScore() > match.getScore()) {
                                continue;
                            }
                        }
                        // add at least one match
                        if (0 == countPartialMatches) {
                            if (oneOneMatch != null) matchMap.removeMatch(oneOneMatch);
                            matchMap.add(match);                            
                        }
                        else {
                            // substract candidate buffer from f1
                            Geometry diff = homogeneousDifference(g1, match.getTarget().getGeometry().buffer(maxDistance, 4));
                            // Add the match if the diff operation modified original geometry
                            if (!diff.equals(g1)) {
                                matchMap.add(match);
                            }
                            // break if f1 is completely covered by candidate buffers
                            if (diff.isEmpty()) break;
                            else g1 = diff;
                        }
                        countPartialMatches++;
                    }
                }
            }
            if (monitor.isCancelRequested()) {
                interrupted = true;
                return matchMap;
            };
            monitor.report(++countf1, total, "features");
        }
        System.out.println("Direct Geometry Matching done in " + (System.currentTimeMillis()-t0) + " ms");
        return matchMap;
    }
    
    private Geometry homogeneousDifference(Geometry g1, Geometry g2) {
        Geometry g = g1.difference(g2);
        if (g.isEmpty()) return g;
        else if (g.getNumGeometries() == 1) return g;
        else if (g.getDimension() < g1.getDimension()) {
            if (g1.getDimension() == 0) return g1.getFactory().createPoint((Coordinate)null);
            if (g1.getDimension() == 1) return g1.getFactory().createLineString(new Coordinate[0]);
            if (g1.getDimension() == 2) return g1.getFactory().createPolygon(g1.getFactory().createLinearRing(new Coordinate[0]), null);
        }
        else {
            List<Geometry> list = new ArrayList<Geometry>();
            for (int i = 0 ; i < g.getNumGeometries() ; i++) {
                if (g.getGeometryN(i).getDimension() == g1.getDimension()) {
                    list.add(g.getGeometryN(i));
                }
            }
            return g1.getFactory().buildGeometry(list);
        }
        return g;
    }
    
    private STRtree indexFeatureCollection(Collection<Feature> collection) {
        STRtree index = new STRtree();
        for (Feature f : collection) {
            index.insert(f.getGeometry().getEnvelopeInternal(), f);
        }
        return index;
    }
    
    private SortedMap<String,Collection<Feature>> indexFeatureCollection(Collection<Feature> collection, String attribute) {
        SortedMap<String,Collection<Feature>> map = new TreeMap<String,Collection<Feature>>();
        for (Feature f : collection) {
            String value = f.getString(attribute);
            Collection coll = map.get(value);
            if (coll == null) {
                coll = new ArrayList<Feature>();
                map.put(value, coll);
            }
            coll.add(f);
        }
        return map;
    }
    
    private Geometry union(List<Feature> features) {
        List geom = new ArrayList();
        for (Feature f : features) geom.add(f.getGeometry());
        return UnaryUnionOp.union(geom);
    }
    
    private MatchMap attributeMatching(boolean singleSource, boolean singleTarget) throws Exception {
        String sourceAttribute = attributeMatcher.getSourceAttribute();
        String targetAttribute = attributeMatcher.getTargetAttribute();
        Rule sourceRule = attributeMatcher.getSourceRule();
        Rule targetRule = attributeMatcher.getTargetRule();
        // If geometryMatcher is null, a simple join will be done.
        if (geometryMatcher == null && attributeMatcher != null) {
            monitor.report("Attribute matching : indexing features");
            Index index = attributeMatcher.createIndex(target);
            int count = 0;
            int total = source.size();
            monitor.report("Attribute matching : matching feature attributes");
            for (Feature f1 : source) {
                String sourceValue = sourceRule.transform(f1.getString(sourceAttribute));
                //System.out.println("sourceValue : " + sourceValue);
                Set<Feature> candidates = 
                    index.query(sourceValue);
                if (candidates == null || candidates.isEmpty()) {
                    continue;
                }
                else if (Double.isNaN(attributeMatcher.getMaximumDistance())) {
                    for (Feature f2 : candidates) {
                        matchMap.add(new Match(f1, f2, 1.0));
                    }
                }
                // In the case where a BKTree is used, there is room for 
                // optimizition because distances are already computed by the
                // BKTree query method
                else {
                    for (Feature f2 : candidates) {
                        double d = attributeMatcher.match(f1, f2, null);
                        matchMap.add(new Match(f1, f2, d));
                    }
                }
                if (monitor.isCancelRequested()) {
                    interrupted = true;
                    return matchMap;
                };
                monitor.report(++count, total, "features");
            }            
            // index attribute data
        }
        // If a geometry matching has already been done, attribute matching
        // use the resulting MatchMap from the geometry matching process 
        else {
            List<Match> new_matches = new ArrayList<Match>();
            Set<Match> allMatches = matchMap.getAllMatches();
            int count = 0;
            int total = allMatches.size();
            for (Match m : allMatches) {
                String srcA = sourceRule.transform(m.getSource().getString(sourceAttribute));
                String tgtA = targetRule.transform(m.getTarget().getString(targetAttribute));
                double newScore = m.combineScore(attributeMatcher.match(srcA, tgtA, null));
                if (newScore > 0.0) {
                    new_matches.add(new Match(m.getSource(), m.getTarget(), newScore));
                }
                if (monitor.isCancelRequested()) {
                    interrupted = true;
                    return matchMap;
                };
                monitor.report(++count, total, "matches");
            }
            matchMap.clear();
            for (Match match : new_matches) {
                matchMap.add(match);
            }

        }
        return matchMap;
    }

}
