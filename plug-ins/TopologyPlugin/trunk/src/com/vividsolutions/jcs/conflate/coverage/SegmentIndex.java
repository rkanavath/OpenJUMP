package com.vividsolutions.jcs.conflate.coverage;

import java.util.*;
import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.index.SpatialIndex;

import com.vividsolutions.jcs.qa.FeatureSegment;

/**
 * An index of line segments.
 *
 * @author unascribed
 * @version 1.0
 */
public class SegmentIndex {
    
    private boolean built = false;

    private Set<FeatureSegment> segments = new HashSet<FeatureSegment>();

    public SegmentIndex() {}

    public void add(FeatureSegment segment) {
        segments.add(segment);
    }
    
    //public void build() {
    //    if (!built) {
    //        for (FeatureSegment segment : map.keySet()) {
    //            map.put(segment, segment.getMatches());
    //        }
    //    }
    //}
    
    //public Set<FeatureSegment> getMatches(FeatureSegment fs) {
    //    if (built) {
    //        return map.get(fs);
    //    } else return null;
    //}
    
    public boolean contains(FeatureSegment testSegment) {
        return segments.contains(testSegment);
    }
    
    //public Set<FeatureSegment> query(Envelope env) {
    //    return new HashSet(spatialIndex.query(env));
    //}
    
    public int size() {return segments.size();}
    
}