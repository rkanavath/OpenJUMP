/*
 * The JCS Conflation Suite (JCS) is a library of Java classes that
 * can be used to build automated or semi-automated conflation solutions.
 *
 * Copyright (C) 2003 Vivid Solutions
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
 * Vivid Solutions
 * Suite #1A
 * 2328 Government Street
 * Victoria BC  V8T 5G5
 * Canada
 *
 * (250)385-6040
 * www.vividsolutions.com
 */

package com.vividsolutions.jcs.qa;

import java.util.*;
import com.vividsolutions.jcs.conflate.boundarymatch.*;
import com.vividsolutions.jump.algorithm.VertexHausdorffDistance;
import com.vividsolutions.jump.feature.*;
import com.vividsolutions.jump.geom.*;
import com.vividsolutions.jump.util.CoordinateArrays;

import com.vividsolutions.jts.geom.*;
//import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.util.Assert;
import com.vividsolutions.jts.util.Debug;
import com.vividsolutions.jump.task.*;
import com.vividsolutions.jts.util.Debug;

//import com.vividsolutions.jcs.plugin.I18NPlug;
import fr.michaelm.jump.plugin.topology.I18NPlug;

/**
 * Finds line segments in a single
 * coverage which match but are not identical.
 * The coverage is represented as a FeatureCollection of area Geometry's.
 * <p>
 * Matched segments are usually indicative of gaps or overlaps within the coverage.
 * <p>
 * Duplicate segments
 * are removed before matches are tested, since only segments which are not
 * paired are of interest.
 */
public class InternalMatchedSegmentFinder {

    public Parameters getParam() {
        return param;
    }

    public static List createIndicatorList(LineSegment fs0, LineSegment fs1) {
        // this prevents creating duplicate indicators
        // if (fs0.getFeature().getID() <= fs1.getFeature().getID()) return;
        // create indicator showing size of match difference
        LineSegment projSeg1 = fs0.project(fs1);
        LineSegment projSeg2 = fs1.project(fs0);
        if (projSeg1.equalsTopo(projSeg2)) {
            return createEqualProjectionIndicators(fs0, fs1, projSeg1);
        }
        List indicatorList = new ArrayList();
        VertexHausdorffDistance hDist = new VertexHausdorffDistance(projSeg1, projSeg2);
        Coordinate[] coord = hDist.getCoordinates();
        Geometry overlapSizeInd = factory.createLineString(coord);
        indicatorList.add(overlapSizeInd);
        return indicatorList;
    }

    /**
     * If the projection of each segment is equal, the segments must be
     * parallel and overlapping.  In this case, the indicators
     * will simply be the endpoint(s) which is/are <b>not</b> shared.
     * @param fs0 a matching segment
     * @param fs1 a matching segment
     * @param projSeg the common projection segment
     */
    public static List createEqualProjectionIndicators(LineSegment fs0, LineSegment fs1, LineSegment projSeg) {
        List indicatorList = new ArrayList();
        for (int i = 0; i < 2; i++) {
            Coordinate projPt = projSeg.getCoordinate(i);
            boolean inFS0 = projPt.equals(fs0.p0) || projPt.equals(fs0.p1);
            boolean inFS1 = projPt.equals(fs1.p0) || projPt.equals(fs1.p1);
            boolean inBoth = inFS0 && inFS1;
            if (! inBoth) {
                Geometry overlapSizeInd = factory.createPoint(projPt);
                indicatorList.add(overlapSizeInd);
            }
        }
        return indicatorList;
    }

    public static class Parameters {
        public Parameters(){}
        public Parameters(double distanceTolerance, double angleTolerance) {
            this.distanceTolerance = distanceTolerance;
            this.angleTolerance = angleTolerance;
        }
        /**
         * The distance below which segments are considered to match
         */
        public double distanceTolerance = 1.0;
        
        /**
         * The maximum angle between matching segments.
         */
        public double angleTolerance = 22.5;
    }

    private static final GeometryFactory factory = new GeometryFactory();
    
    private Parameters param;
    private boolean createIndicators = true;
    
    private FeatureCollection inputFC;
    
    private boolean isComputed = false;
    
    private FeatureCollection matchedLinesFC = null;
    private FeatureCollection sizeIndicatorFC = null;
    
    // a list of FeatureSegments
    private List<FeatureSegment> matchedFeatureSegments = new ArrayList<FeatureSegment>();
    // a list of Geometry's
    private List<Geometry> matchedLines = new ArrayList();
    // a list of Geometry's
    private List<Geometry> sizeIndicators = new ArrayList();
    
    private SegmentMatcher segmentMatcher;
    private TaskMonitor monitor;
    private Set matchedFeatureSet = new TreeSet(new FeatureUtil.IDComparator());
    private List<FeatureSegment> uniqueFSList;
    
    // singleton objects created once for efficiency and reused as necessary
    private Envelope itemEnv = new Envelope();
    private SpatialIndex featureSegmentIndex = new STRtree();
    private Geometry fence = null;
    
    public InternalMatchedSegmentFinder(FeatureCollection inputFC, Parameters param) {
        this(inputFC, param, new DummyTaskMonitor());
    }
    
    public InternalMatchedSegmentFinder(FeatureCollection inputFC,
                                        Parameters param, TaskMonitor monitor) {
        this.inputFC = inputFC;
        this.param = param;
        this.monitor = monitor;
        // only check segs with opposite orientation, since features should not overlap in a coverage
        // (MM - this suppose that polygons and their holes are normalized)
        segmentMatcher = new SegmentMatcher(param.distanceTolerance,
                                            param.angleTolerance,
                                            SegmentMatcher.OPPOSITE_ORIENTATION);
    }

    /**
     * Indicates whether spatial indicators should be generated.
     * The default is true.
     * If indicators are not generated, nulls will
     * be returned from #getMatchedFeatures, #getMatchedSegments,
     * and #getSizeIndicators
     *
     * @param createIndicators
     */
    public void setCreateIndicators(boolean createIndicators) {
        this.createIndicators = createIndicators;
    }

    public void setFence(Envelope fenceEnv) {this.fence = factory.toGeometry(fenceEnv);}

    public void setFence(Geometry fence) {this.fence = fence;}

    /**
     * Gets the matched segments as Features.
     */
    public FeatureCollection getMatchedSegments() {
        computeMatches();
        return matchedLinesFC;
    }

    /**
     * Gets the original matched {@link FeatureSegment}s
     *
     * @return a list of FeatureSegments
     */
    public List<FeatureSegment> getMatchedFeatureSegments() {
        computeMatches();
        return matchedFeatureSegments;
    }

    public FeatureCollection getSizeIndicators() {
        computeMatches();
        return sizeIndicatorFC;
    }


    /**
     * Computes the set of {@link Feature}s which contain matched segments.
     *
     * @return a FeatureCollection of unique matched Features
     */
    public FeatureCollection getMatchedFeatures() {
        // make sure matched feature segments have been computed
        computeMatches();
        for (Iterator i = matchedFeatureSegments.iterator(); i.hasNext(); ) {
            FeatureSegment f = (FeatureSegment) i.next();
            matchedFeatureSet.add(f.getFeature());
        }
        FeatureCollection matchedFeatures = new FeatureDataset(matchedFeatureSet, inputFC.getFeatureSchema());
        return matchedFeatures;
    }

    /**
     * Computes the set of Features which contain matched segments.
     * @return a FeatureCollection of unique matched Features
     */
    public FeatureCollection getUniqueSegmentFeatures() {
        // make sure unique feature segments have been computed
        computeMatches();
        Set uniqueSegFeatSet = new HashSet();
        for (Iterator i = uniqueFSList.iterator(); i.hasNext(); ) {
            FeatureSegment f = (FeatureSegment) i.next();
            uniqueSegFeatSet.add(f.getFeature());
        }
        FeatureCollection uniqueSegFeaturesFC = new FeatureDataset(uniqueSegFeatSet, inputFC.getFeatureSchema());
        return uniqueSegFeaturesFC;
    }

    /**
     * Gets the features which have vertices which touch matched segment vertices
     *
     * @return a FeatureCollection
     */
    private FeatureCollection getAdjacentFeaturesToMatches() {
        //Collection matchedLineSegs = getMatchedLineSegments();
        Collection<FeatureSegment> matchedLineSegs = getMatchedFeatureSegments();
        Set adjFeatures = new HashSet();
        for (Iterator i = matchedLineSegs.iterator(); i.hasNext(); ) {
            LineSegment seg = (LineSegment) i.next();
            
            List featWithVertex0 = getFeaturesWithVertex(seg.p0);
            adjFeatures.addAll(featWithVertex0);
            
            List featWithVertex1 = getFeaturesWithVertex(seg.p1);
            adjFeatures.addAll(featWithVertex1);
        }
        FeatureCollection adjFeatFC = new FeatureDataset(adjFeatures, inputFC.getFeatureSchema());
        return adjFeatFC;
    }

    private List getFeaturesWithVertex(Coordinate pt) {
        // create an envelope to intersect any possible matching segments
        itemEnv.init(pt);
        List candidateSegments = featureSegmentIndex.query(itemEnv);
        List resultFeatures = new ArrayList();
        for (Iterator i = candidateSegments.iterator(); i.hasNext(); ) {
            FeatureSegment seg = (FeatureSegment) i.next();
            if (seg.p0.equals2D(pt) || seg.p1.equals2D(pt)) {
                resultFeatures.add(seg.getFeature());
            }
        }
        return resultFeatures;
    }

    public void computeMatches() {
        if (isComputed) return;
        isComputed = true;
        Debug.println("  1.1 - Get unique segments");
        uniqueFSList = getUniqueSegments();
        // it is only necessary to check unique segments to see if they match,
        // since non-unique segments by definition are already aligned.
        Debug.println("  1.2 - Create index");
        createIndex(uniqueFSList);
        // only unique segments will be flagged as matching
        // i.e. if a segment has a "partner" it is considered to be aligned and hence correct
        Debug.println("  1.3 - Find Matches");
        findMatches(uniqueFSList);
        if (createIndicators) {
            matchedLinesFC = FeatureDatasetFactory.createFromGeometry(matchedLines);
            sizeIndicatorFC = FeatureDatasetFactory.createFromGeometryWithLength(sizeIndicators, "LENGTH");
        }
    }

    private List<FeatureSegment> getUniqueSegments() {
        FeatureSegmentCounter fsc = new FeatureSegmentCounter(false, monitor);
        fsc.setFence(fence);
        fsc.add(inputFC);
        return fsc.getUniqueSegments();
    }

    /**
     * Create a spatial index for the FeatureSegments in the list
     *
     * @param fsList
     */
    private void createIndex(Collection<FeatureSegment> fsList) {
        monitor.allowCancellationRequests();
        monitor.report(I18NPlug.getI18N("qa.InternalMatchedSegmentFinder.creating-segment-index"));
        int totalSegments = fsList.size();
        int count = 0;
        for (Iterator i = fsList.iterator(); i.hasNext(); ) {
            FeatureSegment fs = (FeatureSegment) i.next();
            // ignore zero-length segments
            if (fs.p0.equals(fs.p1)) continue;
            itemEnv.init(fs.p0, fs.p1);
            monitor.report(++count, totalSegments, I18NPlug.getI18N("qa.InternalMatchedSegmentFinder.segments"));
            featureSegmentIndex.insert(new Envelope(itemEnv), fs);
        }
    }

    private void findMatches(List<FeatureSegment> queryFSList) {
        monitor.allowCancellationRequests();
        monitor.report(I18NPlug.getI18N("qa.InternalMatchedSegmentFinder.finding-segment-matches"));
        int totalSegments = queryFSList.size();
        for (int i = 0; i < totalSegments && !monitor.isCancelRequested(); i++) {
            FeatureSegment fs = queryFSList.get(i);
            findMatches(fs);
            monitor.report(i+1, totalSegments, I18NPlug.getI18N("qa.InternalMatchedSegmentFinder.segments"));
        }
    }

    /**
     * Find any segments that match the query segment.  The segment index is
     * used to speed up the performance.
     *
     * @param querySeg the candidate segment to be matched
     */
    private void findMatches(FeatureSegment querySeg) {
        // zero-length segments should not be matched
        // mmichaud change length computation by equality test between endpoints
        // double segmentLength = querySeg.p0.distance(querySeg.p1);
        // if (segmentLength <= 0.0) return;
        if (querySeg.p0.equals(querySeg.p1)) return;
        
        // create an envelope to intersect any possible matching segments
        itemEnv.init(querySeg.p0, querySeg.p1);
        Envelope queryEnv = EnvelopeUtil.expand(itemEnv, param.distanceTolerance);
        
        List candidateSegments = featureSegmentIndex.query(queryEnv);
        Debug.println("      - match " + querySeg + " - ");
        boolean hasMatch = checkMatches(querySeg, candidateSegments);
        if (hasMatch) {
            // save the matched segment
            matchedFeatureSegments.add(querySeg);
            if (createIndicators) {
                matchedLines.add(LineSegmentUtil.asGeometry(factory, querySeg));
            }
            
        }
    }

    /**
     * Check matches between a given FeatureSegment and a set of
     * candidate segments.
     * Segments from the same feature will not be reported as a match.
     *
     * @param fs the feature segment to check
     * @param candidateSegments the list of potential matching segments
     * @return <code>true</code> if a match is found
     */
    private boolean checkMatches(FeatureSegment fs, List candidateSegments) {
        boolean hasMatch = false;
        for (Iterator i = candidateSegments.iterator(); i.hasNext(); ) {
            FeatureSegment candidateFS = (FeatureSegment) i.next();
            // if segments are from same feature do not report them as a match
            if (candidateFS.getFeature() == fs.getFeature()) continue;
            
            boolean isEqual = fs.equalsTopo(candidateFS);
            if (isEqual) continue;
            // zero-length segments should not be matched
            // mmichaud : replace length calculation by equality test
            //double candidateLen = candidateFS.p0.distance(candidateFS.p1);
            boolean zeroLength = candidateFS.p0.equals(candidateFS.p1);
            if (zeroLength) continue;
            
            boolean isMatch = segmentMatcher.isMatch(fs, candidateFS);
            Debug.println("       -> " + candidateFS);
            if (isMatch && !isEqual && !zeroLength) {
                hasMatch = true;
                Debug.println(" MATCH !");
                //System.out.println("match : " + fs.getFeature().getID()+"|"+fs.toString() + " - " + candidateFS.getFeature().getID()+"|"+candidateFS.toString());
                //System.out.println("   checkMatch=true for " + fs + " / " + candidateFS);
                // save matched segments for future processing
                fs.addMatch(candidateFS);
                candidateFS.addMatch(fs);
                
                if (createIndicators) {
                // check for relative size of IDs to avoid creating duplicate indicators
                    if (fs.getFeature().getID() > candidateFS.getFeature().getID()) {
                      List indicators = createIndicatorList(fs, candidateFS);
                      sizeIndicators.addAll(indicators);
                    }
                }
            }
            else Debug.println("");
        }
        return hasMatch;
    }

    // testing only for now
    // I don't know why MD wanted to find triangular gaps (or overlaps)
    // It seems those three methods are unused [mmichaud 2010-02-01]
    public List findTriangleMatches() {
        List triangleFS = new ArrayList();
        for (Iterator i = matchedFeatureSegments.iterator(); i .hasNext(); ) {
            FeatureSegment fs = (FeatureSegment) i.next();
            if (isTriangleMatch(fs)) {
                triangleFS.add(fs);
                System.out.println(fs);
            }
        }
        return triangleFS;
    }

    private boolean isTriangleMatch(FeatureSegment fs) {
        Collection matches = fs.getMatches();
        if (matches.size() != 2) return false;
        Feature[] oppFeat = new Feature[2];
        int j = 0;
        for (Iterator i = matches.iterator(); i.hasNext(); ) {
            FeatureSegment fsMatch = (FeatureSegment) i.next();
            oppFeat[j++] = fsMatch.getFeature();
            if (! isOnlyMatch(fsMatch, fs)) return false;
        }
        //short segments must come from same feature
        if (oppFeat[0] != oppFeat[1]) return false;
        return true;
    }

    private boolean isOnlyMatch(FeatureSegment fs, FeatureSegment matchFS) {
        Collection matches = fs.getMatches();
        if (matches.size() != 1) return false;
        //Assert.isTrue(matches0.get(0) == fs);
        return true;
    }
}
