package ca.ucalgary.engg.moveantools.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;
import org.openjump.core.geomutils.algorithm.GeometryConverter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.union.UnaryUnionOp;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.feature.FeatureUtil;

public class LoCoHUtil {

    /**
     * creates the isopleths, i.e. polygons that cover a certain percentage of points
     * @param hulls containing the convex hulls as features that must have a "density" attribute
     * @param percentageOfPoints 0..100
     * @param param
     * @param featureSchema
     * @return
     */
    public static ArrayList<Feature> getIsoplethsByDensity(ArrayList<Feature> hulls, int percentageOfPoints, double param, 
    		int numTotalPoints, STRtree tree, FeatureSchema featureSchema){
    	ArrayList<Feature> regions = new ArrayList<Feature>();
    	ArrayList<Geometry> resgeoms = new ArrayList<Geometry>();
    	double percentage = 0;
    	if(percentageOfPoints == 100){
        	//-- union all hulls
        	ArrayList<Geometry> unionHulls100 = calcUnion(FeatureUtil.toGeometries(hulls), true); 
        	resgeoms.addAll(unionHulls100);
        	percentage = 100;
    	}
    	else{
    		//-- sort the hulls by density of points they contain (i.e. density)
    		List sortedHullList = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(hulls, "density");
    		//-- aggregate from highest = highest density index on until we reach
    		int maxIndex = sortedHullList.size() - 1;
    		int numPointsNeeded = (int)Math.floor(percentageOfPoints / 100.0 * numTotalPoints);
    		int numPtsContained = 0; int runIndex = 0;
    		ArrayList<Geometry> tempGeoms = new ArrayList<Geometry>();
    		boolean goon = true;
    		while(goon){
    			tempGeoms.add(((Feature)sortedHullList.get(maxIndex-runIndex)).getGeometry());
    			ArrayList<Geometry> unionHullGeoms = calcUnion(tempGeoms, true);
    			//--
    			numPtsContained = checkNumPointsCovered(unionHullGeoms, tree);
    			//System.out.println("numPointsContained: " + numPtsContained + " - percent: " + numPtsContained /  (numTotalPoints /100.0) );
    			//--
    			runIndex = runIndex+1;
    			//-- store if we finish
    			if(numPtsContained >= numPointsNeeded){
    				goon = false;
    				resgeoms.addAll(unionHullGeoms);
    				percentage = numPtsContained /  (numTotalPoints /100.0);
    			}
    			else{
    				goon = true;
    			}
    			if((maxIndex-runIndex) < 0){ //we can only use the hulls we have
    				goon = false;
    				resgeoms.addAll(unionHullGeoms);
    				percentage = numPtsContained /  (numTotalPoints /100.0);
    			}
    		}
    	}
		//-- create Features
		for (Iterator iterator = resgeoms.iterator(); iterator.hasNext();) {
			Geometry gtemp = (Geometry) iterator.next();
			Feature ftemp = new BasicFeature(featureSchema);
			ftemp.setGeometry(gtemp);
			ftemp.setAttribute("percent-cover-hull", new Double(percentage));
			ftemp.setAttribute("param", new Double(param));
			regions.add(ftemp);
		}
    	return regions;
    }

    /**
     * calculates the union of a collection of geometries
     * @param geoms
     * @param explode into single geometries afterwards?
     * @return
     */
	public static ArrayList<Geometry> calcUnion(Collection<Geometry> geoms, boolean explode){
		Geometry g = UnaryUnionOp.union(geoms);
		ArrayList<Geometry> unionRes = new ArrayList<Geometry>();
		unionRes.add(g);
        ArrayList<Geometry> resgeoms = new ArrayList<Geometry>();
    	//-- explode multigeoms
		if (explode){
	        for (Iterator iterator = unionRes.iterator(); iterator.hasNext();) {
				Geometry gt = (Geometry) iterator.next();
				ArrayList<Geometry> expGeom = GeometryConverter.explodeGeomsIfMultiG(gt);
				resgeoms.addAll(expGeom);
	        }
		}
		else{
			resgeoms = unionRes;
		}
        return resgeoms;
    }
    
	/**
	 * calculates how many points are covered by the hull polygons (using geom.intersects(geom2))
	 * @param unionHullGeoms
	 * @param tree
	 * @return
	 */
    public static int checkNumPointsCovered(
			ArrayList<Geometry> unionHullGeoms, STRtree tree) {
    	int numPointsCovered = 0;
    	for (Iterator iterator = unionHullGeoms.iterator(); iterator.hasNext();) {
			Geometry hullPoly = (Geometry) iterator.next();
			List candidates = tree.query(hullPoly.getEnvelopeInternal());
			for (Iterator iterator2 = candidates.iterator(); iterator2.hasNext();) {
				Feature pt = (Feature) iterator2.next();
				if (pt.getGeometry().intersects(hullPoly)){
					numPointsCovered = numPointsCovered + 1;
				}
			}
		}
		return numPointsCovered;
	}
    
    /**
     * Retrieve the k-nearest points for the input point, based on querying the tree.
     * We need an initial distance to determine the size of the search envelope. If
     * too few points are in the (initial) search envelope, then the envelope is grown. 
     * @param ptf
     * @param tree
     * @param k
     * @param startingDist, i.e. size of the initial search envelope
     * @return
     */
	public static ArrayList<Feature> getKNearestPoints(Feature ptf,
			STRtree tree, int k, double startingDist) {
		
		Geometry ptGeom = ptf.getGeometry();
		ArrayList<Feature> kpoints = new ArrayList<Feature>();
		
		//-- buffer, i.e. create circle,  and get the number of points in the buffer
		Geometry circle = ptGeom.buffer(startingDist);
		List candidates = tree.query(circle.getEnvelopeInternal()); // note, the list of candidates will contain the point itself too
		int numPoints = candidates.size(); 
		int minNumOfPointsInEnvelope = (int)Math.floor(1.5*(k+1)); // get 1.5 times k points since we operate with a squared envelope
																   // this should reduce the bias introduced by points in the envelope corners
		if (numPoints < minNumOfPointsInEnvelope){
			double circleRadius = startingDist;
			while(numPoints < minNumOfPointsInEnvelope){
				//-- enlarge
				circleRadius = 2*circleRadius; //this way the radius will be doubled every time
				circle = ptGeom.buffer(circleRadius);
				candidates = tree.query(circle.getEnvelopeInternal());
				numPoints = candidates.size();
			}
		}
		//-- now we have more points than we need. We can calculate the distance to all and get the first k. 
		//   we create new features for sorting later
		FeatureSchema newFsDist = FeatureSchemaTools.copyFeatureSchema(ptf.getSchema());
		newFsDist.addAttribute("dist", AttributeType.DOUBLE);
		ArrayList<Feature> points = new ArrayList<Feature>();
		for (Iterator iterator = candidates.iterator(); iterator.hasNext();) {
			Feature tempF = (Feature) iterator.next();
			double distance = tempF.getGeometry().distance(ptGeom);
			if (distance > 0){ //avoid copying itself
				Feature fnew = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(tempF, newFsDist);
				fnew.setAttribute("dist", new Double(distance));
				points.add(fnew);
			}
		}
		//-- sort the features by distance
		List sortedFeaturesList = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(points, "dist");
		//-- add the points, begin with closest 
		Iterator iterator = sortedFeaturesList.iterator();
		boolean goon = iterator.hasNext(); int i = 0;
		while (goon) {
			i=i+1;
			Feature ft = (Feature) iterator.next();
			kpoints.add(ft);
			goon = iterator.hasNext();
			//-- stop when we have all (k-1) points
			if (i == (k-1)){
				goon = false;
			}
		}
		return kpoints;
	}
 
	/**
     * Retrieve the alpha-nearest points for the input point, based on querying the tree.
     * Alpha is the sum of all distances of n nearest points. This basically creates
     * a circle of adaptive size for the LoCoH approach (i.e. LoCoH-a).
     * We need an initial distance to determine the size of the search envelope. If
     * too few points are in the (initial) search envelope, then the envelope is grown. 
     * @param ptf
     * @param tree
     * @param alphaDistance
     * @param startingDist, i.e. size of the initial search envelope
     * @return
     */
	public static ArrayList<Feature> getAlphaNearestPoints(Feature ptf,
			STRtree tree, double alphaDistance, double startingDist) {
		
		Geometry ptGeom = ptf.getGeometry();
		ArrayList<Feature> npoints = new ArrayList<Feature>();
		
		//-- buffer, i.e. create circle,  and get the points in the buffer
		Geometry circle = ptGeom.buffer(startingDist);
		List candidates = tree.query(circle.getEnvelopeInternal()); // note, the list of candidates will contain the point itself too
		double curDistance = getSumDistanceToRootpoint(ptf, candidates); 
		if (curDistance < alphaDistance){
			double circleRadius = startingDist;
			while(curDistance < alphaDistance){
				//-- enlarge
				circleRadius = 2*circleRadius; //this way the radius will be doubled every time
				circle = ptGeom.buffer(circleRadius);
				candidates = tree.query(circle.getEnvelopeInternal());
				curDistance = getSumDistanceToRootpoint(ptf, candidates);
			}
		}
		//-- now we have more points than we need. We can calculate the distance to all and get
		//   as may as needed starting with the closest point
		//-- we create new features for sorting later
		FeatureSchema newFsDist = FeatureSchemaTools.copyFeatureSchema(ptf.getSchema());
		newFsDist.addAttribute("dist", AttributeType.DOUBLE);
		ArrayList<Feature> points = new ArrayList<Feature>();
		for (Iterator iterator = candidates.iterator(); iterator.hasNext();) {
			Feature tempF = (Feature) iterator.next();
			double distance = tempF.getGeometry().distance(ptGeom);
			if (distance > 0){ //avoid copying itself
				Feature fnew = FeatureCollectionTools.copyFeatureAndSetFeatureSchema(tempF, newFsDist);
				fnew.setAttribute("dist", new Double(distance));
				points.add(fnew);
			}
		}
		//-- sort the features by distance
		List sortedFeaturesList = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(points, "dist");
		//-- add the points, begin with closest 
		Iterator iterator = sortedFeaturesList.iterator();
		boolean goon = iterator.hasNext(); int i = 0;
		double sumDist = 0;
		while (goon) {
			i=i+1;
			Feature ft = (Feature) iterator.next();
			sumDist = sumDist + (Double)ft.getAttribute("dist");
			if(sumDist <= alphaDistance){
				npoints.add(ft);
				goon = iterator.hasNext();
			}
			else{
				goon = false;
			}
		}
		return npoints;
	}

	/**
	 * Calculates the sum distance. Note, this function operates on points.
	 * @param rootPoint
	 * @param candidates
	 * @return
	 */
	public static double getSumDistanceToRootpoint(Feature rootPoint, List<Feature> pointFeatureList) {
		double sumDistance = 0;
		Geometry rootg = rootPoint.getGeometry();
		for (Iterator iterator = pointFeatureList.iterator(); iterator.hasNext();) {
			Feature feature = (Feature) iterator.next();
			double dist = rootg.distance(feature.getGeometry());
			sumDistance = sumDistance +	dist;		
		}
		return sumDistance;
	}
	
    /**
     * Used to calculated the initial size of the search envelope for k nearest neighbors.
     * @param points
     * @return
     */
    public static double calcSmallestDistanceOfFirstFiveFeatures(FeatureCollection points){
    	Iterator it = points.iterator(); boolean goon = it.hasNext();
    	int cnt = 0; 
    	double smallestDistance = 0; int distCount = 0; 
    	Geometry firstpoint = null;
    	while (it.hasNext()) {
			Feature f = (Feature) it.next();
			if (cnt == 0){
				//don't do anything but assign
				firstpoint = f.getGeometry();
			}
			else{
				//calculate a distance
				double dist = firstpoint.distance(f.getGeometry());
				if (dist > 0){
					if(distCount == 0){
						smallestDistance = dist;
					}
					else{
						if(dist<smallestDistance){
							smallestDistance = dist;
						}
					}
					distCount = distCount + 1;
				}
			}
			goon = it.hasNext();
			if (cnt == 5){ // stop after 5 points, we only want to use the first
				goon = false;
			}
			cnt = cnt + 1;
		}
    	return smallestDistance;   	
    }
	
}
