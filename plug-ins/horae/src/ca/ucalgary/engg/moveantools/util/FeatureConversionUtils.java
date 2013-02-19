package ca.ucalgary.engg.moveantools.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.openjump.core.geomutils.algorithm.IntersectGeometries;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;
import com.vividsolutions.jump.I18N;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.tools.AttributeMapping;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;
import com.vividsolutions.jump.workbench.ui.GenericNames;

/**
 * contains convenience methods for data conversions
 * @author sstein
 *
 */
public class FeatureConversionUtils {

	public static FeatureCollection calculatePolysFromContours( FeatureCollection contours, 
			TaskMonitor monitor, PlugInContext context) {
		 
		//-- calculate the intersections and use the Polygonizer
    	Collection<Geometry> lines = new ArrayList<Geometry>();
    	for (Iterator iterator = contours.iterator(); iterator.hasNext();) {
			Feature edge = (Feature) iterator.next();
    		lines.add(edge.getGeometry());
    	}
		Collection<Geometry>  nodedLines = IntersectGeometries.nodeLines(lines);
	    Polygonizer polygonizer = new Polygonizer();
	    for (Iterator i = nodedLines.iterator(); i.hasNext(); ) {
	        Geometry g = (Geometry) i.next();
	        polygonizer.add(g);
	      }
	    //-- get the Polygons
		Collection<Geometry> polys = polygonizer.getPolygons();
		//-- remove the inner ones since we need them
		Collection<Geometry> cleanedPolys = FeatureConversionUtils.removePolygonsInPolygons(polys);
		//Collection<Geometry> cleanedPolys = polys;
    	//-- transfer Attributes
		FeatureCollection resultD = FeatureConversionUtils.transferAttributesFromLinesToPolys(contours, cleanedPolys, context, monitor); 
		
		return resultD;
	}
	
	/**
	 * removes polygons within polygons (i.e. those that cover the holes)
	 * @param polys
	 * @return
	 */
	public static Collection<Geometry> removePolygonsInPolygons(Collection<Geometry> polys){
		Collection<Geometry> coll = new ArrayList();
		GeometryFactory gf = new GeometryFactory();
		int i = 0;
		for (Iterator iterator = polys.iterator(); iterator.hasNext();) {
			Geometry geom = (Geometry) iterator.next();
			//-- check if this geom is contained in any other (except itself)
			int j = 0; boolean contained = false;
			for (Iterator iterator2 = polys.iterator(); iterator2.hasNext();) {
				Polygon geomtoTest = (Polygon) iterator2.next();
				if( i != j){
					//-- test with the exterior ring transformed to a polygon
					LineString ls = geomtoTest.getExteriorRing();
					LinearRing lr = gf.createLinearRing(ls.getCoordinates());
					Polygon ringPolygon = gf.createPolygon(lr, null);
					if(ringPolygon.contains(geom)){
						contained = true;
						//System.out.println("geometry contained in other");
					}
				}
				j++;
			}
			if(contained == false){
				coll.add(geom);
			}
			i++;
		}
		
		return coll; 
	}
	
	/**
	 * transfers the attributes from lines to polygons using the second line point as geometric test object and the spatial predicate "contains".
	 * @param fcA
	 * @param geometries
	 * @param context
	 * @param monitor
	 * @return
	 */
	public static FeatureCollection transferAttributesFromLinesToPolys(FeatureCollection fcA, Collection<Geometry> geometries, PlugInContext context, TaskMonitor monitor){
		//-- check if the polygon has a correspondent 
		//	 if yes, transfer the attributes - if no: remove the polygon
		
		//-- build a tree for the existing layers first.
		SpatialIndex treeA = new STRtree();
		for (Iterator iterator = fcA.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			treeA.insert(f.getGeometry().getEnvelopeInternal(), f);
		}
		// -- get all intersecting features (usually there should be only one
		// corresponding feature per layer)
		// to avoid problems with spatial predicates we do the query for an
		// internal point of the result polygons
		// and apply an point in polygon test
		AttributeMapping mapping = new AttributeMapping(fcA.getFeatureSchema(),
				new FeatureSchema());
		// -- create the empty dataset with the final FeatureSchema
		FeatureDataset fd = new FeatureDataset(mapping.createSchema("Geometry"));
		// -- add the features and do the attribute mapping
		for (Iterator iterator = geometries.iterator(); iterator
				.hasNext();) {
			Geometry geom = (Geometry) iterator.next();
			//-- get a boundary point (i.e. the second one)
			Coordinate coord = geom.getCoordinates()[1];
			Point pt = new GeometryFactory().createPoint(coord);
			Feature f = new BasicFeature(fd.getFeatureSchema());
			Feature featureA = null;
			Feature featureB = null;
			// -- query Layer A ---
			List candidatesA = treeA.query(pt.getEnvelopeInternal());
			int foundCountA = 0;
			for (Iterator iterator2 = candidatesA.iterator(); iterator2.hasNext();){
				Feature ftemp = (Feature) iterator2.next();
				if (ftemp.getGeometry().contains(pt)) {
					foundCountA++;
					featureA = ftemp;
				}
			}
			if (foundCountA > 1) {
				if (context != null) {
					context.getWorkbenchFrame().warnUser(
							I18N.get("org.openjump.plugin.tools.IntersectPolygonLayersPlugIn.Found-more-than-one-source-feature-in-Layer")
							+ " " + GenericNames.LAYER_A);
				}
			} else if (foundCountA == 0) {
				if (context != null) {
					context.getWorkbenchFrame().warnUser("no corresponding feature in Layer A");
				}
			}
			if (foundCountA > 0){ 
				// -- do mapping
				mapping.transferAttributes(featureA, featureB, f);
				// -- set Geometry
				f.setGeometry((Geometry) geom.clone());
				fd.add(f);
			}
//			else{
//				System.out.println("polygon without correspondent"); 
//			}
	    	if (monitor != null){
	    		if (monitor.isCancelRequested()){
	    			return fd;
	    		}
	    	}
		}
		return fd;
	}

	/**
     * Creates a single convex cull out of all points delivered 
     * @param points
     * @return
     */
    public static Geometry createConvexHullFromFeatures(List<Feature> points) {
    	MultiPoint mp = null;
    	Coordinate[] coords = new Coordinate[points.size()];
    	int i = 0;
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if(f.getGeometry() instanceof Point){ 
				coords[i] = ((Point)f.getGeometry()).getCoordinate();
				i++;
			}
			else{
				System.out.println("createConvexHullFromFeatures: feature not a point, ID: " + f.getID());
			}
		}
		mp = new GeometryFactory().createMultiPoint(coords);
		Geometry hull = mp.convexHull();
		return hull;
	}
}
