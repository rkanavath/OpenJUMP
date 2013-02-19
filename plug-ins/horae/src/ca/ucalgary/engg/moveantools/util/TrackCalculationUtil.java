package ca.ucalgary.engg.moveantools.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.openjump.core.apitools.FeatureCollectionTools;
import org.openjump.core.apitools.FeatureSchemaTools;

import ca.ucalgary.engg.moveantools.util.geom.OrientationMBR;
import ca.ucalgary.engg.moveantools.util.geom.PointLineDistance;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.distance.DistanceOp;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.task.TaskMonitor;
import com.vividsolutions.jump.workbench.model.StandardCategoryNames;
import com.vividsolutions.jump.workbench.plugin.PlugInContext;

public class TrackCalculationUtil {

	public final static String TIME_DIFFERENCE_ATTRIBUTE_NAME = "timeDiff";
	
	/**
	 * creates one value out of three given values of the sort YYYYMMDD
	 * @param f
	 * @param yearAttributeName
	 * @param monthAttributeName
	 * @param dayAttributeName
	 * @return
	 */
	public static double getTime(Feature f, String yearAttributeName, String monthAttributeName, String dayAttributeName){
		double time = -1;
		double year = FeatureCollectionTools.getNumericalAttributeValue(f, yearAttributeName);
		double month = FeatureCollectionTools.getNumericalAttributeValue(f, monthAttributeName);
		double day = FeatureCollectionTools.getNumericalAttributeValue(f, dayAttributeName);
		if (year == Double.NaN) {year = 0;};
		if (month == Double.NaN) {month = 0;};
		if (day == Double.NaN) {day = 0;};
		//-- sum up time value 
		time = day + (month *100) + (year*10000);
		return time;
	}
	
    /**
     * Sorts a list of features according to the values of the time attributes. If values are similar the feature
     * are ordered the same way as in the input list.
     * TODO: this method has been tested only briefly (not exhaustively)
     * @param features
     * @param timeAttributeNamesForSorting, attribute needs to be either Integer or Double 
     * @return list of sorted features; the smallest value will be first in the list.
     */
    public static ArrayList<Feature> sortFeatureListByTimeBeginWithSmallest(
			List<Feature> features, String[] timeAttributeNamesForSorting) {
    	ArrayList<Feature> sortedFeatureList = new ArrayList<Feature>();
    	ArrayList<Double> sortedValueList = new ArrayList<Double>(); // used to speed up sorting
    	int i = 0;
    	boolean first = true; 
    	for (Iterator iterator = features.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if(first){
				//-- just add the first feature
				sortedFeatureList.add(f.clone(true));
				double valuef = TrackCalculationUtil.getTime(f, timeAttributeNamesForSorting[0], timeAttributeNamesForSorting[1], timeAttributeNamesForSorting[2]);
				sortedValueList.add(valuef);
				first = false;
			}
			else{
				//-- get value
				double valuef = TrackCalculationUtil.getTime(f, timeAttributeNamesForSorting[0], timeAttributeNamesForSorting[1], timeAttributeNamesForSorting[2]);
				//-- parse the existing list
				boolean isLarger = true;
				int j = 0;
				while(isLarger){
					//-- to speed up things (i.e. avoid cumbersome value derivation), use the sortedValueList instead
					//Feature ftemp = sortedFeatureList.get(j);
					//double valueFtemp = TrackCalculationUtil.getTime(ftemp, timeAttributeNamesForSorting[0], timeAttributeNamesForSorting[1], timeAttributeNamesForSorting[2]);
					double valueFtemp = sortedValueList.get(j);
					if(valuef >= valueFtemp){
						//-- everything is fine, keep searching
					}
					else{
						//-- valuef is now smaller, squeeze it in right before ftemp (i.e. the same position)
						sortedFeatureList.add(j,f.clone(true));
						sortedValueList.add(j,valuef);
						//-- end the search, and sort the next item from "features"
						isLarger = false;
					}
					//-- if we are already at the end, then we need to insert the feature too
					if ( j+1 == sortedFeatureList.size()){
						sortedFeatureList.add(f.clone(true));
						sortedValueList.add(valuef);
						//-- end the search, and sort the next item from "features"
						isLarger = false;
					}
					j++;
				}//-- end while
			}
			i++;
		}//-- end for
		return sortedFeatureList;
	}
    
	/**
	 * calculates pt-line distances if points would be left out
	 * @param coords an ordered list of coordinates, i.e. in the order a line would be drawn
	 * @return distances
	 */
	public static double[] calcLeaveOneOutDistance(Coordinate[] coords){
		double[] distances= new double[coords.length-2];
		for (int i = 0; i < distances.length; i++) {
			PointLineDistance dist = new PointLineDistance(coords[i+1], coords[i], coords[i+2]);
			distances[i] = dist.getDistance();
		}
		return distances;
	}
	
	/**
	 * calculates the average travel distances per day, based on the MBR(day) of the convex hull(day)
	 * for all points taken of each day
	 * @param sortedPoints
	 * @param dayAttribute
	 * @param inputLayerName
	 * @param displayMBRs
	 * @param context can be null
	 * @return the travel distance per day in a list
	 */
	public static double[] calcOneDayActivityRadiusDistances(
			FeatureCollection sortedPoints, String dayAttribute, String inputLayerName, boolean displayMBRs,
			PlugInContext context) {
		FeatureSchema fs = new FeatureSchema();
		fs.addAttribute("geometry", AttributeType.GEOMETRY);
		fs.addAttribute("processingid", AttributeType.INTEGER);
		fs.addAttribute(dayAttribute, sortedPoints.getFeatureSchema().getAttributeType(dayAttribute));
		fs.addAttribute("numPoints", AttributeType.INTEGER);
		fs.addAttribute("MBR length", AttributeType.DOUBLE);
		fs.addAttribute("note", AttributeType.STRING);
		FeatureDataset mbrFc = new FeatureDataset(fs);
		
		GeometryFactory gf = new GeometryFactory();
		ArrayList<Double> distances = new ArrayList<Double>();
		Object[] lists = FeatureCollectionTools.sortFeaturesIntoListsByAttributeValue(sortedPoints, dayAttribute);
		Object featuresPerDayList = lists[0];
		ArrayList[] featuresPerDayArrayList = (ArrayList[])featuresPerDayList;
		for (int i = 0; i < featuresPerDayArrayList.length; i++) {
			ArrayList<Feature> oneDayFeatures = (ArrayList)featuresPerDayArrayList[i];
			//-- get the geometries of all points
			Geometry[] ptgeoms = new Geometry[oneDayFeatures.size()];
			int j = 0;
			Feature firstFeature = null;
			for (Iterator iterator = oneDayFeatures.iterator(); iterator
					.hasNext();) {
				Feature ftemp = (Feature) iterator.next();
				ptgeoms[j]= ftemp.getGeometry();
				if (j==0){//to get the date later
					firstFeature = ftemp;
				}
				j++;
			}
        	Feature f = new BasicFeature(fs);
        	f.setAttribute("processingid", i);
        	f.setAttribute("numPoints", ptgeoms.length);
        	f.setAttribute(dayAttribute, firstFeature.getAttribute(dayAttribute));
        	//-- calculate the individual distance and geometry
			if (ptgeoms.length >= 3){
				//-- calculate the convex hull
		        GeometryCollection gc = gf.createGeometryCollection(ptgeoms);
		        Geometry hull = gc.convexHull();
		        //-- get the MBR
		        OrientationMBR mbr = new OrientationMBR(hull);
		        Geometry mbrGeom = mbr.getMbr();
		        if (mbrGeom instanceof Polygon){
		        	//-- assign value
		        	distances.add(mbr.getMbrLength());
		        	//--
		        	f.setGeometry(mbrGeom);
		        	f.setAttribute("MBR length", mbr.getMbrLength());
		        	f.setAttribute("note", "MBR polygon");
		        }
		        else{
		        	context.getWorkbenchFrame().warnUser("MBR no polygon");
		        	System.out.println("TrackCalculationUtil.calcOneDayActivityRadiusDistances - MBR no polygon");
		        	if (mbrGeom instanceof LineString){	
		        		double length = ((LineString)mbrGeom).getLength();
			        	distances.add(length);
			        	//--
						Geometry buffer = mbrGeom.buffer(length/4.0);
			        	f.setGeometry(buffer);
			        	f.setAttribute("MBR length", length);
			        	f.setAttribute("note", "MBR is LineString, but>=3pts");
		        	}
		        	else{
			        	//--
			        	f.setGeometry(hull);
			        	f.setAttribute("MBR length", Double.NaN);
			        	f.setAttribute("note", "MBR is ?, but>=3pts");
		        	}
		        }
			} 
			else if(ptgeoms.length == 2){
				//-- take distance between points
				double ptdist = DistanceOp.distance(ptgeoms[0], ptgeoms[1]);
				distances.add(ptdist);
	        	//--
				Coordinate[] coords = new Coordinate[2];
				coords[0] = ptgeoms[0].getCoordinate();
				coords[1] = ptgeoms[1].getCoordinate();
				double x1 = ptgeoms[0].getCoordinate().x; double x2 = ptgeoms[1].getCoordinate().x;
				double y1 = ptgeoms[0].getCoordinate().y; double y2 = ptgeoms[1].getCoordinate().y;
				//-- something is/was wrong here. The multiplication doesn't work - so I get the 
				//   variables first to check what's going on.
				double xPt = (x1 + x2) / 2.0;
				double yPt = (y1 + y2) / 2.0;
				Coordinate ptCoord = new Coordinate(xPt,yPt);
				//-- use a line buffer
				//LineString ls = gf.createLineString(coords);
				//Geometry buffer = ls.buffer(ptdist/10.0);
				//-- or: create a circle
				Point pt = gf.createPoint(ptCoord);
				Geometry buffer = pt.buffer(0.5 * ptdist);
	        	//--
	        	f.setGeometry(buffer);
	        	f.setAttribute("MBR length", ptdist);
	        	f.setAttribute("note", "2 point circle");
			}
			else if(ptgeoms.length == 1){
				//-- note, we don't add/calculate a distance if 
				//   we have only one point for a day
				//   to get a distance we could use the last point from the previous day
				//   and the first point from the next day. However, the points I have are 
				//   unordered so I would need to look in the original dataset (probably
				//   using the location attribute).
				//   Hence, for visualization I will add a fake point of 1m radius
				/*
				if((i > 0) && (i < featuresPerDayArrayList.length-1)){
					Geometry pt = ptgeoms[0];
					//-- can't use this since point sets are not ordered.  
					ArrayList<Feature> previousDayFeatures = (ArrayList)featuresPerDayArrayList[i-1];
					ArrayList<Feature> nextDayFeatures = (ArrayList)featuresPerDayArrayList[i-1];
					Geometry ptPrev = ((Feature)previousDayFeatures.get(previousDayFeatures.size()-1)).getGeometry();
					Geometry ptAfter = ((Feature)previousDayFeatures.get(0)).getGeometry();
					double distBefore =  DistanceOp.distance(pt, ptPrev);
					double distAfter =  DistanceOp.distance(pt, ptAfter);
					double circleDist = 0;
					// use the smaller travel distance
					if (distBefore >= distAfter){
						circleDist = distAfter;
					}
					else{
						circleDist = distBefore;
					}
					Geometry buffer = pt.buffer(0.5 * circleDist);
		        	//--
		        	f.setGeometry(buffer);
		        	f.setAttribute("MBR length", Double.NaN);
		        	f.setAttribute("note", "single point circle");
				}
				else{
					//don't do anything
					System.out.println("calcOneDayActivityRadiusDistances: first or last day has only one point ");
				}
				*/
				Geometry buffer = ptgeoms[0].buffer(1.0);
	        	f.setGeometry(buffer);
	        	f.setAttribute("MBR length", Double.NaN);
	        	f.setAttribute("note", "single point circle");
			}
        	mbrFc.add(f);
		}
		//-- convert to double[] for output
		double[] mbrDistancePerDay = new double[distances.size()]; 
		int k= 0;
		for (Iterator iterator = distances.iterator(); iterator
				.hasNext();) {
			Double d = (Double) iterator.next();
			mbrDistancePerDay[k] = d.doubleValue();
			k++;
		}		
		//-- display shapes/MBRs
		if (context != null){
			if (displayMBRs && (mbrFc.size() > 0)){
				context.addLayer(StandardCategoryNames.RESULT, inputLayerName + "-DailyMBRs", mbrFc);	
			}
		}
		return mbrDistancePerDay;
	}
	
	/**
	 * sort the input points by location id
	 * @param the features
	 * @param locAttribute
	 * @return sorted points
	 */
	public static FeatureCollection sortPointsByLocationId(FeatureCollection points,
			String locAttribute) {
		ArrayList<Feature> sortedFeatures = FeatureCollectionTools.sortFeatureListByAttributeBeginWithSmallest(points.getFeatures(), 
				locAttribute);
		FeatureDataset resultFc = new FeatureDataset(points.getFeatureSchema());
		for (Iterator iterator = sortedFeatures.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			resultFc.add(f);
		}
		return resultFc;
	}
	
	/**
	 * Converts from two time attributes into one. The resulting time will be in hours, calculated
	 * by days * 24 + hours.
	 * @param points
	 * @param dayAttributeName
	 * @param hourAttributeName
	 * @param newTimeAttributeName
	 * @return
	 */
	public static FeatureCollection aggregateDayAndHourForPoints(FeatureCollection points,
			String dayAttributeName, String hourAttributeName, String newTimeAttributeName) {
    	
		FeatureSchema oldFS = points.getFeatureSchema();
    	FeatureSchema fsNewSingle = (FeatureSchema)oldFS.clone();
    	fsNewSingle.addAttribute(newTimeAttributeName, AttributeType.DOUBLE);
    	
		FeatureDataset resultFc = new FeatureDataset(fsNewSingle);
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			Feature fNew = FeatureSchemaTools.copyFeature(f, fsNewSingle);
			Object daysO = f.getAttribute(dayAttributeName);
			double days = Double.NaN;
			if(daysO instanceof Double){
				days = (Double)f.getAttribute(dayAttributeName);
			}
			else if(daysO instanceof Integer){
				days = ((Integer)f.getAttribute(dayAttributeName)).doubleValue();
			}
			else{
				days = 0;
			}
			Object hoursO = f.getAttribute(hourAttributeName);
			double hours = Double.NaN;
			if(hoursO instanceof Double){
				hours = (Double)f.getAttribute(hourAttributeName);
			}
			else if(hoursO instanceof Integer){
				hours = ((Integer)f.getAttribute(hourAttributeName)).doubleValue();
			}
			else{
				hours = 0;
			}
			double inHours = days * 24 + hours;
			fNew.setAttribute(newTimeAttributeName, new Double(inHours));
			resultFc.add(fNew);
		}
		return resultFc;
	}
	
	/**
	 * Converts a set of point features into a set of lines - with each line connecting 
	 * two subsequent points. I.e. points need to be delivered in the correct order. 
	 * @param pointFeatures
	 * @param locationAttribute
	 * @param timeAttribute, can be null
	 * @param context used to draw several result layers.
	 * @param monitor can be null
	 * @return lines in a FeatureCollection.
	 */
    public static FeatureCollection convertToLines(FeatureCollection pointFeatures, String locationAttribute, String timeAttribute, PlugInContext context, TaskMonitor monitor) {
		//-- featureschema for two point one segment
    	FeatureSchema oldFS = pointFeatures.getFeatureSchema();
    	FeatureSchema fsNewSingle = new FeatureSchema();
    	fsNewSingle.addAttribute("geometry", AttributeType.GEOMETRY);
    	fsNewSingle.addAttribute("lineProcID", AttributeType.INTEGER);
    	fsNewSingle.addAttribute("id_pt1", AttributeType.INTEGER);
    	fsNewSingle.addAttribute("id_pt2", AttributeType.INTEGER);
    	fsNewSingle.addAttribute(locationAttribute + "_pt1", oldFS.getAttributeType(locationAttribute));
    	fsNewSingle.addAttribute(locationAttribute + "_pt2", oldFS.getAttributeType(locationAttribute));
    	if (timeAttribute != null){
	    	fsNewSingle.addAttribute(timeAttribute + "_pt1", oldFS.getAttributeType(timeAttribute));
	    	fsNewSingle.addAttribute(timeAttribute + "_pt2", oldFS.getAttributeType(timeAttribute));
	    	fsNewSingle.addAttribute(TIME_DIFFERENCE_ATTRIBUTE_NAME, AttributeType.DOUBLE);
    	}
    	//--
    	FeatureCollection fcResult = null;
    	fcResult = new FeatureDataset(fsNewSingle);
    	//-- convert every point
		Feature firstFeature = (Feature)pointFeatures.getFeatures().get(0);
		if(firstFeature.getGeometry() instanceof Point){
			Collection<Feature> individualPts = pointFeatures.getFeatures();
			//-- generate the lines from the points
			if (monitor != null){
				monitor.report("generate tracks");
			}
			FeatureCollection lines = TrackCalculationUtil.createSingleLineFeaturesFromFeatures(individualPts, fsNewSingle, locationAttribute, timeAttribute, context);
			fcResult.addAll(lines.getFeatures());
		}	
		else{
			if (context != null){
				context.getWorkbenchFrame().warnUser("convertToLines: first feature not a point");
			}
			System.out.println("convertToLines: first feature not a point");
			return null;
		}   
		return fcResult;
	}

    /**
     * creates line features from the points, where every LineString connects only two points. The animal id and 
     * the time attributes are attached as feature attributes.
     * @param points
     * @param fsNew
     * @param locationAttribute can be null
     * @param dateAttribute can be null
     * @param PlugInContext can be null
     * @return
     */
	public static FeatureCollection createSingleLineFeaturesFromFeatures(
			Collection<Feature> points, FeatureSchema fsNew, String locationAttribute, String dateAttribute, PlugInContext context) {
		GeometryFactory gfactory = new GeometryFactory();
    	FeatureDataset fd = new FeatureDataset(fsNew);
    	int i = 0; Feature lastFeature = null;
		for (Iterator iterator = points.iterator(); iterator.hasNext();) {
			Feature f = (Feature) iterator.next();
			if (i>0){ // start building lines when having the second point
				if(f.getGeometry() instanceof Point){ 
					Coordinate[] coords = new Coordinate[2];
					coords[0] = ((Point)lastFeature.getGeometry()).getCoordinate();
					coords[1] = ((Point)f.getGeometry()).getCoordinate();
					LineString ls = gfactory.createLineString(coords);
					//--create and add the feature
					Feature newF = new BasicFeature(fsNew);
					newF.setGeometry(ls);
					newF.setAttribute("lineProcID", i-1);
					newF.setAttribute("id_pt1", i-1);
					newF.setAttribute("id_pt2", i);
					//--
					if(locationAttribute != null){
						newF.setAttribute(locationAttribute + "_pt1", lastFeature.getAttribute(locationAttribute));
						newF.setAttribute(locationAttribute + "_pt2", f.getAttribute(locationAttribute));
					}
					if(dateAttribute != null){
						double curTime = 0;
						Object dayc = f.getAttribute(dateAttribute);
						if(dayc instanceof Double){
							curTime = (Double)f.getAttribute(dateAttribute);
						}
						else if(dayc instanceof Integer){
							curTime = ((Integer)f.getAttribute(dateAttribute)).doubleValue();
						}
						double prevTime = 0;
						Object dayp = lastFeature.getAttribute(dateAttribute);
						if(dayp instanceof Double){
							prevTime = (Double)lastFeature.getAttribute(dateAttribute);
						}
						else if(dayp instanceof Integer){
							prevTime = ((Integer)lastFeature.getAttribute(dateAttribute)).doubleValue();
						}
						newF.setAttribute(dateAttribute + "_pt1", new Double(prevTime));
						newF.setAttribute(dateAttribute + "_pt2", new Double(curTime));
						double deltaT = curTime - prevTime;
						if(deltaT < 0){
							deltaT = Math.abs(deltaT);
							if(context != null){
								context.getWorkbenchFrame().warnUser("found negative time difference - points incorrectly sorted!");
							}
						}
						newF.setAttribute(TIME_DIFFERENCE_ATTRIBUTE_NAME, deltaT);
					}
					//-- add
					fd.add(newF);
					//--
				}
				else{
					System.out.println("createSingleLineFeaturesFromFeatures: feature not a point, ID: " + f.getID());
				}
			}//--end if(i>0)
			lastFeature = f.clone(true);
			i++;
		}
		return fd;
	}
	
	
 }
