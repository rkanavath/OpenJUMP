package fr.michaelm.jump.plugin.topology;

import com.vividsolutions.jts.geom.*;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.operation.distance.DistanceOp;
import com.vividsolutions.jts.operation.distance.GeometryLocation;
import com.vividsolutions.jts.operation.distance3d.Distance3DOp;
import com.vividsolutions.jump.feature.Feature;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Michaël on 19/05/14.
 */
public class TargetGeometry {

    Feature feature;
    Geometry geometry;
    GeometryWrapper wgeometry;
    double maxDistance = Double.POSITIVE_INFINITY;
    VertexSnapper snapper;
    List<Coordinate> projectedCoordinates;
    boolean done = false;

    public TargetGeometry(Feature feature, double maxDistance, VertexSnapper snapper, STRtree index) {
        this.feature = feature;
        this.geometry = feature.getGeometry();
        this.wgeometry = GeometryWrapper.createWrapper(feature, index);
        this.maxDistance = maxDistance;
        this.snapper = snapper;

    }

    /*
    public void project(Feature point) {
        Location location = null;
        if (done || geometry.isEmpty()) {
            return;
        }
        else if (geometry instanceof Point) {
            if (geometry.distance(point.getGeometry()) <= maxDistance) {
                location = new Location(-1, -1, -1, geometry.getCoordinate());
            }
        }
        else if (geometry instanceof MultiPoint) {
            double minDist = Double.POSITIVE_INFINITY;
            Coordinate c = null;
            int index = -1;
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                double dist = point.getGeometry().distance(geometry.getGeometryN(i));
                if (dist < minDist) {
                    minDist = dist;
                    index = i;
                    c = geometry.getGeometryN(i).getCoordinate();
                }
            }
            location = new Location(index, -1, -1, geometry.getCoordinate());
        }
        else if (geometry instanceof LineString || geometry instanceof MultiLineString) {
            DistanceOp op = new DistanceOp(point.getGeometry(), geometry);
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation loc = op.nearestLocations()[1];
                Coordinate[] cc = loc.getGeometryComponent().getCoordinates();
                LineSegment line = new LineSegment(cc[loc.getSegmentIndex()], cc[loc.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                int[] indices = getComponent(geometry, (LineString)loc.getGeometryComponent());
                location = new Location(indices[0], 0, loc.getSegmentIndex(), proj.getCoord());
            }
        }
        else if (geometry instanceof Polygon || geometry instanceof MultiPolygon) {
            DistanceOp op = new DistanceOp(point.getGeometry(), geometry.getBoundary());
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation loc = op.nearestLocations()[1];
                Coordinate[] cc = loc.getGeometryComponent().getCoordinates();
                LineSegment line = new LineSegment(cc[loc.getSegmentIndex()], cc[loc.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                int[] indices = getComponent(geometry, (LineString)loc.getGeometryComponent());
                location = new Location(indices[0], indices[1], loc.getSegmentIndex(), proj.getCoord());
            }
        }
        else { // GeometryCollection
            double minDist = Double.POSITIVE_INFINITY;
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                Geometry component = geometry.getGeometryN(i);
                if (component.getDimension() == 2) component = component.getBoundary();
                DistanceOp op = new DistanceOp(point.getGeometry(), component);
                double distance = op.distance();
                if (distance <= maxDistance && distance < minDist) {
                    GeometryLocation loc = op.nearestLocations()[1];
                    Coordinate[] cc = loc.getGeometryComponent().getCoordinates();
                    LineSegment line = new LineSegment(cc[loc.getSegmentIndex()], cc[loc.getSegmentIndex()+1]);
                    Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                    int[] indices = getComponent(geometry, (LineString)loc.getGeometryComponent());
                    location = new Location(indices[0], indices[1], loc.getSegmentIndex(), proj.getCoord());
                }
            }
        }
    }

    @Deprecated
    public List<Projection> projectToCoordinate(Feature point) {

        List<Projection> projectedCoordinates = new ArrayList<Projection>();

        if (done || geometry.isEmpty()) {
            return projectedCoordinates;
        }
        else if (geometry instanceof Point) {
            if (geometry.distance(point.getGeometry()) <= maxDistance) {
                projectedCoordinates.add(new Projection(point,
                        feature,
                        feature.getGeometry().getCoordinate(),
                        geometry.distance(point.getGeometry())));
            }
        }
        else if (geometry instanceof MultiPoint) {
            double minDist = Double.POSITIVE_INFINITY;
            Coordinate c = null;
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                double dist = point.getGeometry().distance(geometry.getGeometryN(i));
                if (dist < minDist) {
                    minDist = dist;
                    c = geometry.getGeometryN(i).getCoordinate();
                }
            }
            projectedCoordinates.add(new Projection(point, feature, c, minDist));
        }
        else if (geometry instanceof LineString || geometry instanceof MultiLineString) {
            DistanceOp op = new DistanceOp(point.getGeometry(), geometry);
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation location = op.nearestLocations()[1];
                Coordinate[] cc = location.getGeometryComponent().getCoordinates();
                LineSegment line = new LineSegment(cc[location.getSegmentIndex()], cc[location.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                projectedCoordinates.add(proj);
            }
        }
        else if (geometry instanceof Polygon || geometry instanceof MultiPolygon) {
            DistanceOp op = new DistanceOp(point.getGeometry(), geometry.getBoundary());
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation location = op.nearestLocations()[1];
                Coordinate[] cc = location.getGeometryComponent().getCoordinates();
                LineSegment line = new LineSegment(cc[location.getSegmentIndex()], cc[location.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                projectedCoordinates.add(proj);
            }
        }
        else { // GeometryCollection
            double minDist = Double.POSITIVE_INFINITY;
            Coordinate c = null;
            Projection proj = null;
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                Geometry component = geometry.getGeometryN(i);
                if (component.getDimension() == 2) component = component.getBoundary();
                DistanceOp op = new DistanceOp(point.getGeometry(), component);
                double distance = op.distance();
                if (distance <= maxDistance) {
                    GeometryLocation location = op.nearestLocations()[1];
                    Coordinate[] cc = location.getGeometryComponent().getCoordinates();
                    LineSegment line = new LineSegment(cc[location.getSegmentIndex()], cc[location.getSegmentIndex()+1]);
                    proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                    projectedCoordinates.add(proj);
                }
            }
            projectedCoordinates.add(proj);
        }
        done = true;
        return projectedCoordinates;
    }

    public void insert(Feature point) {
        List<Projection> projectedCoordinates = new ArrayList<Projection>();
        if (done || geometry.isEmpty()) {
            return;
        }
        else if (geometry.getDimension() == 0) {
            project(point);
        }
        else if (geometry instanceof LineString || geometry instanceof MultiLineString) {
            DistanceOp op = new DistanceOp(point.getGeometry(),geometry);
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation location = op.nearestLocations()[1];
                Coordinate[] cc = geometry.getCoordinates();
                LineSegment line = new LineSegment(cc[location.getSegmentIndex()], cc[location.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                projectedCoordinates.add(proj);
                int[] component = getComponent(geometry, (LinearRing)location.getGeometryComponent());
                insert(component[0], component[1], location.getSegmentIndex(), proj.getCoord());
            }
        }
        done = true;
    }

    public void split(Feature point, boolean onlyOnce) {
        List<Projection> projectedCoordinates = new ArrayList<Projection>();
        if (done || geometry.isEmpty()) {
            return;
        }
        else if (geometry.getDimension() == 0) {
            project(point);
        }
        else if (geometry instanceof LineString || geometry instanceof MultiLineString) {
            DistanceOp op = new DistanceOp(point.getGeometry(), geometry);
            double distance = op.distance();
            if (distance <= maxDistance) {
                GeometryLocation location = op.nearestLocations()[1];
                Coordinate[] cc = geometry.getCoordinates();
                LineSegment line = new LineSegment(cc[location.getSegmentIndex()], cc[location.getSegmentIndex()+1]);
                Projection proj = snapper.snap(point, new SegmentElement(wgeometry, line.p0, line.p1));
                projectedCoordinates.add(proj);
                int[] component = getComponent(geometry, (LinearRing)location.getGeometryComponent());
                insert(component[0], component[1], location.getSegmentIndex(), proj.getCoord());
            }
        }
        done = true;
    }


    private int[] getComponent(Geometry geometry, LineString linearComponent) {
        for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
            Geometry component = geometry.getGeometryN(i);
            if (component.getDimension() == 1 && component.equals(linearComponent)) {
                return new int[]{i,-1};
            } else if (component.getDimension() == 2) {
                Polygon polygon = (Polygon)component;
                if (polygon.getExteriorRing().equals(linearComponent)) {
                    return new int[]{i,0};
                }
                for (int j = 0 ; j < polygon.getNumInteriorRing() ; j++) {
                    if (polygon.getInteriorRingN(j).equals(linearComponent)) {
                        return new int[]{i,j};
                    }
                }
            }
        }
        return new int[]{-1,-1};
    }
*/


    /**
     * Replace geometry by a new Geometry where Coordinate c has been inserted in
     * component componentIndex, linearComponent subComponent, at index segmentIndex.
     */
    /*
    private void insert(int componentIndex, int subComponent, int segmentIndex, Coordinate c) {
        Geometry component = geometry.getGeometryN(componentIndex);
        Geometry newComponent = null;
        if (component instanceof LineString) {
            newComponent = insertInLineString((LineString) component, segmentIndex, c);
        } else if (component instanceof Polygon) {
            newComponent = insertInPolygon((Polygon)component, subComponent, segmentIndex, c);
        } else {
            newComponent = component;
        }
        if (geometry instanceof GeometryCollection) {
            List<Geometry> components = new ArrayList<Geometry>(geometry.getNumGeometries());
            for (int i = 0 ; i < geometry.getNumGeometries() ; i++) {
                if (i == componentIndex) {
                    components.add(newComponent);
                } else {
                    components.add(geometry.getGeometryN(i));
                }
                geometry = geometry.getFactory().buildGeometry(components);
            }
        } else {
            geometry = newComponent;
        }
    }
    */

    /**
     * Insert Coordinate c in LinearRing ring at index segmentIndex
     */
    /*
    private Polygon insertInPolygon(Polygon polygon, int linearComponentIndex, int segmentIndex, Coordinate c) {
        if (linearComponentIndex == 0) {
            LinearRing exteriorRing = insertInLinearRing(polygon.getExteriorRing(), segmentIndex, c);
            LinearRing[] holes = new LinearRing[polygon.getNumInteriorRing()];
            for (int i = 0 ; i < polygon.getNumInteriorRing() ; i++) {
                holes[i] = (LinearRing)polygon.getInteriorRingN(i);
            }
            return polygon.getFactory().createPolygon(exteriorRing, holes);
        } else {
            LinearRing exteriorRing = (LinearRing)polygon.getExteriorRing();
            LinearRing[] holes = new LinearRing[polygon.getNumInteriorRing()];
            for (int i = 0 ; i < polygon.getNumInteriorRing() ; i++) {
                if (i == linearComponentIndex-1) {
                    holes[i] = insertInLinearRing(polygon.getInteriorRingN(i), segmentIndex, c);
                } else {
                    holes[i] = (LinearRing)polygon.getInteriorRingN(i);
                }
            }
            return polygon.getFactory().createPolygon(exteriorRing, holes);
        }
    }
    */

    /**
     * Insert Coordinate c in LinearRing ring at index segmentIndex
     */
    /*
    private LinearRing insertInLinearRing(LineString ring, int segmentIndex, Coordinate c) {
        CoordinateList list = new CoordinateList(ring.getCoordinates());
        list.add(segmentIndex+1, c);
        return ring.getFactory().createLinearRing(list.toCoordinateArray());
    }
    */

    /**
     * Insert Coordinate c in LineString line at index segmentIndex
     */
    /*
    private LineString insertInLineString(LineString line, int segmentIndex, Coordinate c) {
        CoordinateList list = new CoordinateList(line.getCoordinates());
        list.add(segmentIndex+1, c);
        return line.getFactory().createLineString(list.toCoordinateArray());
    }
    */

    /*
    private void project(Location location) {

    }

    private void insert(Location location) {

    }

    private void split(Location location) {

    }

    class Location {
        int component = -1;
        int subComponent = -1;
        int segmentIndex = -1;
        Coordinate coordinate;
        Location() {}
        Location(int component, int subComponent, int segmentIndex, Coordinate coordinate) {
            this.component = component;
            this.subComponent = subComponent;
            this.segmentIndex = segmentIndex;
            this.coordinate = coordinate;
        }
        public void setComponent(int component) {this.component = component;}
        public void setSubComponent(int subComponent) {this.subComponent = subComponent;}
        public void setSegmentIndex(int segmentIndex) {this.segmentIndex = segmentIndex;}
        public void setCoordinate(Coordinate coordinate) {this.coordinate = coordinate;}
    }
    */

}
