package fr.michaelm.jump.feature.jgrapht;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * This interface is implemented by every object used as a Node in a graph
 * composed with JUMP features or JTS geometries.
 * INodes may correspond to features in a feature collection (ex. crossroads) or
 * to feature boundaries (ex. start point and end point of a linear road
 * segment).<br>
 * @author Michael Michaud
 * @version 0.2 (2007-04-20)
 */
// 0.2 (2007-04-20) : add the static geometryFactory used in implementation classes
// 0.1 (2006-06-01) : initial release

public interface INode {
    
    static final GeometryFactory DEFAULT_GEOMETRY_FACTORY = new GeometryFactory();
    
   /**
    * Return the coordinate of this Node.
    */
    Coordinate getCoordinate();

   /**
    * Return a Geometry representing this Node.
    */
    Geometry getGeometry();

}

