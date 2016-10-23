package fr.michaelm.jump.feature.jgrapht;

import java.util.*;

import org.jgrapht.*;
import org.jgrapht.alg.*;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Utility class to work with graphs built from feature collections.
 * @author Michael Michaud
 * @version 0.1 (2007-05-28)
 */

public class GraphUtil {
    
    
   /**
    * Returns true if the graph formed by features is connected.
    * @param features the collection of features
    * @param dim3 true if c(x,y,z) and c(x,y,z') are considered as different nodes
    */
    public static boolean isGraphConnected(Collection features, boolean dim3) {
        Graph<INode,FeatureAsEdge> g = GraphFactory.createGraph(features, dim3);
        return new ConnectivityInspector((UndirectedGraph<INode,FeatureAsEdge>)g)
                   .isGraphConnected();
    }
    
   /**
    * Returns a list of connected Set s of vertices.
    * @param features the collection of features
    * @param dim3 true if c(x,y,z) and c(x,y,z') are considered as different nodes
    */
    public static List createConnectedNodeSets(Collection features, boolean dim3) {
        Graph<INode,FeatureAsEdge> g = GraphFactory.createGraph(features, dim3);
        return new ConnectivityInspector((UndirectedGraph<INode,FeatureAsEdge>)g)
                   .connectedSets();
    }
    
   /**
    * Returns vertices having a deegree higher than min and lower than max as a list of
    * geometries.
    * @param features the collection of features
    * @param degree the degree of nodes to return (inclusive)
    * @param dim3 true if c(x,y,z) and c(x,y,z') are considered as different nodes
    */
    public static List getVertices(Collection features, int degree, boolean dim3) {
        return getVertices(features, degree, degree, dim3);
    }
    
    /**
    * Returns vertices having a deegree higher than min and lower than max as a list of
    * geometries.
    * @param features the collection of features
    * @param minDegree the minimum degree of nodes to return (inclusive)
    * @param maxDegree the maximum degree of nodes to return (inclusive)
    * @param dim3 true if c(x,y,z) and c(x,y,z') are considered as different nodes
    */
    public static List getVertices(Collection features, int minDegree, int maxDegree, boolean dim3) {
        assert minDegree >= 0 : "" + minDegree + " : minDegree must be positive or null";
        assert maxDegree >= minDegree : "" + maxDegree + " : maxDegree must more or equals to minDegree";
        UndirectedGraph<INode,FeatureAsEdge> g =
            (UndirectedGraph<INode,FeatureAsEdge>)GraphFactory.createGraph(features, dim3);
        List geometries = new ArrayList();
        for (Iterator<INode> it = g.vertexSet().iterator() ; it.hasNext() ; ) {
            INode node = it.next();
            int degree = g.degreeOf(node);
            if (degree>=minDegree && degree<=maxDegree) {
                geometries.add(node.getGeometry());
            }
        }
        return geometries;
    }
    
}

