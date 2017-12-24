/* 
 * Kosmo - Sistema Abierto de Informaci�n Geogr�fica
 * Kosmo - Open Geographical Information System
 *
 * http://www.saig.es
 * (C) 2006, SAIG S.L.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation;
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * For more information, contact:
 * 
 * Sistemas Abiertos de Informaci�n Geogr�fica, S.L.
 * Avnda. Rep�blica Argentina, 28
 * Edificio Domocenter Planta 2� Oficina 7
 * C.P.: 41930 - Bormujos (Sevilla)
 * Espa�a / Spain
 *
 * Tel�fono / Phone Number
 * +34 954 788876
 * 
 * Correo electr�nico / Email
 * info@saig.es
 *
 */
package org.openjump.advancedtools.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

//import javax.vecmath.Point2d;
//import org.apache.commons.collections.CollectionUtils;
//import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.log4j.Logger;
//import org.cresques.cts.IProjection;

//import org.saig.core.dao.datasource.filedatasource.shape.geometry.IShapeGeometry;
//import org.saig.core.dao.datasource.filedatasource.shape.geometry.ShapeGeometryConverter;
//import org.saig.core.filter.Filter;
//import com.iver.cit.gvsig.fmap.tools.geo.Geo;
import com.vividsolutions.jts.algorithm.RobustLineIntersector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;
import com.vividsolutions.jts.geom.util.GeometryEditor.GeometryEditorOperation;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;
import com.vividsolutions.jts.noding.IntersectionAdder;
import com.vividsolutions.jts.noding.MCIndexNoder;
import com.vividsolutions.jts.noding.NodedSegmentString;
import com.vividsolutions.jts.noding.SegmentNode;
import com.vividsolutions.jts.noding.SegmentNodeList;
import com.vividsolutions.jts.operation.distance.DistanceOp;
import com.vividsolutions.jts.operation.overlay.snap.GeometrySnapper;
import com.vividsolutions.jts.operation.polygonize.Polygonizer;
import com.vividsolutions.jts.precision.EnhancedPrecisionOp;
import com.vividsolutions.jump.coordsys.CoordinateSystem;
import com.vividsolutions.jump.coordsys.Reprojector;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureSchema;
import com.vividsolutions.jump.geom.EnvelopeUtil;
import com.vividsolutions.jump.util.CoordinateArrays;
import com.vividsolutions.jump.workbench.model.Layer;
import com.vividsolutions.jump.workbench.ui.LayerViewPanel;

/**
 * Utilidades varias para las herramientas de edicion Utilities for editing
 * tools
 * <p>
 * </p>
 * 
 * @author Sergio Ba�os Calvo
 * @since Kosmo 1.0.0
 * 
 *        Giuseppe Aruta 2017_22_01 - Adapted for OpenJUMP Utils that requires
 *        extra libraries are deactivated
 */
public class EditUtils {

    /** Log */
    private static final Logger LOGGER = Logger.getLogger(EditUtils.class);

    /** Geometry factory */
    private static GeometryFactory geomFac = new GeometryFactory();

    /** Snap tolerance value */
    // TODO: Check for a tolerance value valid for all the input geometries,
    // independent
    // for the SRS and view units values
    public static final double DEFAULT_SNAP_TOLERANCE = 0.0001;

    /**
     * It gets fragments from resulting geometries
     * 
     * @param firstGeometry
     * @param elseGeometries
     * @return
     */
    public static Geometry[] getFragmentos(Geometry firstGeometry,
            Geometry elseGeometries) {
        ArrayList elseGeometriesAsList = new ArrayList();
        Geometry[] fragmentosGeometries;
        getFragmentosGeometria(elseGeometriesAsList, firstGeometry, false);

        // [SBCALVO 16/04/2009] - Don't separe the other fragments
        // getFragmentosGeometria(elseGeometriesAsList, elseGeometries, false);
        elseGeometriesAsList.add(elseGeometries);
        fragmentosGeometries = new Geometry[elseGeometriesAsList.size()];
        elseGeometriesAsList.toArray(fragmentosGeometries);
        return fragmentosGeometries;
    }

    /**
     * @param elseGeometriesAsList
     * @param geometria
     * @param soloPoligono
     */
    public static void getFragmentosGeometria(ArrayList elseGeometriesAsList,
            Geometry geometria, boolean soloPoligono) {
        if (geometria instanceof GeometryCollection) {
            GeometryCollection collection = (GeometryCollection) geometria;
            for (int i = 0; i < collection.getNumGeometries(); i++) {
                Geometry geometryBarrida = collection.getGeometryN(i);
                getFragmentosGeometria(elseGeometriesAsList, geometryBarrida,
                        soloPoligono);
                // elseGeometriesAsList.add(geometryBarrida);
            }
            // for
        } else if (geometria instanceof MultiPolygon) {
            // Nos quedamos con cada una de sus partes
            MultiPolygon multiP = (MultiPolygon) geometria;
            for (int i = 0; i < multiP.getNumGeometries(); i++) {
                Geometry newGeometry = multiP.getGeometryN(i);
                elseGeometriesAsList.add(newGeometry);
            }
        } else if (geometria instanceof com.vividsolutions.jts.geom.Polygon) {
            elseGeometriesAsList.add(geometria);
        } else {
            if (!soloPoligono) {
                elseGeometriesAsList.add(geometria);
            }
        }
        // else
    }

    /**
     * It get geometries adjacent to a given one
     * 
     * @param simplyGeometryToProcess
     * @param layer
     * @return List of adjacent geometries
     * @throws Exception
     */

    public Collection<Feature> getColindantes(Geometry simplyGeometryToProcess,
            Layer layer) throws Exception {
        Collection candidatos = layer.getFeatureCollectionWrapper().query(
                simplyGeometryToProcess.getEnvelopeInternal());
        ArrayList<Feature> result = new ArrayList<Feature>();
        for (Iterator candIt = candidatos.iterator(); candIt.hasNext();) {
            Feature feat = (Feature) candIt.next();
            if (simplyGeometryToProcess != null
                    && !simplyGeometryToProcess.equals(feat.getGeometry())
                    && (simplyGeometryToProcess.touches(feat.getGeometry()) || simplyGeometryToProcess
                            .intersects(feat.getGeometry()))) {

            }
        }
        return result;
    }

    /**
     * Boolean if given coordinates are within a geometry
     * 
     * @param geometry
     * @param target
     * @return
     */
    public static boolean contieneCoordenada(Geometry geometry,
            Coordinate target) {
        boolean resultado = false;

        List coordArrays = com.vividsolutions.jump.util.CoordinateArrays
                .toCoordinateArrays(geometry, false);

        for (Iterator i = coordArrays.iterator(); i.hasNext();) {
            Coordinate[] coordinates = (Coordinate[]) i.next();
            for (int j = 1; j < coordinates.length; j++) {
                if (coordinates[j].equals2D(target)) {
                    return true;
                }
            }

        }
        return resultado;
    }

    /**
     * @param geometry
     * @param target
     * @return
     */
    public static LineSegment segmentInRange(Geometry geometry,
            Coordinate target) {
        // It's possible that the geometry may have no segments in range; for
        // example, if it
        // is empty, or if only has points in range. [Jon Aquino]
        LineSegment closest = null;
        List coordArrays = CoordinateArrays.toCoordinateArrays(geometry, false);

        for (Iterator i = coordArrays.iterator(); i.hasNext();) {
            Coordinate[] coordinates = (Coordinate[]) i.next();
            for (int j = 1; j < coordinates.length; j++) {
                // 1
                LineSegment candidate = new LineSegment(coordinates[j - 1],
                        coordinates[j]);
                if ((closest == null)
                        || (candidate.distance(target) < closest
                                .distance(target))) {
                    closest = candidate;
                }
            }
        }
        return closest;
    }

    /**
     * @param geometry
     * @param layer
     * @param operator
     * @return
     * @throws Exception
     */
    // public static Set<Feature> getColindantes( Geometry geometry, Layer
    // layer, Filter operator )
    // throws Exception {
    //
    // FeatureIterator itColindantes = null;
    // Set<Feature> colindantes = new HashSet<Feature>();
    //
    // try {
    // itColindantes =
    // layer.getFeatureCollectionWrapper().queryIterator(operator,
    // geometry.getEnvelopeInternal());
    //
    // while( itColindantes.hasNext() ) {

    // Feature featureCand = itColindantes.next();
    // if (featureCand != null
    // && !featureCand.getGeometry().equals(geometry)
    // && (featureCand.getGeometry().touches(geometry) ||
    // featureCand.getGeometry()
    // .intersects(geometry))) {
    // colindantes.add(featureCand);
    // }
    //
    // }
    // } finally {
    // if (itColindantes != null) {
    // itColindantes.close();
    // }
    // }

    // while
    // return colindantes;
    // }
    /**
     * It gets the coordinate of the geometry closest to the given coordinate
     * 
     * @param c
     *            Coordinates
     * @param g
     *            Geometry
     * @return Coordinate
     */
    public static Coordinate closestCoordinateToGeometry(Coordinate c,
            Geometry g) {
        Point p = geomFac.createPoint(c);
        DistanceOp dop = new DistanceOp(p, g);
        Coordinate[] coords = dop.nearestPoints();
        if (coords.length < 2)
            return null;
        Coordinate cr = p.getCoordinate().equals(coords[0]) ? coords[1]
                : coords[0];
        return cr;
    }

    /**
     * It gets the angle between two coordinates
     * 
     * @param x
     * @param y
     * @param xd
     * @param yd
     * @return double
     */
    public static double getAngle(double x, double y, double xd, double yd) {
        float angle;
        float d = (float) Math.sqrt((x - xd) * (x - xd) + (y - yd) * (y - yd));
        float sina = (float) (x - xd) / d;
        float cosa = (float) (yd - y) / d;
        if (cosa > 0)
            angle = (float) Math.asin(sina);
        else
            angle = (float) (-Math.asin(sina) + Math.PI);

        if (angle < 0)
            angle += Math.PI * 2;
        return angle;
    }

    /**
     * It create an arc from the center, its radius, the starting angle, the
     * final angle and the number of segments
     * 
     * @param center
     *            Coordinate of the center of the arc
     * @param ratio
     *            Radius of the circumpher
     * @param angleStart
     *            Start angle
     * @param angleEnd
     *            Final angle
     * @param nSegments
     *            Number of segments
     * @return Coordinate[] - Arc
     */
    public static Coordinate[] createArc(Coordinate center, double ratio,
            double angleStart, double angleEnd, int nSegments) {
        Coordinate[] c = new Coordinate[nSegments + 1];
        double arc;
        if (angleEnd < angleStart) {
            angleEnd += Math.PI * 2;
        }
        arc = angleEnd - angleStart;
        for (int i = 0; i < nSegments; i++) {
            double angle = i * ((arc) / nSegments) + angleStart;
            double x = center.x + Math.sin(angle) * ratio;
            double y = center.y + Math.cos(angle) * ratio;
            c[i] = new Coordinate(x, y);
        }
        double angle = nSegments * ((arc) / nSegments) + angleStart;
        double x = center.x + Math.sin(angle) * ratio;
        double y = center.y + Math.cos(angle) * ratio;
        c[nSegments] = new Coordinate(x, y);

        return c;
    }

    /**
     * It creates the circumference from the coordinate of its center, its
     * radius and the number of segments
     * 
     * @param center
     *            Coordinate of the center of the circumference
     * @param ratio
     *            Radius of the circumference
     * @param nSegments
     *            Number of segments
     * @return Coordinate[] - Coordinates of the circumference
     */
    public static Coordinate[] createCircle(Coordinate center, double ratio,
            int nSegments) {
        Coordinate[] c = new Coordinate[nSegments + 1];
        for (int i = 0; i < nSegments; i++) {
            double angle = i * (Math.PI * 2 / nSegments);
            double x = center.x + Math.sin(angle) * ratio;
            double y = center.y + Math.cos(angle) * ratio;
            c[i] = new Coordinate(x, y);
        }
        c[nSegments] = c[0];
        return c;
    }

    /**
     * It gets the existing geometry at the selected point
     * 
     * @param sc source coordinate
     * @param l layer
     * @param scale Scale factor
     * @return Feature
     * @throws Exception
     */

    public static Feature getSelectedGeom(Coordinate sc, Layer l, double scale)
            throws Exception {
        double pixel = 1.0 / scale;
        Envelope env = new Envelope(sc.x - pixel, sc.x + pixel, sc.y - pixel,
                sc.y + pixel);
        Set ifeats = intersectingFeatures(l, env);
        if (ifeats.size() == 0)
            return null;
        Iterator it = ifeats.iterator();
        if (it.hasNext()) {
            Feature feat = (Feature) it.next();
            return feat;
        }
        return null;
    }

    /**
     * @param envelope
     *            the envelope, which may have zero area
     * @return those features of the layer that intersect the given envelope; an
     *         empty FeatureCollection if no features intersect it
     * @throws Exception
     */

    public static Set<Feature> intersectingFeatures(Layer layer,
            Envelope envelope) throws Exception {
        HashSet<Feature> intersectingFeatures = new HashSet<Feature>();
        Geometry geom = EnvelopeUtil.toGeometry(envelope);

        List<Feature> candidateFeatures = layer.getFeatureCollectionWrapper()
                .query(geom.getEnvelopeInternal());

        for (Iterator<Feature> i = candidateFeatures.iterator(); i.hasNext();) {
            Feature feature = i.next();

            if (feature == null)
                continue;

            // optimization - if the feature envelope is completely inside the
            // query envelope
            // it must be selected
            if (feature.getGeometry() != null
                    && LayerViewPanel.intersects(feature.getGeometry(), geom)) {
                intersectingFeatures.add(feature);
            }
        }

        return intersectingFeatures;
    }

    /**
     * It indicates whether the coordinate corresponds to one end of the line
     * 
     * @param ls
     *            Line
     * @param c
     *            Coordinateenada
     * @return boolean - True if it is an extreme of the line
     */
    public static boolean isConectedToExtreme(LineString ls, Coordinate c) {
        Coordinate c1 = ls.getStartPoint().getCoordinate();
        Coordinate c2 = ls.getEndPoint().getCoordinate();
        return (c1.equals2D(c) || c2.equals2D(c));
    }

    /**
     * It divide the lineString from the indicated coordinate
     * 
     * @param line lineString to divide
     * @param coord Coordinates of the point to divide
     * @return LineString[] - two fragments of the Linestring
     */
    public static LineString[] divideLineString(LineString line, Coordinate coord) {

        Coordinate[] coordinates = line.getCoordinates();
        int i = 0;
        boolean enc = false;
        while (!enc && i < coordinates.length) {
            if (coordinates[i].equals(coord))
                enc = true;
            else
                i++;
        }
        if (enc) {
            Coordinate[] coordinatesA = new Coordinate[i + 1];
            Coordinate[] coordinatesB = new Coordinate[coordinates.length - i];
            System.arraycopy(coordinates, 0, coordinatesA, 0,
                    coordinatesA.length);
            System.arraycopy(coordinates, i, coordinatesB, 0,
                    coordinatesB.length);
            LineString[] lines = null;
            if (coordinatesA.length > 1 && coordinatesB.length > 1) {
                lines = new LineString[2];
                lines[0] = geomFac.createLineString(coordinatesA);
                lines[1] = geomFac.createLineString(coordinatesB);
            } else if (coordinatesB.length <= 1) {
                lines = new LineString[1];
                lines[0] = geomFac.createLineString(coordinatesA);
            } else if (coordinatesA.length <= 1) {
                lines = new LineString[1];
                lines[0] = geomFac.createLineString(coordinatesB);
            }
            return lines;
        }
        return null;
    }

    /**
     * Modified from Kosmo 2.0
     * 
     * @author Sergio Ba�os Calvo
     * @since Kosmo 1.0.0
     * 
     *        Add a coordinate to an array of coordinates in the closest segment
     *        (if it doesn't exist)
     * 
     * @param coordinatesTarget
     * @param cootdinateToAdd
     * @return
     */
    public static Coordinate[] addCoordinate(Coordinate[] coordinatesTarget,
            Coordinate cootdinateToAdd) {
        int startIndex = ArrayUtils.indexOf(coordinatesTarget, cootdinateToAdd);
        if (startIndex == ArrayUtils.INDEX_NOT_FOUND) {
            LineSegment startSegment = segmentInRange(
                    geomFac.createLineString(coordinatesTarget),
                    cootdinateToAdd);
            for (int i = 0; i < coordinatesTarget.length - 1; i++) {
                if (startSegment.getCoordinate(0)
                        .equals2D(coordinatesTarget[i])
                        && startSegment.getCoordinate(1).equals2D(
                                coordinatesTarget[i + 1])) {
                    return (Coordinate[]) ArrayUtils.add(coordinatesTarget,
                            i + 1, cootdinateToAdd);
                }
            }
        }
        return coordinatesTarget;
    }

    /**
     * It returns a list of possible routes (LineStrings) to get from the
     * initial coordinate to the finale, not depending to the lienString sense,
     * depending on whether it is closed or not
     * 
     * @param lineString
     * @param startCoordinate
     * @param endCoordinate
     * @return
     * @author Juan Jose Macias Moron
     */
    public static List<LineString> calculatePosiblePaths(
            final LineString lineString, final Coordinate startCoordinate,
            final Coordinate endCoordinate) {
        List<LineString> lineStringsList = new ArrayList<LineString>();
        if (lineString == null || startCoordinate == null
                || endCoordinate == null) {
            return lineStringsList;
        }
        Coordinate[] coordinates = lineString.getCoordinates();
        coordinates = addCoordinate(coordinates, startCoordinate);
        coordinates = addCoordinate(coordinates, endCoordinate);
        int startIndex = ArrayUtils.indexOf(coordinates, startCoordinate);
        int endIndex = ArrayUtils.indexOf(coordinates, endCoordinate);
        if (startIndex == ArrayUtils.INDEX_NOT_FOUND
                || endIndex == ArrayUtils.INDEX_NOT_FOUND) {
            return lineStringsList;
        }
        // Si el indice inicial es el mismo al final
        if (startIndex == endIndex) {
            // si la geometr�a es cerrada devuelco una sola linestring del
            // indice al final + del inicio(+1) al �ndice+1
            if (lineString instanceof LinearRing
                    || (lineString instanceof LineString && ((LineString) lineString)
                            .getStartPoint().equalsExact(
                                    ((LineString) lineString).getEndPoint()))) {
                Coordinate[] startPart = null;
                Coordinate[] endPart = null;
                startPart = (Coordinate[]) ArrayUtils.subarray(coordinates,
                        startIndex, coordinates.length);
                endPart = (Coordinate[]) ArrayUtils.subarray(coordinates, 1,
                        endIndex + 1);
                lineStringsList.add(geomFac
                        .createLineString((Coordinate[]) ArrayUtils.addAll(
                                startPart, endPart)));
            }
            // si la geometria NO es cerrada devuelvo lineStringsList vac�o
            return lineStringsList;
        }
        Coordinate[] coordLS = (Coordinate[]) ArrayUtils.subarray(coordinates,
                Math.min(startIndex, endIndex),
                Math.max(startIndex, endIndex) + 1);
        if (startIndex > endIndex) {
            ArrayUtils.reverse(coordLS);
        }
        lineStringsList.add(geomFac.createLineString(coordLS));

        if (lineString instanceof LinearRing
                || lineString.getStartPoint().equalsExact(
                        lineString.getEndPoint())) {
            Coordinate[] startPart = null;
            Coordinate[] endPart = null;
            if (startIndex > endIndex) {
                startPart = (Coordinate[]) ArrayUtils.subarray(coordinates,
                        startIndex, coordinates.length);
                endPart = (Coordinate[]) ArrayUtils.subarray(coordinates, 1,
                        endIndex + 1);
            } else {
                startPart = (Coordinate[]) ArrayUtils.subarray(coordinates, 0,
                        startIndex + 1);
                ArrayUtils.reverse(startPart);
                endPart = (Coordinate[]) ArrayUtils.subarray(coordinates,
                        endIndex, coordinates.length - 1);
                ArrayUtils.reverse(endPart);
            }
            lineStringsList.add(geomFac
                    .createLineString((Coordinate[]) ArrayUtils.addAll(
                            startPart, endPart)));
        }
        return lineStringsList;
    }

    /**
     * @param geometry
     * @return
     */
    public static Collection<LineString> extractLineStringCollection(
            final Geometry geometry) {
        ArrayList<LineString> lines = new ArrayList<LineString>();
        if (geometry instanceof LineString) {
            lines.add((LineString) geometry);
        } else if (geometry instanceof GeometryCollection) {
            GeometryCollection oldCollection = (GeometryCollection) geometry;
            for (int i = 0; i < oldCollection.getNumGeometries(); i++) {
                lines.addAll(extractLineStringCollection(oldCollection
                        .getGeometryN(i)));
            }
        } else if (geometry instanceof Polygon) {
            lines.addAll(extractLineStringCollection(((Polygon) geometry)
                    .getBoundary()));
        }
        return lines;
    }

    /**
     * It extracts from a geometryCollection the first geometry that has a valid
     * type defined as valid
     * 
     * @param geometryCollection
     * @param validGeometryTypes
     * @return
     */
    public static Geometry extractGeometryByType(
            final Geometry geometryCollection, final String[] validGeometryTypes) {
        if (geometryCollection == null || geometryCollection.isEmpty()) {
            return geometryCollection;
        }

        if (validGeometryTypes == null
                || ArrayUtils.isEmpty(validGeometryTypes)) {
            return null;
        }

        for (int i = 0; i < geometryCollection.getNumGeometries(); i++) {
            Geometry geom = geometryCollection.getGeometryN(i);
            for (int j = 0; j < validGeometryTypes.length; j++) {
                String type = validGeometryTypes[j];
                if (geom.getGeometryType().equals(type)) {
                    return geom;
                }
            }
        }
        return null;
    }

    /**
     * <p>
     * Metodo que crea nodos en las intersecciones una lista de geometrias. Solo
     * busca nodos 1 vez y actualiza todas las geometrias implicadas con
     * EXACTAMENTE EL MISMO nodo. Con lo que ya se puede poligonalizar sin
     * problemas, y los poligonos resultantes cumplen perfectamente todos los
     * predicados JTS sobre las geometrias nodadas.
     * </p>
     * <p>
     * Si se le pasa como parametro un nodeSet distinto de nulo lo rellena con
     * las coordenadas de los nodos encontrados. Tanto los nuevos a�adidos como
     * los que ya existian. Se consideran nodos los inicios y finales de lineas
     * cerradas.
     * </p>
     * <p>
     * Al ser un set solo puede contener una coordenada igual. Por defecto las
     * coordenadas son iguales en 2D (ignorando la z)
     * </p>
     * 
     * @param nodeSet
     * @param geometryList
     * @author Juan Jose Macias Moron
     */
    public static void permanentExactNoder(final List<Geometry> geometryList,
            final Set<Coordinate> nodeSet, double snappingTolerance) {
        if (geometryList == null || geometryList.isEmpty()) {
            return;
        }

        // First of all, snap all the vertexes together
        if (snappingTolerance > 0.0) {
            snapGeometries(geometryList, snappingTolerance);
        }

        // Extraer lista de CoordinateSequences con editor, editando todas las
        // geometrias
        List<CoordinateSequence> coordSeqList = generateCoordinateSequenceList(geometryList);

        // Generar lista de nodedSegments a partir de las CoordinateSequence,
        // usando como data la coordinateSequences
        List<NodedSegmentString> nodedSegmentsToNode = new ArrayList<NodedSegmentString>();
        for (CoordinateSequence coordinateSequence : coordSeqList) {
            nodedSegmentsToNode.add(new NodedSegmentString(coordinateSequence
                    .toCoordinateArray(), coordinateSequence));
        }

        // Procesar lista de nodedSegments, crear nodos
        MCIndexNoder mcIndexNoder = new MCIndexNoder();
        mcIndexNoder.setSegmentIntersector(new IntersectionAdder(
                new RobustLineIntersector()));
        mcIndexNoder.computeNodes(nodedSegmentsToNode);

        // Extraer mapa<CoordinateSequence, NodedCoordenates>.
        // Procesando cada NodedSegmentString usando data como key y como value
        // la secuencia con nodos
        Map<Object, Coordinate[]> nodedCoordinateSequenceMap = new HashMap<Object, Coordinate[]>();
        for (NodedSegmentString nodedSegmentString : nodedSegmentsToNode) {
            nodedCoordinateSequenceMap.put(nodedSegmentString.getData()
                    .toString(), processNodedSegmentString(nodedSegmentString));
            SegmentNodeList nodeList = nodedSegmentString.getNodeList();
            for (Iterator it = nodeList.iterator(); it.hasNext();) {
                SegmentNode segmentNode = (SegmentNode) it.next();
                if (nodeSet != null) {
                    nodeSet.add(segmentNode.coord);
                }
            }
        }

        // Contruir GeometryEditorOperation que use ese mapa y remplace las
        // sequencias
        // TODO ACTUALIZAR NUESTRO GEOMETRYEDITOR
        com.vividsolutions.jts.geom.util.GeometryEditor geomEditor = new com.vividsolutions.jts.geom.util.GeometryEditor(
                geomFac);
        CoordinateSequenceReplacerOperation csReplacerOp = new CoordinateSequenceReplacerOperation(
                nodedCoordinateSequenceMap);

        // Sustituir cada Geometria de la lista por su geometria editada
        for (int i = 0; i < geometryList.size(); i++) {
            Geometry geometry = geometryList.get(i);
            if (geometry == null || geometry.isEmpty()) {
                continue;
            }
            geometryList.set(i, geomEditor.edit(geometry, csReplacerOp));
        }
    }

    /**
     * <p>
     * Metodo que crea nodos en las intersecciones una lista de geometrias. Solo
     * busca nodos 1 vez y actualiza todas las geometrias implicadas con
     * EXACTAMENTE EL MISMO nodo. Con lo que ya se puede poligonalizar sin
     * problemas, y los poligonos resultantes cumplen perfectamente todos los
     * predicados JTS sobre las geometrias nodadas.
     * </p>
     * 
     * @param geometryList
     * @author Juan Jose Macias Moron
     */
    public static void permanentExactNoder(final List<Geometry> geometryList) {
        permanentExactNoder(geometryList, null, DEFAULT_SNAP_TOLERANCE);
    }

    /**
     * Hace exactamente lo mismo que el metodo permanentExactNoder
     * (sobrescribiendo la lista de geometrias con sus versiones nodadas) y
     * devuelve una coleccion de poligonos formados tras la poligonalizacion
     * 
     * @param nodeSet
     * @param geometryList
     *            lista de geometrias a nodar
     * @param nodeSet
     *            List of newly added nodes
     * @param snappingTolerance
     *            quantity of units to consider in the snapping algorithm (0.0
     *            to just don't do presnapping before the noder)
     * @return lista de poligonos formados tras la poligonalizacion
     * @author Juan Jose Macias Moron
     */
    public static Collection permanentExactNoderAndPolygonize(
            final List<Geometry> geometryList, final Set<Coordinate> nodeSet,
            double snappingTolerance) {
        // Nodamos la coleccion de geometrias iniciales
        permanentExactNoder(geometryList, nodeSet, snappingTolerance);

        // Extraemos lal lineas de las geometrias nodadas
        List<LineString> lineStringList = new ArrayList<LineString>();
        for (Geometry geometry : geometryList) {
            lineStringList.addAll(EditUtils
                    .extractLineStringCollection(geometry));
        }

        // Generamos la multiLineString (ya est� nodada)
        MultiLineString mls = geomFac.createMultiLineString(lineStringList
                .toArray(new LineString[] {}));
        Point mlsPt = geomFac.createPoint(mls.getCoordinate());
        Geometry nodedLines = mls.union(mlsPt);

        // Poligonizar usando las l�neas nodadas
        Polygonizer polygonizer = new Polygonizer();
        polygonizer.add(nodedLines);

        return polygonizer.getPolygons();
    }

    /**
     * Hace exactamente lo mismo que el metodo permanentExactNoder
     * (sobrescribiendo la lista de geometrias con sus versiones nodadas) y
     * devuelve una coleccion de poligonos formados tras la poligonalizacion
     * 
     * @param nodeSet
     * @param geometryList
     *            lista de geometrias a nodar
     * @param nodeSet
     *            List of newly added nodes
     * @param snappingTolerance
     *            quantity of units to consider in the snapping algorithm (0.0
     *            to just don't do presnapping before the noder)
     * @return Object with 4 Geometry collections (valid polygons, dangled
     *         lines, cut edges and invalid rings)
     * @author Sergio Ba�os Calvo
     */
    public static Collection[] permanentExactNoderAndPolygonizeCompletely(
            final List<Geometry> geometryList, final Set<Coordinate> nodeSet,
            double snappingTolerance) {
        // Nodamos la coleccion de geometrias iniciales
        permanentExactNoder(geometryList, nodeSet, snappingTolerance);

        // Extraemos lal lineas de las geometrias nodadas
        List<LineString> lineStringList = new ArrayList<LineString>();
        for (Geometry geometry : geometryList) {
            lineStringList.addAll(EditUtils
                    .extractLineStringCollection(geometry));
        }

        // Generamos la multiLineString (ya est� nodada)
        MultiLineString mls = geomFac.createMultiLineString(lineStringList
                .toArray(new LineString[] {}));
        Point mlsPt = geomFac.createPoint(mls.getCoordinate());
        Geometry nodedLines = mls.union(mlsPt);

        // Poligonizar usando las l�neas nodadas
        Polygonizer polygonizer = new Polygonizer();
        polygonizer.add(nodedLines);

        Collection[] result = new Collection[4];
        result[0] = polygonizer.getPolygons();
        result[1] = polygonizer.getDangles();
        result[2] = polygonizer.getCutEdges();
        result[3] = polygonizer.getInvalidRingLines();
        return result;
    }

    /**
     * Hace exactamente lo mismo que el metodo permanentExactNoder
     * (sobrescribiendo la lista de geometrias con sus versiones nodadas) y
     * devuelve una coleccion de poligonos formados tras la poligonalizacion
     * 
     * @param geometryList
     *            lista de geometrias a nodar
     * @return lista de poligonos formados tras la poligonalizacion
     * @author Juan Jose Macias Moron
     */
    public static Collection permanentExactNoderAndPolygonize(
            final List<Geometry> geometryList) {
        return permanentExactNoderAndPolygonize(geometryList, null,
                DEFAULT_SNAP_TOLERANCE);
    }

    /**
     * @param nodedSegmentString
     * @return
     * @author Juan Jose Macias Moron
     */
    private static Coordinate[] processNodedSegmentString(
            NodedSegmentString nodedSegmentString) {
        List<NodedSegmentString> resultEdgelist = new ArrayList<NodedSegmentString>();
        List segStrings = new ArrayList();
        segStrings.add(nodedSegmentString);
        NodedSegmentString.getNodedSubstrings(segStrings, resultEdgelist);
        List<Coordinate> finalCoordinateList = new ArrayList<Coordinate>();
        Coordinate lastCoord = null;
        for (NodedSegmentString subString : resultEdgelist) {
            Coordinate[] coordinates = subString.getCoordinates();
            for (int i = 0; i < coordinates.length; i++) {
                Coordinate coordinate = coordinates[i];
                // [SBCALVO - 07/01/2010] - Evitar puntos duplicados
                if (/* i == 0 && */coordinate.equals(lastCoord)) {
                    continue;
                }
                lastCoord = coordinate;
                finalCoordinateList.add(coordinate);
            }
        }
        return finalCoordinateList.toArray(new Coordinate[] {});
    }

    /**
     * @param geometryList
     * @return
     * @author Juan Jose Macias Moron
     */
    private static List<CoordinateSequence> generateCoordinateSequenceList(
            List<Geometry> geometryList) {
        // TODO ACTUALIZAR NUESTRO GEOMETRYEDITOR
        com.vividsolutions.jts.geom.util.GeometryEditor geomEditor = new com.vividsolutions.jts.geom.util.GeometryEditor(
                geomFac);
        CoordinateSequenceExtractorOperation csExtractorOp = new CoordinateSequenceExtractorOperation();
        for (Geometry geometry : geometryList) {
            geomEditor.edit(geometry, csExtractorOp);
        }
        return csExtractorOp.getCoordinateSequenceList();
    }

    /**
     * Snaps a list of geometries between them, to share all the vertexes that
     * are within a given tolerance
     * 
     * @param geometries
     *            List of input geometries
     * @param snappingTolerance
     *            Tolerance for the snapping algorithm
     */
    public static void snapGeometries(final List<Geometry> geometries,
            double snappingTolerance) {
        if (geometries.isEmpty()) {
            return;
        }
        for (int i = 0; i < geometries.size(); i++) {
            Geometry currentGeom = (Geometry) geometries.get(i).clone();
            // for( int j = 0; j < geometries.size(); j++ ) {
            // if (j == i) {
            // continue;
            // }
            for (int j = i + 1; j < geometries.size(); j++) {
                Geometry[] results = GeometrySnapper.snap(currentGeom,
                        geometries.get(j), snappingTolerance);
                currentGeom = results[0];
                geometries.set(j, results[1]);
            }
            geometries.set(i, currentGeom);
        }
    }

    public static void main(String[] args) {
        GeometryFactory fact = new GeometryFactory();
        WKTReader reader = new WKTReader();
        try {
            // Geometry geom1 =
            // reader.read("MULTIPOLYGON (((510076.35854872194 4729717.920412078, 509780.4128273819 4729396.607914623, 508969.28766300983 4729826.082228411, 508928 4730010, 508910 4730145, 508895 4730355, 508982 4730420, 509066 4730485, 509129 4730599, 509350 4730660, 509312 4730764, 509235 4730887, 509193.39 4730962.58, 509175 4730996, 509200 4731196, 509314 4731118, 509428 4731014, 509490.06 4730963.01, 509512 4730945, 509589 4730909, 509839 4730869, 510009 4730820, 510048 4730820, 510110.0880974055 4730808.203261493, 510524.50492675113 4730495.8348796, 510438.49212839734 4730346.529644722, 510575.79452267365 4730232.110982825, 510458.6704990494 4729991.105780368, 510318.9154467989 4730138.962574777, 510076.35854872194 4729717.920412078)))");
            // Geometry geom2 =
            // reader.read("MULTIPOLYGON (((510076.35854872194 4729717.920412078, 510524.50492675113 4730495.8348796, 510110.0880974055 4730808.203261493, 510048 4730820, 510009 4730820, 509839 4730869, 509589 4730909, 509512 4730945, 509490.06 4730963.01, 509428 4731014, 509314 4731118, 509200 4731196, 509175 4730996, 509193.39 4730962.58, 509235 4730887, 509312 4730764, 509350 4730660, 509129 4730599, 509066 4730485, 508982 4730420, 508895 4730355, 508910 4730145, 508928 4730010, 508969.28766300983 4729826.082228411, 509780.4128273819 4729396.607914623, 510076.35854872194 4729717.920412078)))");

            //Geometry geom1 = reader.read("POLYGON ((297 521, 1190 508, 328 529,297 521))"); 
            //Geometry geom2 = reader.read("POLYGON ((1189.2903624189241 507.3946527017896,1188.9611738697026 508.01144808875205,1188.6804973172084 507.3911875591662,1189.2903624189241 507.3946527017896))"); 
            Geometry geom1 = reader
                    .read("POLYGON ((547409.39 4704629.255, 547401.418 4704638.216, 547396.721 4704642.217, 547386.557 4704647.423, 547384.116 4704648.087, 547299.042 4704665.575, 547306.75 4704705.287, 547310.439 4704724.287, 547329.389 4704713.573, 547330.801 4704712.847, 547342.79 4704706.879, 547358.757 4704695.742, 547376.857 4704681.376, 547379.235 4704678.952, 547382.008 4704676.125, 547396.125 4704666.698, 547424.238 4704650.287, 547437.562 4704646.819, 547433.002 4704639.652, 547427.786 4704634.453, 547422.061 4704631.297, 547409.39 4704629.255))"); 
            Geometry geom2 = reader
                    .read("POLYGON ((546868.765 4704506.633, 546659.095 4704512.831, 546664.844 4704521.785, 546666.721 4704527.021, 546667.826 4704533.135, 546667.852 4704539.349, 546666.799 4704545.472, 546664.699 4704551.32, 546662.046 4704559.219, 546660.754 4704563.063, 546660.024 4704565.238, 546658.42 4704572.429, 546657.782 4704582.093, 546658.833 4704591.72, 546658.864 4704591.875, 546663.35 4704605.171, 546668.096 4704615.777, 546673.481 4704629.072, 546673.849 4704630.526, 546675.737 4704637.982, 546677.797 4704646.114, 546683.005 4704660.606, 546685.52 4704674.886, 546687.514 4704686.212, 546694.274 4704716.466, 546695.138 4704722.371, 546696.404 4704727.208, 546702.646 4704725.972, 546714.547 4704723.314, 546717.631 4704722.716, 546847.795 4704697.452, 546850.557 4704736.569, 546853.1 4704763.809, 546853.546 4704767.533, 546856.22 4704805.247, 546857.124 4704814.378, 546859.056 4704822.593, 546860.096 4704825.58, 546982.833 4704804.027, 546994.19 4704801.993, 547002.319 4704843.987, 547150.296 4704806.592, 547149.371 4704802.559, 547149.004 4704793.825, 547155.293 4704784.05, 547171.743 4704767.449, 547188.285 4704760.252, 547211.711 4704755.688, 547220.593 4704755.05, 547230.421 4704754.345, 547245.14 4704753.284, 547272.036 4704746, 547329.389 4704713.573, 547330.801 4704712.847, 547342.79 4704706.879, 547358.757 4704695.742, 547376.857 4704681.376, 547379.235 4704678.952, 547382.008 4704676.125, 547396.125 4704666.698, 547424.238 4704650.287, 547437.562 4704646.819, 547433.002 4704639.652, 547427.786 4704634.453, 547422.061 4704631.297, 547409.39 4704629.255, 547401.418 4704638.216, 547396.721 4704642.217, 547386.557 4704647.423, 547384.116 4704648.087, 547293.827 4704666.647, 547314.637 4704537.729, 547146.476 4704570.369, 547170.866 4704696.025, 546988.652 4704731.393, 546950.536 4704535.1, 547002.581 4704524.998, 547085.903 4704508.825, 546868.765 4704506.633))"); 
            List<Geometry> geometries = new ArrayList<Geometry>();
            geometries.add(geom1);
            geometries.add(geom2);

            // Try to snap them together and see the result
            snapGeometries(geometries, DEFAULT_SNAP_TOLERANCE);
            LOGGER.info(geometries.toString());
            geom1 = reader
                    .read("LINESTRING(547272.036 4704746, 547329.389 4704713.573)"); 
            geom2 = reader.read("POINT(547310.439 4704724.287)");
            LOGGER.info("Distancia = " + geom2.distance(geom1));
        } catch (ParseException e) {
            LOGGER.error("", e);
        }

    }

    /**
     * Calculate the area of a simple polygon in geodesics, not of
     * multipolygons, for the latter you will have to divide into simple
     * polygons and sum the results.
     * 
     * @param coordinates
     * @return �rea.
     */
    // public static double returnGeoCArea( List<Coordinate> coordinates ) {
    // double[] lat = new double[coordinates.size()];
    // double[] lon = new double[coordinates.size()];
    //
    // for( int K = 0; K < coordinates.size(); K++ ) {
    // Coordinate currentCoord = coordinates.get(K);
    // lon[K] = currentCoord.x / Geo.Degree;
    // lat[K] = currentCoord.y / Geo.Degree;
    // }
    // return (Geo.sphericalPolyArea(lat, lon, coordinates.size() - 1) *
    // Geo.SqM);
    // }

    /**
     * Obtains the area of a simple polygon for the given projection
     * 
     * @param polygon
     * @param proj
     * @return
     */
    // public static double areaWorld( Polygon polygon, IProjection proj ) {
    // double area = 0;
    // if (proj.isProjected()) {
    // area = polygon.getArea();
    // } else {
    // area = returnGeoCArea(Arrays.asList(polygon.getCoordinates()));
    // }
    //
    // return area;
    // }

    /**
     * Obtains the perimeter of a simple polygon for the given projection
     * 
     * @param polygon
     * @param proj
     * @return
     */
    // public static double perimeterWorld( Polygon polygon, IProjection proj )
    // {
    // double area = 0;
    // if (proj.isProjected()) {
    // area = polygon.getLength();
    // } else {
    // // FIXME: Find a way to calculate this value for projected SRSs
    // area = polygon.getLength();
    // }
    //
    // return area;
    // }

    /**
     * Removes points from
     * 
     * @param overlappingFeats
     * @param sourceGeometry
     * @param tolerance
     * @return
     */
    public static Geometry removePointsInTolerance(
            Collection<Feature> overlappingFeats, Geometry sourceGeometry,
            double tolerance) {

        Geometry result = (Geometry) sourceGeometry.clone();
        com.vividsolutions.jts.geom.util.GeometryEditor geomEditor = new com.vividsolutions.jts.geom.util.GeometryEditor(
                geomFac);
        CoordinateSequenceExtractorOperation csExtractorOp = new CoordinateSequenceExtractorOperation();
        geomEditor.edit(result, csExtractorOp);
        List<CoordinateSequence> sourceSequenceList = csExtractorOp
                .getCoordinateSequenceList();
        csExtractorOp = new CoordinateSequenceExtractorOperation();
        geomEditor.edit(sourceGeometry, csExtractorOp);
        List<CoordinateSequence> originalSequenceList = csExtractorOp
                .getCoordinateSequenceList();

        Map<Object, Coordinate[]> coordinateSequenceReplaceMap = new HashMap<Object, Coordinate[]>();
        for (Iterator<Feature> itFeats = overlappingFeats.iterator(); itFeats
                .hasNext();) {
            Feature currentFeat = itFeats.next();
            Geometry currentGeom = currentFeat.getGeometry();
            csExtractorOp = new CoordinateSequenceExtractorOperation();
            geomEditor.edit(currentGeom, csExtractorOp);
            List<CoordinateSequence> currentSequenceList = csExtractorOp
                    .getCoordinateSequenceList();
            for (Iterator itSequences = currentSequenceList.iterator(); itSequences
                    .hasNext();) {
                CoordinateSequence coordinateSequence = (CoordinateSequence) itSequences
                        .next();
                for (int i = 0; i < coordinateSequence.size(); i++) {
                    Coordinate currentCoord = coordinateSequence
                            .getCoordinate(i);
                    // Loop for the starting geometry, filtering out those
                    // coordinates that are in
                    // tolerance
                    boolean removingMode = false;
                    List<CoordinateSequence> replacerSequenceList = new ArrayList<CoordinateSequence>();
                    for (int j = 0; j < sourceSequenceList.size(); j++) {
                        CoordinateSequence currentSourceSequence = sourceSequenceList
                                .get(j);
                        List<Coordinate> coords = new ArrayList<Coordinate>();
                        Coordinate lastAddedCoordinate = null;
                        for (int k = 0; k < currentSourceSequence.size(); k++) {
                            Coordinate sourceCoord = currentSourceSequence
                                    .getCoordinate(k);
                            if (currentCoord.equals2D(sourceCoord)) {
                                if (lastAddedCoordinate == null
                                        || lastAddedCoordinate != null
                                        && !lastAddedCoordinate
                                                .equals2D(sourceCoord)) {
                                    coords.add(sourceCoord);
                                }
                                lastAddedCoordinate = sourceCoord;
                                removingMode = true;
                            } else {
                                boolean inTolerance = currentCoord
                                        .distance(sourceCoord) <= tolerance;
                                if (!removingMode) {
                                    if (!inTolerance) {
                                        coords.add(sourceCoord);
                                    } else {
                                        removingMode = true;
                                        coords.add(currentCoord);
                                    }
                                } else {
                                    if (!inTolerance) {
                                        removingMode = false;
                                        coords.add(sourceCoord);
                                    }
                                }
                            }
                        }
                        // Rebuild the coordinate sequence
                        if (!coords.get(0).equals2D(
                                coords.get(coords.size() - 1))) {
                            coords.add((Coordinate) coords.get(0).clone());
                        }
                        Coordinate[] coordArray = new Coordinate[coords.size()];
                        CoordinateSequence replacedCoordinates = new CoordinateArraySequence(
                                coords.toArray(coordArray));
                        replacerSequenceList.add(j, replacedCoordinates);

                    }
                    sourceSequenceList = replacerSequenceList;
                }
            }
        }
        // Populate the map
        for (int i = 0; i < originalSequenceList.size(); i++) {
            CoordinateSequence originalCoordinateSequence = originalSequenceList
                    .get(i);
            coordinateSequenceReplaceMap.put(originalCoordinateSequence
                    .toString(), sourceSequenceList.get(i).toCoordinateArray());
        }
        // Replace the coordinate sequences with the result
        CoordinateSequenceReplacerOperation csReplacerOp = new CoordinateSequenceReplacerOperation(
                coordinateSequenceReplaceMap);
        result = geomEditor.edit(result, csReplacerOp);

        return result;
    }

    /**
     * Remove the all the consecutive points from a given geometry that are in
     * tolerance with the next/s one/s
     * 
     * @param geom
     * @param tolerance
     * @return
     */
    public static Geometry removeConsecutivePointsInTolerance(Geometry geom,
            double tolerance) {
        Geometry result = (Geometry) geom.clone();
        com.vividsolutions.jts.geom.util.GeometryEditor geomEditor = new com.vividsolutions.jts.geom.util.GeometryEditor(
                geomFac);
        RemoveConsecutivePointsInToleranceOperation operation = new RemoveConsecutivePointsInToleranceOperation(
                tolerance);
        result = geomEditor.edit(result, operation);
        return result;
    }

    /**
     * Modifies the geometry if needed by adding vertexes to the segments that
     * are in tolerance from an existent one
     * 
     * @param startingGeom
     * @param tolerance
     * @return
     */
    public static Geometry segmentAndInsertVertexes(Geometry startingGeom,
            double tolerance) {
        Geometry result = (Geometry) startingGeom.clone();
        // Loop for the geometry vertexes and look for each vertex which
        // linesegments are in range
        // (if any)
        Map<String, List<LineSegment>> lineSegmentToEditedLineSegmentsMap = new HashMap<String, List<LineSegment>>();
        List<Coordinate[]> coordArrays = CoordinateArrays.toCoordinateArrays(
                result, false);
        for (Iterator<Coordinate[]> itArrays = coordArrays.iterator(); itArrays
                .hasNext();) {
            Coordinate[] coordinates = itArrays.next();
            for (int i = 0; i < coordinates.length; i++) {
                List<LineSegment> segmentsInRange = segmentsInRange(result,
                        coordinates[i], tolerance);
                if (!segmentsInRange.isEmpty()

                ) {
                    for (Iterator itSegmentsInRange = segmentsInRange
                            .iterator(); itSegmentsInRange.hasNext();) {
                        LineSegment currentLineSegment = (LineSegment) itSegmentsInRange
                                .next();
                        List<LineSegment> editedLineSegments = new ArrayList<LineSegment>();
                        editedLineSegments.add(new LineSegment(
                                currentLineSegment.getCoordinate(0),
                                coordinates[i]));
                        editedLineSegments.add(new LineSegment(coordinates[i],
                                currentLineSegment.getCoordinate(1)));
                        lineSegmentToEditedLineSegmentsMap.put(
                                currentLineSegment.toString(),
                                editedLineSegments);
                    }
                }
            }
        }

        // If there are segments in range for at least one geometry, we must
        // change the geometry and
        // insert this vertex in the
        // detected lineSegment
        if (!lineSegmentToEditedLineSegmentsMap.isEmpty()
        // MapUtils.isNotEmpty(lineSegmentToEditedLineSegmentsMap)
        ) {
            com.vividsolutions.jts.geom.util.GeometryEditor geomEditor = new com.vividsolutions.jts.geom.util.GeometryEditor(
                    geomFac);
            LineSegmentInRangeInsertVertexOperation operation = new LineSegmentInRangeInsertVertexOperation(
                    lineSegmentToEditedLineSegmentsMap);
            result = geomEditor.edit(result, operation);
        }
        return result;
    }

    /**
     * Returns the segments of geometry 'geometry' that are at a distance
     * 'tolerance' to the coordinate 'coordinate'
     * 
     * @param geometry
     *            - la geometr�a
     * @param target
     *            - la coordenada
     * @return
     */
    protected static List<LineSegment> segmentsInRange(Geometry geometry,
            Coordinate target, double tolerance) {
        List<LineSegment> inRange = new ArrayList<LineSegment>();
        List coordArrays = CoordinateArrays.toCoordinateArrays(geometry, false);
        for (Iterator i = coordArrays.iterator(); i.hasNext();) {
            Coordinate[] coordinates = (Coordinate[]) i.next();
            for (int j = 1; j < coordinates.length; j++) { // 1

                if (coordinates[j - 1].equals(target)
                        || coordinates[j].equals(target)) {
                    continue;
                }

                LineSegment candidate = new LineSegment(coordinates[j - 1],
                        coordinates[j]);
                if (candidate.distance(target) > tolerance) {
                    continue;
                }
                inRange.add(candidate);
            }
        }
        return inRange;
    }

    /**
     * Operator that, using edit, il fills a List<CoordinateSequence> that can
     * be used in other moments
     * 
     * @author Juan Jose Macias Moron
     * @since 1.3
     */
    public static class CoordinateSequenceExtractorOperation implements
            GeometryEditorOperation {

        private List<CoordinateSequence> coordinateSequenceList;

        public CoordinateSequenceExtractorOperation() {
            super();
            coordinateSequenceList = new ArrayList<CoordinateSequence>();
        }

        public void clearCoordinateSequenceList() {
            coordinateSequenceList = new ArrayList<CoordinateSequence>();
        }

        @Override
        public Geometry edit(Geometry geometry, GeometryFactory factory) {
            if (geometry != null
                    && !geometry.isEmpty()
                    && (geometry instanceof Point || geometry instanceof LineString)) {
                coordinateSequenceList.add(new CoordinateArraySequence(geometry
                        .getCoordinates()));
            }
            return geometry;
        }

        /**
         * @return the coordinateSequenceList
         */
        public List<CoordinateSequence> getCoordinateSequenceList() {
            return coordinateSequenceList;
        }
    }

    /**
     * it substitutes the coordinateSecuences of a geometry using
     * coordinateSequenceMap
     * 
     * @author Juan Jose Macias Moron
     * @since 1.3
     */
    public static class CoordinateSequenceReplacerOperation implements
            GeometryEditorOperation {

        private Map<Object, Coordinate[]> coordinateSequenceMap;

        public CoordinateSequenceReplacerOperation(
                Map<Object, Coordinate[]> coordSequenceMap) {
            super();
            this.coordinateSequenceMap = coordSequenceMap;
        }

        @Override
        public Geometry edit(Geometry geometry, GeometryFactory factory) {
            CoordinateSequence coordSeq = null;
            if (geometry != null) {
                coordSeq = new CoordinateArraySequence(
                        geometry.getCoordinates());
            }
            if (coordSeq == null) {
                return geometry;
            }
            Coordinate[] coordinates = coordinateSequenceMap.get(coordSeq
                    .toString());
            if (coordinates == null) {
                return geometry;
            }
            if (geometry instanceof Point && coordinates.length > 0) {
                return factory.createPoint(coordinates[0]);
            } else if (geometry instanceof LinearRing) {
                if (coordinates.length < 4) {
                    // Must complete the coordinate until creating a valid
                    // linearring
                    Coordinate[] newCoordinates = new Coordinate[4];
                    System.arraycopy(coordinates, 0, newCoordinates, 0,
                            coordinates.length);
                    for (int i = coordinates.length; i < 4; i++) {
                        newCoordinates[i] = coordinates[0];
                    }
                    return factory.createLinearRing(newCoordinates);
                }
                return factory.createLinearRing(coordinates);
            } else if (geometry instanceof LineString) {
                return factory.createLineString(coordinates);
            }
            return geometry;
        }

        /**
         * @return the coordinateSequenceMap
         */
        public Map<Object, Coordinate[]> getCoordinateSequenceMap() {
            return coordinateSequenceMap;
        }

        /**
         * @param coordSequenceMap
         *            the coordinateSequenceMap to set
         */
        public void setCoordinateSequenceMap(
                Map<Object, Coordinate[]> coordSequenceMap) {
            this.coordinateSequenceMap = coordSequenceMap;
        }
    }

    /**
     * Given a geometry, it replaces the linesegments present in the map for its
     * edited one
     * <p>
     * </p>
     * 
     * @author Sergio Ba�os Calvo - sbc@saig.es
     * @since 2.0
     */
    public static class LineSegmentInRangeInsertVertexOperation implements
            GeometryEditorOperation {
        private Map<String, List<LineSegment>> lineSegmentToEditedLineSegmentsMap;

        public LineSegmentInRangeInsertVertexOperation(
                Map<String, List<LineSegment>> map) {
            super();
            this.lineSegmentToEditedLineSegmentsMap = map;
        }

        @Override
        public Geometry edit(Geometry geometry, GeometryFactory factory) {
            if (geometry == null || geometry.isEmpty()) {
                return null;
            }
            if (geometry instanceof LinearRing) {
                // Extract its LineSegments and replace when necessary
                List<Coordinate[]> coordArrays = CoordinateArrays
                        .toCoordinateArrays(geometry, false);
                List<Coordinate[]> newCoordArrays = new ArrayList<Coordinate[]>();
                for (Iterator<Coordinate[]> itArrays = coordArrays.iterator(); itArrays
                        .hasNext();) {
                    Coordinate[] coordinates = itArrays.next();
                    List<Coordinate> coords = new ArrayList<Coordinate>();
                    for (int i = 0; i < coordinates.length; i++) {
                        LineSegment newSegment = null;
                        if (i == coordinates.length - 1) {
                            newSegment = new LineSegment(coordinates[i],
                                    coordinates[0]);
                        } else {
                            newSegment = new LineSegment(coordinates[i],
                                    coordinates[i + 1]);
                        }
                        if (lineSegmentToEditedLineSegmentsMap
                                .containsKey(newSegment.toString())) {
                            List<LineSegment> editedSegments = lineSegmentToEditedLineSegmentsMap
                                    .get(newSegment.toString());
                            for (Iterator<LineSegment> itSegments = editedSegments
                                    .iterator(); itSegments.hasNext();) {
                                LineSegment currentLineSegment = itSegments
                                        .next();
                                if (coords.size() > 0
                                        && !coords.get(coords.size() - 1)
                                                .equals(currentLineSegment
                                                        .getCoordinate(0))) {
                                    coords.add(currentLineSegment
                                            .getCoordinate(0));
                                }
                                coords.add(currentLineSegment.getCoordinate(1));
                            }
                        } else {
                            coords.add(coordinates[i]);
                        }
                    }
                    if (!coords.isEmpty()
                    // CollectionUtils.isNotEmpty(coords)
                    ) {
                        if (!coords.get(0)
                                .equals(coords.get(coords.size() - 1))) {
                            coords.add((Coordinate) coords.get(0).clone());
                        }
                        Coordinate[] array = new Coordinate[coords.size()];
                        newCoordArrays.add(coords.toArray(array));
                    }
                }
                List<Geometry> geoms = CoordinateArrays.fromCoordinateArrays(
                        newCoordArrays, geomFac);
                return geomFac.createLinearRing(geoms.get(0).getCoordinates());
            }
            return geometry;
        }
    }

    /**
     * Remove all the duplicated points from each vertex given a tolerance
     * <p>
     * </p>
     * 
     * @author Sergio Ba�os Calvo - sbc@saig.es
     * @since 2.0
     */
    public static class RemoveConsecutivePointsInToleranceOperation implements
            GeometryEditorOperation {
        private double tolerance;

        /**
         * @param tolerance
         */
        public RemoveConsecutivePointsInToleranceOperation(double tolerance) {
            super();
            this.tolerance = tolerance;
        }

        @Override
        public Geometry edit(Geometry geometry, GeometryFactory factory) {
            if (geometry == null || geometry.isEmpty()) {
                return null;
            }
            if (geometry instanceof LinearRing) {
                // Extract its LineSegments and replace when necessary
                List<Coordinate[]> coordArrays = CoordinateArrays
                        .toCoordinateArrays(geometry, false);
                List<Coordinate[]> newCoordArrays = new ArrayList<Coordinate[]>();
                for (Iterator<Coordinate[]> itArrays = coordArrays.iterator(); itArrays
                        .hasNext();) {
                    Coordinate[] coordinates = itArrays.next();
                    List<Coordinate> coords = new ArrayList<Coordinate>();
                    for (int i = 0; i < coordinates.length; i++) {
                        Coordinate currentCoord = coordinates[i];
                        coords.add(currentCoord);
                        if (i == coordinates.length - 1) {
                            continue;
                        }
                        Coordinate nextCoord = coordinates[i + 1];
                        while (currentCoord.distance(nextCoord) < tolerance) {
                            i++;
                            if (i == coordinates.length - 1) {
                                coords.add(coords.get(0));
                                break;
                            }
                            nextCoord = coordinates[i + 1];
                        }
                    }
                    if (!coords.isEmpty()
                    // CollectionUtils.isNotEmpty(coords)
                    ) {
                        Coordinate[] array = new Coordinate[coords.size()];
                        newCoordArrays.add(coords.toArray(array));
                    }
                }
                List<Geometry> geoms = CoordinateArrays.fromCoordinateArrays(
                        newCoordArrays, geomFac);
                return geomFac.createLinearRing(geoms.get(0).getCoordinates());
            }
            return geometry;
        }
    }

    /**
     * Analyzes the starting geometry and generate the necessary polygons from
     * it: when two vertexes are the same, it generates a new multipolygon part
     * from each one of the parts, in a recursive way
     * 
     * @param geom
     *            Starting geometry
     * @return
     */
    public static Geometry analyzeAndCleanPolygonalGeometry(Geometry geom) {
        if (!(geom instanceof MultiPolygon || geom instanceof Polygon)) {
            return geom;
        }
        List<Geometry> geomList = new ArrayList<Geometry>();
        // For each polygonal part
        for (int i = 0; i < geom.getNumGeometries(); i++) {
            Polygon currentPartGeom = (Polygon) geom.getGeometryN(i);
            // Treat the boundaries
            List<LinearRing> cleanedBoundaries = buildLinearRingsFromCoordArray(currentPartGeom
                    .getExteriorRing().getCoordinates());
            List<Geometry> cleanedGeometries = new ArrayList<Geometry>();
            for (LinearRing cleanedBoundary : cleanedBoundaries) {
                Polygon newPolygon = geomFac.createPolygon(cleanedBoundary,
                        null);
                cleanedGeometries.add(newPolygon);
            }
            Geometry currentPartCleaned = geomFac
                    .buildGeometry(cleanedGeometries);
            // Build a geometry without holes
            List<LinearRing> cleanedHoles = new ArrayList<LinearRing>();
            for (int j = 0; j < currentPartGeom.getNumInteriorRing(); j++) {
                LinearRing currentHole = (LinearRing) currentPartGeom
                        .getInteriorRingN(j);
                List<LinearRing> currentHolesCleaned = buildLinearRingsFromCoordArray(currentHole
                        .getCoordinates());
                for (LinearRing currentHoleCleaned : currentHolesCleaned) {
                    if (currentHoleCleaned.isValid()
                            && !currentHoleCleaned.isEmpty()) {
                        cleanedHoles.add(currentHoleCleaned);
                    }
                }
            }
            List<Geometry> cleanedHolesGeometries = new ArrayList<Geometry>();
            for (LinearRing cleanedHole : cleanedHoles) {
                Polygon newPolygon = geomFac.createPolygon(cleanedHole, null);
                cleanedHolesGeometries.add(newPolygon);
            }
            // Remove the holes from the boundary geom
            for (Geometry currentHole : cleanedHolesGeometries) {
                currentPartCleaned = EnhancedPrecisionOp.difference(
                        currentPartCleaned, currentHole);
            }
            geomList.add(currentPartCleaned);
        }
        // Build the new geometry from the coordinate array
        geom = geomFac.buildGeometry(geomList);
        return geom;
    }

    /**
     * Builds a list of LinearRings from the coordinate list. It groups the
     * coordinates between them
     * 
     * @param coords
     * @return
     */
    private static List<LinearRing> buildLinearRingsFromCoordArray(
            Coordinate[] coords) {
        List<LinearRing> geomList = new ArrayList<LinearRing>();
        List<Coordinate> coordinates = new ArrayList<Coordinate>();
        for (int i = 0; i < coords.length; i++) {
            Coordinate currentCoord = coords[i];
            coordinates.add((Coordinate) currentCoord.clone());
            int nextIndex = ArrayUtils.indexOf(coords, currentCoord, i + 1);
            if (i == 0 && nextIndex == coords.length - 1) {
                // Ignore them, are the same first and end coordinates
                continue;
            } else if (nextIndex != -1) {
                List<LinearRing> partGeoms = buildLinearRingsFromCoordArray((Coordinate[]) ArrayUtils
                        .subarray(coords, i, nextIndex + 1));
                for (LinearRing currentPart : partGeoms) {
                    if (currentPart.isValid() && !currentPart.isEmpty()) {
                        geomList.add(currentPart);
                    }
                }
                i = nextIndex;
            }
        }

        // Build the geometry from the coordinates
        if (coordinates.size() >= 4
                && coordinates.get(0).equals(
                        coordinates.get(coordinates.size() - 1))) {
            LinearRing linearRing = geomFac.createLinearRing(coordinates
                    .toArray(new Coordinate[coordinates.size()]));
            geomList.add(linearRing);
        }
        return geomList;
    }

    // The following code comes from
    // com.vividsolutions.jump.workbench.ui.plugin.clipboard.PastItemsPlugIn
    // method conform(Collection<Feature> features, FeatureSchema
    // targetFeatureSchema)
    /**
     * Transfer a feature schema of a layer to a collection of features This
     * method is used whenever a collection of features is cloned for further
     * transformation (move, mirror, etc)
     * 
     * @param features
     *            collection of features
     * @param targetFeatureSchema
     *            schema from the original layer
     * @return collection with feature schema
     */

    public static Collection<Feature> conformCollection(
            Collection<Feature> features, FeatureSchema targetFeatureSchema) {
        final ArrayList<Feature> featureCopies = new ArrayList<Feature>();

        for (Iterator<Feature> i = features.iterator(); i.hasNext();) {
            Feature feature = (Feature) i.next();
            featureCopies.add(conformFeature(feature, targetFeatureSchema));
        }
        return featureCopies;
    }

    /**
     * Transfer a feature schema of a layer to a feature. This method is used
     * whenever feature is cloned for further transformation (move, mirror, etc)
     * 
     * @param original
     * @param targetFeatureSchema
     *            schema from the original layer
     * @return feature with feature schema
     */

    public static Feature conformFeature(Feature original,
            FeatureSchema targetFeatureSchema) {
        // Transfer as many attributes as possible, matching on name. [Jon
        // Aquino]
        Feature copy = new BasicFeature(targetFeatureSchema);
        copy.setGeometry((Geometry) original.getGeometry().clone());

        for (int i = 0; i < original.getSchema().getAttributeCount(); i++) {
            if (i == original.getSchema().getGeometryIndex()) {
                continue;
            }
            String attributeName = original.getSchema().getAttributeName(i);

            if (!copy.getSchema().hasAttribute(attributeName)) {
                continue;
            }
            if (copy.getSchema().getAttributeType(attributeName) != original
                    .getSchema().getAttributeType(attributeName)) {
                continue;
            }
            // [mmichaud 2014-09-22] do not copy if target attribute is the
            // external PK
            if (copy.getSchema().getAttributeIndex(attributeName) != copy
                    .getSchema().getExternalPrimaryKeyIndex()) {
                copy.setAttribute(attributeName,
                        original.getAttribute(attributeName));
            }
        }
        if (original.getSchema().getCoordinateSystem() != CoordinateSystem.UNSPECIFIED
                && copy.getSchema().getCoordinateSystem() != CoordinateSystem.UNSPECIFIED) {
            Reprojector.instance().reproject(copy.getGeometry(),
                    original.getSchema().getCoordinateSystem(),
                    copy.getSchema().getCoordinateSystem());
        }
        return copy;
    }

    /**
     * Extends the given line the distance in each direction
     * 
     * @param inputLine
     * @param distance
     * @return
     */
    // public static LineString extendLine( LineString inputLine, double
    // distance ) {
    // LineString result = (LineString) inputLine.clone();

    // Expand from the beginning
    // Coordinate[] coords = result.getCoordinates();
    // Coordinate startCoord = coords[0];
    // Coordinate endCoord = coords[coords.length - 1];

    // if (!startCoord.equals2D(endCoord)) {

    // Point2d pFinal = new Point2d(startCoord.x, startCoord.y);
    // Point2d pInicial = new Point2d(coords[1].x, coords[1].y);
    // Point2d pInterpolado = new Point2d();
    // double pDistance = pInicial.distance(pFinal);
    // pInterpolado.interpolate(pInicial, pFinal, (pDistance + distance) /
    // pDistance);
    // if (!Double.isNaN(pInterpolado.x) && !Double.isNaN(pInterpolado.y)
    // && !Double.isInfinite(pInterpolado.x) &&
    // !Double.isInfinite(pInterpolado.y)) {
    // coords[0] = new Coordinate(pInterpolado.x, pInterpolado.y);
    // }

    // Expand from the end
    // pInicial = new Point2d(coords[coords.length - 2].x, coords[coords.length
    // - 2].y);
    // pFinal = new Point2d(endCoord.x, endCoord.y);
    // pInterpolado = new Point2d();
    // pDistance = pInicial.distance(pFinal);
    // pInterpolado.interpolate(pInicial, pFinal, (pDistance + distance) /
    // pDistance);

    // if (!Double.isNaN(pInterpolado.x) && !Double.isNaN(pInterpolado.y)
    // && !Double.isInfinite(pInterpolado.x) &&
    // !Double.isInfinite(pInterpolado.y)) {
    // coords[coords.length - 1] = new Coordinate(pInterpolado.x,
    // pInterpolado.y);
    // }
    // }

    // result = geomFac.createLineString(coords);
    //
    // return result;
    // }
}
