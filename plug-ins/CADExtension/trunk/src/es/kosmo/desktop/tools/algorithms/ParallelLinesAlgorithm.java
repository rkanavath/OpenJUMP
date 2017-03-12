/* 
 * Kosmo - Sistema Abierto de Información Geográfica
 * Kosmo - Open Geographical Information System
 *
 * http://www.saig.es
 * (C) 2011, SAIG S.L.
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
 * Sistemas Abiertos de Información Geográfica, S.L.
 * Avnda. República Argentina, 28
 * Edificio Domocenter Planta 2ª Oficina 7
 * C.P.: 41930 - Bormujos (Sevilla)
 * España / Spain
 *
 * Teléfono / Phone Number
 * +34 954 788876
 * 
 * Correo electrónico / Email
 * info@saig.es
 *
 */
package es.kosmo.desktop.tools.algorithms;

import java.util.ArrayList;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geomgraph.Position;

import es.axios.udig.ui.editingtools.precisionparallels.internal.OffsetBuilder;
import es.axios.udig.ui.editingtools.precisionparallels.internal.OffsetBuilder.OffsetPosition;

/**
 * <p>
 * Algorithms for making parallel lines
 * 
 *
 * </p>
 * 
 * @author Sergio Ba&ntilde;os Calvo
 * @since Kosmo 1.0.0
 */
public class ParallelLinesAlgorithm {

    protected static GeometryFactory geomFac = new GeometryFactory();

    /** */
    protected OffsetPosition offsetPosition;
    protected int startPosition;

    /**
     * Calculates the paralell curve
     * 
     * @param selected
     * @param distance
     * @param startCoordinate
     * @return
     */
    public Geometry calculateParallelCurve(Geometry selected, double distance,
            Coordinate startCoordinate) {

        Coordinate[] coords = selected.getCoordinates();
        calculateDistanceAndCurrentPosition(startCoordinate, coords);
        OffsetBuilder builder = new OffsetBuilder(offsetPosition, startPosition);
        ArrayList<Coordinate> outputCoordinates = builder.getLineCurve(coords,
                distance);

        LineString result = geomFac.createLineString(outputCoordinates
                .toArray(new Coordinate[outputCoordinates.size()]));

        return result;
    }

    /**
     * <p>
     * Calculate the distance between the initial point and its closest line
     * segment.
     * </p>
     * <p>
     * Calculate the distance of each segment of the reference line with the
     * provided point. Once it found the nearest segment, it compute the
     * orientation of the provided point.
     * </p>
     * <p>
     * Calculate the offsetPosition {@link OffsetBuilder.OffsetPosition}
     * <li>UPPER means that the generated offset curve will be outside the
     * reference line turn.</li>
     * <li>UNDER means that the generated offset curve will be inside the
     * reference line turn.</li>
     * </p>
     * <p>
     * Calculate the start position respect the reference line segment and its
     * direction.
     * <li>LEFT means that if you go from reference segment point 0 to point 1,
     * the start point is located at the left.</li>
     * <li>RIGHT means that if you go from reference segment point 0 to point 1,
     * the start point is located at the right.</li>
     * </p>
     * 
     * @return the distance
     */
    protected double calculateDistanceAndCurrentPosition(
            Coordinate initialCoordinate, Coordinate[] inputCoordinates) {

        LineSegment seg = new LineSegment();
        LineSegment closestSeg = new LineSegment();
        double distance, closestDistance = Double.MAX_VALUE;

        for (int i = 0; i < inputCoordinates.length - 1; i++) {
            seg.setCoordinates(inputCoordinates[i], inputCoordinates[i + 1]);
            distance = CGAlgorithms.distancePointLine(initialCoordinate,
                    inputCoordinates[i], inputCoordinates[i + 1]);
            if (distance < closestDistance) {
                closestDistance = distance;
                closestSeg = new LineSegment(inputCoordinates[i],
                        inputCoordinates[i + 1]);
            }
        }
        startPosition = Position.LEFT;
        int segmentOrientation = CGAlgorithms.computeOrientation(closestSeg.p0,
                closestSeg.p1, initialCoordinate);
        // offset position respect the first segment and the initial point.
        // Useful when is only one segment.
        if (segmentOrientation == -1) {
            offsetPosition = OffsetPosition.POSITION_UNDER;

        } else {
            offsetPosition = OffsetPosition.POSITION_UPPER;

        }

        int refLineOrientation, length;
        boolean outsideTurn = true;
        length = inputCoordinates.length;
        if (length > 2) {
            if (segmentOrientation == 1) {
                refLineOrientation = CGAlgorithms.computeOrientation(
                        inputCoordinates[0], inputCoordinates[1],
                        inputCoordinates[2]);
                outsideTurn = (refLineOrientation == CGAlgorithms.CLOCKWISE && 1 == Position.LEFT)
                        || (refLineOrientation == CGAlgorithms.COUNTERCLOCKWISE && 1 == Position.RIGHT);
                if (outsideTurn) {
                    offsetPosition = OffsetPosition.POSITION_UPPER;
                    startPosition = Position.LEFT;
                } else {
                    offsetPosition = OffsetPosition.POSITION_UNDER;
                    startPosition = Position.RIGHT;
                }
            } else {
                refLineOrientation = CGAlgorithms.computeOrientation(
                        inputCoordinates[length - 1],
                        inputCoordinates[length - 2],
                        inputCoordinates[length - 3]);
                outsideTurn = (refLineOrientation == CGAlgorithms.CLOCKWISE && 1 == Position.LEFT)
                        || (refLineOrientation == CGAlgorithms.COUNTERCLOCKWISE && 1 == Position.RIGHT);
                if (outsideTurn) {
                    offsetPosition = OffsetPosition.POSITION_UPPER;
                    startPosition = Position.RIGHT;
                } else {
                    offsetPosition = OffsetPosition.POSITION_UNDER;
                    startPosition = Position.LEFT;
                }
            }

        }
        return closestDistance;
    }

}
