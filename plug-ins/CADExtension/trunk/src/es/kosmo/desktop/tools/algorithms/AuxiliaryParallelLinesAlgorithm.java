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

import java.util.Arrays;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;

/**
 * Creates parallel lines and expand them inside an envelope
 * <p>
 * </p>
 * 
 * @author Gabriel Bellido P&eacute;rez - gbp@saig.es
 * @since Kosmo 1.2
 */
public class AuxiliaryParallelLinesAlgorithm {

    private GeometryFactory geomFact = new GeometryFactory();

    ParallelLinesAlgorithm parallelLinesAlg = new ParallelLinesAlgorithm();

    /**
     * Calculates the parallel line with expanded borders
     * <p>
     * How it works: uses the old algorithm of parallel line and takes the last
     * and first segment of it, expands it larger than the screen width or
     * height and then clips it with the screen evelope.
     * </p>
     * 
     * @param selected
     * @param distance
     * @param startCoordinate
     * @param env
     * @return
     */
    public Geometry calculateParallelCurve(Geometry selected, double distance,
            Coordinate startCoordinate, Envelope env) {
        LineString parallelCurve = (LineString) parallelLinesAlg
                .calculateParallelCurve(selected, distance, startCoordinate);
        double lenght = Math.sqrt(env.getHeight() * env.getHeight()
                + env.getWidth() * env.getWidth());
        LineSegment firstSegment = getFirstSegment(parallelCurve);
        LineSegment expandedFirstSegment = normalize(firstSegment, lenght);
        Coordinate[] coordinates = Arrays.copyOf(
                parallelCurve.getCoordinates(),
                parallelCurve.getCoordinates().length);
        coordinates[0] = expandedFirstSegment.p1;

        LineSegment lastSegment = getLastSegment(parallelCurve);
        LineSegment expandedLastSegment = normalize(lastSegment, lenght);
        coordinates[coordinates.length - 1] = expandedLastSegment.p1;

        LineString lineString = geomFact.createLineString(coordinates);
        return lineString.intersection(geomFact.toGeometry(env));

    }

    /**
     * Expand the beginning of the line to the screen border
     * 
     * @param geom
     * @param env
     * @return
     */
    public Geometry expandFirstSegment(LineString line, Envelope env) {
        LineString geom = (LineString) line.clone();
        double lenght = Math.sqrt(env.getHeight() * env.getHeight()
                + env.getWidth() * env.getWidth());
        LineSegment firstSegment = getFirstSegment(geom);
        LineSegment expandedFirstSegment = normalize(firstSegment, lenght);
        Coordinate[] coordinates = Arrays.copyOf(geom.getCoordinates(),
                geom.getCoordinates().length);
        coordinates[0] = expandedFirstSegment.p1;
        LineString createLineString = geomFact.createLineString(coordinates);
        return createLineString.intersection(geomFact.toGeometry(env));
    }

    /**
     * Expand the end of the line to the screen border
     * 
     * @param geom
     * @param env
     * @return
     */
    public Geometry expandLastSegment(LineString line, Envelope env) {
        LineString geom = (LineString) line.clone();
        double lenght = Math.sqrt(env.getHeight() * env.getHeight()
                + env.getWidth() * env.getWidth());
        Coordinate[] coordinates = Arrays.copyOf(geom.getCoordinates(),
                geom.getCoordinates().length);
        LineSegment lastSegment = getLastSegment(geom);
        LineSegment expandedLastSegment = normalize(lastSegment, lenght);
        coordinates[coordinates.length - 1] = expandedLastSegment.p1;
        LineString createLineString = geomFact.createLineString(coordinates);
        return createLineString.intersection(geomFact.toGeometry(env));
    }

    /**
     * Expand the closest extreme of geom to c to the env border
     * 
     * @param geom
     *            line to expand
     * @param env
     *            envelope to cut
     * @param c
     * @return
     */
    public Geometry expandClosestEndSegment(LineString geom, Envelope env,
            Coordinate c) {
        double distanceStart = geom.getStartPoint().getCoordinate().distance(c);
        double distanceEnd = geom.getEndPoint().getCoordinate().distance(c);
        if (distanceStart > distanceEnd) {
            return expandLastSegment(geom, env);
        } else {
            return expandFirstSegment(geom, env);
        }
    }

    /**
     * Get the first segment of the linestring
     * 
     * @param parallelCurve
     * @return
     */
    private LineSegment getFirstSegment(LineString parallelCurve) {
        return new LineSegment(parallelCurve.getCoordinateN(1),
                parallelCurve.getCoordinateN(0));
    }

    /**
     * Get the last segment of the linestring
     * 
     * @param parallelCurve
     * @return
     */
    private LineSegment getLastSegment(LineString parallelCurve) {
        int i = parallelCurve.getNumPoints();
        return new LineSegment(parallelCurve.getCoordinateN(i - 2),
                parallelCurve.getCoordinateN(i - 1));
    }

    /**
     * Make the linesegment 1 of length
     * 
     * @param segment
     * @return
     */
    private LineSegment normalize(LineSegment segment, double lambda) {
        Coordinate coordinate = new Coordinate(segment.p1.x - segment.p0.x,
                segment.p1.y - segment.p0.y);
        double length = segment.getLength();
        Coordinate normalizedLenghts = new Coordinate((coordinate.x / length)
                * lambda, (coordinate.y / length) * lambda);
        return new LineSegment(segment.p0.x, segment.p0.y, segment.p0.x
                + normalizedLenghts.x, segment.p0.y + normalizedLenghts.y);
    }

}
