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
import java.util.Arrays;
import java.util.List;

import org.openjump.advancedtools.utils.EditUtils;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.Point;

/**
 * <p>
 * This class contains an algorithm that puts a vertex between two crossing
 * linestrings.
 * </p>
 * 
 * @author Gabriel Bellido P&eacute;rez - gbp@saig.es
 * @since Kosmo 1.2
 */
public class VertexInCrossingLinesAlgorithm {
    private GeometryFactory geomFact = new GeometryFactory();
    private List<LineString> geoms;
    private Coordinate[] intersectionCoords;

    public Coordinate[] getIntersectionCoords() {
        return intersectionCoords;
    }

    /**
     * put vertex in crossing lines. Return true if lines crosses and algorithm
     * could succesfully end.
     * 
     * @param line1
     * @param line2
     * @return
     */
    public boolean putVertexInCrossingLines(LineString line1, LineString line2) {
        if (line1 == null || line2 == null) {
            return false;
        }

        Geometry intersection = line1.intersection(line2);
        boolean resultIsPoint = intersection instanceof Point
                || intersection instanceof MultiPoint;
        if (resultIsPoint) {
            intersectionCoords = intersection.getCoordinates();
            geoms = new ArrayList<LineString>();
            LineString firstLine = line1;
            LineString secondLine = line2;
            for (Coordinate c : intersectionCoords) {
                firstLine = geomFact.createLineString(EditUtils.addCoordinate(
                        firstLine.getCoordinates(), c));
                secondLine = geomFact.createLineString(EditUtils.addCoordinate(
                        secondLine.getCoordinates(), c));
            }
            geoms.add(firstLine);
            geoms.add(secondLine);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Cut the lines crossing
     * 
     * @param line1
     * @param line2
     * @return
     */
    public boolean cutCrossingLines(LineString line1, LineString line2) {
        if (putVertexInCrossingLines(line1, line2)) {
            for (Coordinate c : intersectionCoords) {
                ArrayList<LineString> modified = new ArrayList<LineString>();
                ArrayList<LineString> result = new ArrayList<LineString>();
                for (LineString toDivide : geoms) {
                    if (hasVertex(c, toDivide)) {
                        LineString[] strings = EditUtils.divideLineString(toDivide,
                                c);
                        modified.add(toDivide);
                        result.addAll(Arrays.asList(strings));
                    }
                }
                geoms.removeAll(modified);
                geoms.addAll(result);
            }
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param c
     * @param toDivide
     * @return
     */
    private boolean hasVertex(Coordinate c, LineString toDivide) {
        for (Coordinate v : toDivide.getCoordinates()) {
            if (v.equals2D(c)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return Returns the firstLine.
     */
    public LineString getFirstLine() {
        return geoms.get(0);
    }

    /**
     * @return Returns the secondLine.
     */
    public LineString getSecondLine() {
        return geoms.get(1);
    }

    /**
     * return the result of cutting
     * 
     * @return
     */
    public List<LineString> getCuts() {
        return geoms;
    }

}
