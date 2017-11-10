package org.openjump.advancedtools.block;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.List;

import bsh.ParseException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTFileReader;
import com.vividsolutions.jump.geom.CoordUtil;

public class GeometryUtils {

    /**
     * Method to scale a selected geometry of a scale factor
     * 
     * @param input
     *            geometry
     * @param double. Scale factor (50 = half, 100 = no rescale, 200 = scale two
     *        times)
     */
    public static void scaleGeometry(Geometry geometry, final double scale) {
        final Coordinate center = geometry.getCentroid().getCoordinate();
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                coordinate.x = center.x + (scale / 100)
                        * (coordinate.x - center.x);
                coordinate.y = center.y + (scale / 100)
                        * (coordinate.y - center.y);
            }
        });
    }

    /**
     * Method to rotate a geometry of a defined angle
     * 
     * @param input
     *            geometry
     * @param angle
     *            in degree
     */
    public static void rotateGeometry(Geometry geometry, final double angle) {
        final Coordinate center = geometry.getCentroid().getCoordinate();
        double Deg2Rad = 0.0174532925199432;
        double radiansAngle = 0.0;
        radiansAngle = Deg2Rad * (-angle);
        final double cosAngle = Math.cos(radiansAngle);
        final double sinAngle = Math.sin(radiansAngle);
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {

                double x = coordinate.x - center.x;
                double y = coordinate.y - center.y;
                coordinate.x = center.x + (x * cosAngle) - (y * sinAngle);
                coordinate.y = center.y + (y * cosAngle) + (x * sinAngle);
            }
        });
    }

    /**
     * Method to counterclock wise rotate a geometry of a defined angle and
     * setting center of rotation
     * 
     * @param input
     *            geometry
     * @param angle
     *            in degree
     * @param coordinate
     *            center of rotation
     */

    public static void rotateGeometry(Geometry geometry, final double angle,
            final Coordinate center) {

        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double cosAngle = Math.cos(angle);
                double sinAngle = Math.sin(angle);
                double x = coordinate.x - center.y;
                double y = coordinate.y - center.x;
                coordinate.x = center.x + (x * cosAngle) + (y * sinAngle);
                coordinate.y = center.y + (y * cosAngle) - (x * sinAngle);
            }
        });
    }

    public static void rotate_Geometry(Geometry geometry, final double angle) {
        final Coordinate center = geometry.getCentroid().getCoordinate();
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double cosAngle = Math.cos(angle);
                double sinAngle = Math.sin(angle);
                double x = coordinate.x - center.y;
                double y = coordinate.y - center.x;
                coordinate.x = center.x + (x * cosAngle) + (y * sinAngle);
                coordinate.y = center.y + (y * cosAngle) - (x * sinAngle);
            }
        });
    }

    /**
     * Method to counterclock wise rotate a geometry of a defined angle
     * 
     * @param input
     *            geometry
     * @param angle
     *            in degree
     */
    public static void rotate_counterclockwise_Geometry(Geometry geometry,
            final double angle) {
        final Coordinate center = geometry.getCentroid().getCoordinate();
        double Deg2Rad = 0.0174532925199432;
        double radiansAngle = 0.0;
        radiansAngle = Deg2Rad * (-angle);
        final double cosAngle = Math.cos(radiansAngle);
        final double sinAngle = Math.sin(radiansAngle);
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double x = coordinate.x - center.x;
                double y = coordinate.y - center.y;
                coordinate.x = center.x + (x * cosAngle) - (y * sinAngle);
                coordinate.y = center.y + (y * cosAngle) + (x * sinAngle);
            }
        });
    }

    /**
     * Method to clock wise rotate a geometry of a defined angle
     * 
     * @param input
     *            geometry
     * @param angle
     *            in degree
     */
    public static void rotate_clockwise_Geometry(Geometry geometry,
            final double angle) {
        final Coordinate center = geometry.getCentroid().getCoordinate();
        double Deg2Rad = 0.0174532925199432;
        double radiansAngle = 0.0;
        radiansAngle = Deg2Rad * (-angle);
        final double cosAngle = Math.cos(radiansAngle);
        final double sinAngle = Math.sin(radiansAngle);
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double x = coordinate.x - center.x;
                double y = coordinate.y - center.y;
                coordinate.x = center.x + (x * cosAngle) - (y * sinAngle);
                coordinate.y = center.y + (y * cosAngle) + (x * sinAngle);
            }
        });
    }

    /**
     * Move a geometry to a defined coordinate
     * 
     * @param geometry
     * @param coordinate
     *            to move
     */
    public static void centerGeometry(final Geometry geometry,
            final Coordinate displacement) {
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                coordinate.setCoordinate(CoordUtil
                        .add(coordinate, displacement));
            }
        });
    }

    /**
     * Translate a geometry to a defined coordinate
     * 
     * @param geometry
     * @param coordinate
     *            to move
     */

    protected void translateGeometry(Geometry geometry,
            final Coordinate displacement) {

        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double x = coordinate.x + displacement.x;
                double y = coordinate.y + displacement.y;
                coordinate.x = x;
                coordinate.y = y;
            }
        });
    }

    /**
     * Mirror a geometry to a defined coordinate
     * 
     * @param geometry
     * @param coordinate
     *            to move
     */

    public static void mirrorY(Geometry geometry) {
        geometry.apply(new CoordinateFilter() {
            @Override
            public void filter(Coordinate coordinate) {
                double x = -coordinate.x;
                double y = coordinate.y;
                coordinate.x = x;
                coordinate.y = y;
            }
        });
    }

    /**
     * Reads a WKT file and returns the first geometry.
     * 
     * @param filenamedir
     * @return
     * @throws ParseException
     * @throws IOException
     * @throws com.vividsolutions.jts.io.ParseException
     */
    public static Geometry geom(String wktpath) throws ParseException,
            IOException, com.vividsolutions.jts.io.ParseException {
        com.vividsolutions.jts.io.WKTReader reader = new com.vividsolutions.jts.io.WKTReader();
        WKTFileReader fileReader = new WKTFileReader(wktpath, reader);
        @SuppressWarnings("rawtypes")
        List geomList = fileReader.read();
        if (geomList.size() == 1)
            return (Geometry) geomList.get(0);
        return null;
    }

    /**
     * Write a geometry to WKT file
     * 
     * @param geom
     * @param path
     * @throws IOException
     * @throws com.vividsolutions.jts.io.ParseException
     */
    public static void writeToFile(Geometry geom, String path)
            throws IOException, com.vividsolutions.jts.io.ParseException {
        String wkt = geom.toString();

        Writer writer = null;

        try {
            writer = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(path + ".wkt"), "utf-8"));
            writer.write("1:16:" + wkt);
        } catch (IOException ex) {
            // report
        } finally {
            try {
                writer.close();
            } catch (Exception ex) {/* ignore */
            }
        }
    }

}
