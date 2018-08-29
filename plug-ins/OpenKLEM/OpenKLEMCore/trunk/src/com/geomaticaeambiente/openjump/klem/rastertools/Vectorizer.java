package com.geomaticaeambiente.openjump.klem.rastertools;

import java.util.ArrayList;
import java.util.List;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.PackedCoordinateSequenceFactory;
import com.vividsolutions.jts.operation.union.CascadedPolygonUnion;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;
import com.vividsolutions.jump.feature.AttributeType;
import com.vividsolutions.jump.feature.BasicFeature;
import com.vividsolutions.jump.feature.Feature;
import com.vividsolutions.jump.feature.FeatureCollection;
import com.vividsolutions.jump.feature.FeatureDataset;
import com.vividsolutions.jump.feature.FeatureSchema;

/**
 *
 * @author deluca
 */
public class Vectorizer {

    public Vectorizer(DoubleBasicGrid grid, boolean multipolygons) {
        this.grid = grid;
        this.multipolygons = multipolygons;
    }

    @SuppressWarnings("unchecked")
    public FeatureCollection vectorizePolygons() {

        // Find unique values
        final double[] uniqueVals = Utils.findUniqueVals(grid);
        final int uniqueValsCount = uniqueVals.length;

        // Create

        arrAll = new ArrayList[uniqueValsCount];
        for (int i = 0; i < uniqueValsCount; i++) {
            arrAll[i] = new ArrayList<Polygon>();
        }

        final int nCols = grid.getColumnCount();
        final int nRows = grid.getRowCount();

        for (int r = 0; r < nRows; r++) {

            double oldVal = grid.getNoData();
            int cStart = 0;
            boolean started = false;
            for (int c = 0; c < nCols; c++) {
                final double val = grid.getValue(c, r);

                if (!grid.isNoData(val) && !started) {
                    cStart = c;
                    oldVal = val;
                    started = true;
                    continue;
                }
                if (started) {
                    if (grid.isNoData(val)) {
                        polygonise(grid, cStart, r, c - 1, oldVal, uniqueVals);
                        started = false;
                    } else {
                        if (val != oldVal) {
                            polygonise(grid, cStart, r, c - 1, oldVal,
                                    uniqueVals);
                            cStart = c;
                            oldVal = val;
                            started = true;
                        }
                        if (val == oldVal && c == nCols - 1) {
                            polygonise(grid, cStart, r, c, oldVal, uniqueVals);
                        }
                    }

                }
            }
        }

        // Collapse polygons
        final FeatureSchema featSchema = new FeatureSchema();
        featSchema.addAttribute("GEOMETRY", AttributeType.GEOMETRY);
        featSchema.addAttribute(ATTRIBUTE_NAME, AttributeType.INTEGER);

        // Create feature collection
        final FeatureCollection featColl = new FeatureDataset(featSchema);

        for (int i = 0; i < uniqueValsCount; i++) {

            Geometry geom = CascadedPolygonUnion.union(arrAll[i]);
            geom = DouglasPeuckerSimplifier.simplify(geom, 0);
            geom = TopologyPreservingSimplifier.simplify(geom, 00);

            if (multipolygons) {

                final Feature feature = new BasicFeature(featSchema);
                feature.setGeometry(geom);
                feature.setAttribute(1, uniqueVals[i]);
                featColl.add(feature);

            } else {

                // From multipolygons to single polygons
                for (int g = 0; g < geom.getNumGeometries(); g++) {
                    final Feature feature = new BasicFeature(featSchema);
                    feature.setGeometry(geom.getGeometryN(g));
                    feature.setAttribute(1, uniqueVals[i]);
                    featColl.add(feature);
                }

            }

        }

        return featColl;

    }

    private void polygonise(DoubleBasicGrid grid, int cStart, int r, int cEnd,
            double val, double[] uniqueVals) {

        final Coordinate[] coords = new Coordinate[5];
        final PackedCoordinateSequenceFactory pcsf = new PackedCoordinateSequenceFactory();
        final GeometryFactory geomFactory = new GeometryFactory();

        final int nRows = grid.getRowCount();
        final Coordinate llCorner = grid.getLowerLeftCoord();
        final double cellSize = grid.getCellSize();

        final double yurCorner = llCorner.y + (nRows * grid.getCellSize());

        // Polygonize
        coords[0] = new Coordinate(llCorner.x + cStart * cellSize, yurCorner
                - r * cellSize - cellSize);
        coords[1] = new Coordinate(llCorner.x + (cEnd + 1) * cellSize,
                coords[0].y);
        coords[2] = new Coordinate(coords[1].x, coords[0].y + cellSize);
        coords[3] = new Coordinate(coords[0].x, coords[2].y);
        coords[4] = coords[0];

        final CoordinateSequence cs = pcsf.create(coords);
        final LinearRing lr = new LinearRing(cs, geomFactory);
        final Polygon polygon = new Polygon(lr, null, geomFactory);
        arrAll[arrPos((int) val, uniqueVals)].add(polygon);

    }

    private static int arrPos(double valIn, double[] arrayIn) {

        int valOut = -9999;
        for (int i = 0; i < arrayIn.length; i++) {
            if (arrayIn[i] == valIn) {
                valOut = i;
                break;
            }
        }
        return valOut;
    }

    public static final String ATTRIBUTE_NAME = "Val";
    private final DoubleBasicGrid grid;
    private final boolean multipolygons;
    private List<Polygon>[] arrAll;

}
