package com.geomaticaeambiente.openjump.klem.rastertools;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import org.nfunk.jep.JEP;

import com.geomaticaeambiente.openjump.klem.exceptions.NotSpatiallyConsistentGridsException;
import com.geomaticaeambiente.openjump.klem.grid.BasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel2.AbstractStripe2;
import com.geomaticaeambiente.openjump.klem.parallel2.ByteStripeGrid2;
import com.geomaticaeambiente.openjump.klem.parallel2.DoubleStripeGrid2;
import com.geomaticaeambiente.openjump.klem.parallel2.IntStripeGrid2;

/**
 *
 * @author AdL
 */
public class RasterComb extends AbstractStripe2 {

    public RasterComb(
            LinkedHashMap<String, DoubleStripeGrid2> rasterAndNames_m,
            String expression) {
        this.rasterAndNames_m = rasterAndNames_m;
        this.expression = expression;
    }

    @Override
    public DoubleBasicGrid call() throws Exception {
        return combine();
    }

    @Override
    protected DoubleStripeGrid2[] getDoubleStripeGridsToUpdate() {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    @Override
    protected IntStripeGrid2[] getIntStripeGridsToUpdate() {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    @Override
    protected ByteStripeGrid2[] getByteStripeGridsToUpdate() {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    @Override
    protected void plugUpdatedDoubleStripegrids(
            DoubleStripeGrid2[] newDoubleStripeGrids) {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    @Override
    protected void plugUpdatedIntStripegrids(IntStripeGrid2[] newIntStripeGrids) {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    @Override
    protected void plugUpdatedByteStripegrids(
            ByteStripeGrid2[] newByteStripeGrids) {
        throw new UnsupportedOperationException("Not supported yet."); // To
                                                                       // change
                                                                       // body
                                                                       // of
                                                                       // generated
                                                                       // methods,
                                                                       // choose
                                                                       // Tools
                                                                       // |
                                                                       // Templates.
    }

    private DoubleBasicGrid combine()
            throws NotSpatiallyConsistentGridsException, Exception {

        // Check spatial consistency of grids
        @SuppressWarnings("rawtypes")
        Iterator iter = rasterAndNames_m.values().iterator();
        DoubleStripeGrid2 grid = (DoubleStripeGrid2) iter.next();
        while (iter.hasNext()) {
            if (!grid.isSpatiallyConsistentWith((BasicGrid) iter.next())) {
                throw new NotSpatiallyConsistentGridsException();
            }
        }

        // Instantiate output grid
        final int rowCount = grid.getRowCount();
        final int columnCount = grid.getColumnCount();
        final DoubleBasicGrid outputGrid = new DoubleBasicGrid(
                new double[rowCount][columnCount], grid.getCellSize(),
                grid.getNoData(), grid.getLowerLeftCoord());

        // Instantiate JEP
        final JEP jep = new JEP();
        jep.addStandardConstants();
        jep.addStandardFunctions();

        // Add variables
        iter = rasterAndNames_m.keySet().iterator();
        while (iter.hasNext()) {
            final String rasterName = (String) iter.next();
            jep.addVariable(rasterName, rasterAndNames_m.get(rasterName));
        }

        // Parse expression
        jep.parseExpression(expression);

        // Evaluate output
        for (int r = 0; r < rowCount; r++) {
            for (int c = 0; c < columnCount; c++) {

                double outVal = outputGrid.getNoData();
                boolean allNoData = true;

                iter = rasterAndNames_m.keySet().iterator();
                int i = 0;
                while (iter.hasNext()) {
                    final String gridName = (String) iter.next();
                    grid = rasterAndNames_m.get(gridName);
                    if (!grid.isNoData(grid.getValue(c, r))) {
                        allNoData = false;
                    }
                    jep.setVarValue(gridName, grid.getValue(c, r));
                    i++;
                }
                if (!allNoData) {
                    try {
                        outVal = jep.getValue();
                    } catch (final Exception ex) {
                        throw new Exception(ex);
                    }
                }

                outputGrid.setValue(c, r, outVal);

            }
        }

        return outputGrid;

    }

    private final Map<String, DoubleStripeGrid2> rasterAndNames_m;
    private final String expression;

}
