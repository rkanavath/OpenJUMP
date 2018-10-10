package com.geomaticaeambiente.openjump.klem.realArea;

import java.io.IOException;
import java.util.concurrent.Callable;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;

/**
 *
 * @author AdL
 */
public class RealAreaStripe implements Callable<DoubleBasicGrid> {

    public RealAreaStripe(int stripeEffectiveRows,
            DoubleBasicGrid slopeDegsGrid, int yOffset) {

        this.stripeEffectiveRows = stripeEffectiveRows;
        this.slopeDegsGrid = slopeDegsGrid;
        this.yOffset = yOffset;

    }

    @Override
    public DoubleBasicGrid call() throws Exception {
        return calcAspect();
    }

    private DoubleBasicGrid calcAspect() throws IOException {

        final int nRows = stripeEffectiveRows;
        final int nCols = slopeDegsGrid.getColumnCount();

        final DoubleBasicGrid realAreaGrid = new DoubleBasicGrid(
                new byte[nRows][nCols], slopeDegsGrid.getCellSize(), -9999,
                slopeDegsGrid.getLowerLeftCoord());

        for (int row = 0; row < nRows; row++) {
            for (int col = 0; col < nCols; col++) {

                realAreaGrid.setValue(col, row, slopeDegsGrid.getNoData());

                final java.awt.Point cell = new java.awt.Point(col, row
                        + yOffset);

                if (!slopeDegsGrid.isNoData(slopeDegsGrid.getValue(cell))) {

                    final double area = slopeDegsGrid.getCellSize()
                            * slopeDegsGrid.getCellSize();
                    final double slopeRad = Math.toRadians(slopeDegsGrid
                            .getValue(col, row));

                    final double realArea = area / Math.cos(slopeRad);

                    if (!Double.isInfinite(realArea)) {
                        realAreaGrid.setValue(col, row, realArea);
                    } else {
                        realAreaGrid.setValue(col, row,
                                slopeDegsGrid.getNoData());
                    }
                    // hillshadeGrid.setValue(col, row,
                    // Math.toDegrees(hillshade));

                }
            }
        }

        return realAreaGrid;

    }

    private final int stripeEffectiveRows;
    private final DoubleBasicGrid slopeDegsGrid;

    private final int yOffset;

}
