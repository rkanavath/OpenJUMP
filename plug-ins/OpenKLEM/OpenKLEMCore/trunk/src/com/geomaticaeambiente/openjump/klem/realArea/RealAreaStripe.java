package com.geomaticaeambiente.openjump.klem.realArea;

import java.io.IOException;
import java.util.concurrent.Callable;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.slope.SlopeCalculator;
import com.geomaticaeambiente.openjump.klem.slope.SlopeStripe.SlopeAlgo;
import com.geomaticaeambiente.openjump.klem.slope.SlopeStripe.SlopeUnits;

/**
 *
 * @author AdL
 */
public class RealAreaStripe implements Callable<DoubleBasicGrid> {

    public RealAreaStripe(int stripeEffectiveRows, DoubleBasicGrid Grid,
            int yOffset) {

        this.stripeEffectiveRows = stripeEffectiveRows;
        this.Grid = Grid;

    }

    @Override
    public DoubleBasicGrid call() throws Exception {
        return calcAspect();
    }

    private DoubleBasicGrid calcAspect() throws IOException {

        final int nRows = stripeEffectiveRows;
        final int nCols = Grid.getColumnCount();

        final DoubleBasicGrid realAreaGrid = new DoubleBasicGrid(
                new byte[nRows][nCols], Grid.getCellSize(), Grid.getNoData(),
                Grid.getLowerLeftCoord());

        DoubleBasicGrid slopeGrid = new DoubleBasicGrid(new byte[nRows][nCols],
                Grid.getCellSize(), Grid.getNoData(), Grid.getLowerLeftCoord());

        for (int row = 0; row < nRows; row++) {
            for (int col = 0; col < nCols; col++) {

                realAreaGrid.setValue(col, row, Grid.getNoData());

                final SlopeAlgo slopeAlgo = SlopeAlgo.HORN;
                final SlopeUnits slopeUnits = SlopeUnits.RADIANS;
                try {
                    final SlopeCalculator sc = new SlopeCalculator(Grid, null,
                            100d, slopeAlgo, slopeUnits);
                    slopeGrid = sc.calculate();
                } catch (final Exception e) {
                }

                //     final java.awt.Point cell = new java.awt.Point(col, row
                //             + yOffset);

                //      if (!slopeDegsGrid.isNoData(slopeDegsGrid.getValue(cell))) {

                final double area = Grid.getCellSize() * Grid.getCellSize();
                final double slopeRad = slopeGrid.getValue(col, row);

                final double realArea = area / Math.cos(slopeRad);

                if (!Double.isInfinite(realArea)) {
                    realAreaGrid.setValue(col, row, realArea);
                } else {
                    realAreaGrid.setValue(col, row, Grid.getNoData());
                }
                // hillshadeGrid.setValue(col, row,
                // Math.toDegrees(hillshade));

                //   }
            }
        }

        return realAreaGrid;

    }

    private final int stripeEffectiveRows;
    private final DoubleBasicGrid Grid;

}
