package com.geomaticaeambiente.openjump.klem.realArea;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;

/**
 *
 * @author AdL
 */
public class RealAreaCalculator {

    /**
     * Calculates the hillshade
     * 
     * @param slopeDegsGrid
     * @param aspectDegsGrid
     * @param zenithDegs
     * @param azimuthDegs
     */
    public RealAreaCalculator(DoubleBasicGrid slopeDegsGrid) {
        this.slopeDegsGrid = slopeDegsGrid;

    }

    /**
     * Calculates the aspect (in radians).
     * 
     * @return The aspect (radians).
     * @throws InterruptedException
     * @throws ExecutionException
     * @throws IOException
     */
    public DoubleBasicGrid calculate() throws InterruptedException,
            ExecutionException, IOException {

        final int rowCount = slopeDegsGrid.getRowCount();
        final ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        final int stripeRowCount = execBuilder.getRowPerStripeCount();
        final int stripeCount = execBuilder.getStripeCount();

        final List<Callable<DoubleBasicGrid>> asepectToDos_l = new ArrayList<Callable<DoubleBasicGrid>>();

        /* Stripe */
        for (int s = 0; s < stripeCount; s++) {

            int stripeEffectiveRows;
            if (s != stripeCount - 1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);
            }

            final int offset = s * stripeRowCount;
            final RealAreaStripe aspectStripe = new RealAreaStripe(
                    stripeEffectiveRows, slopeDegsGrid, offset);

            asepectToDos_l.add(aspectStripe);

        }

        /* Calculate */
        final List<Future<DoubleBasicGrid>> aspectFutures_l = execBuilder
                .getExecutorService().invokeAll(asepectToDos_l);
        final List<DoubleBasicGrid> aspectGrids_l = new ArrayList<DoubleBasicGrid>();
        for (final Future<DoubleBasicGrid> asepctFuture : aspectFutures_l) {
            aspectGrids_l.add(asepctFuture.get());
        }

        return GridDestriper.assembleDoubleStrips(aspectGrids_l);

    }

    private final DoubleBasicGrid slopeDegsGrid;

}
