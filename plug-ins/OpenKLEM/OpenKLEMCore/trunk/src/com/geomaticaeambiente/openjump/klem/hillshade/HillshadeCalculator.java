package com.geomaticaeambiente.openjump.klem.hillshade;

import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.GridDestriper;
import com.geomaticaeambiente.openjump.klem.grid.GridStriper;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 *
 * @author AdL
 */
public class HillshadeCalculator {
    
    /**
     * Calculates the hillshade
     * @param slopeDegsGrid
     * @param aspectDegsGrid
     * @param zenithDegs
     * @param azimuthDegs
     */
    public HillshadeCalculator(DoubleBasicGrid slopeDegsGrid, DoubleBasicGrid aspectDegsGrid,
            double zenithDegs, double azimuthDegs) {
        this.slopeDegsGrid = slopeDegsGrid;
        this.aspectDegsGrid = aspectDegsGrid;
        this.zenithDegs = zenithDegs;
        this.azimuthDegs = azimuthDegs;
    }
    
    /**
     * Calculates the aspect (in radians).
     * @return The aspect (radians).
     * @throws InterruptedException
     * @throws ExecutionException
     * @throws IOException 
     */
    public DoubleBasicGrid calculate() throws InterruptedException, ExecutionException, IOException {
        
        int rowCount = slopeDegsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        List<Callable<DoubleBasicGrid>> asepectToDos_l = new ArrayList<Callable<DoubleBasicGrid>>();
        
        DoubleBasicGrid[] hillshadeDegStripes = GridStriper.stripeDoubleGrid(slopeDegsGrid, stripeRowCount);
        
        /* Stripe */
        for(int s=0; s<stripeCount; s++) {

            int stripeEffectiveRows;
            if(s != stripeCount -1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }
            
            int offset = s * stripeRowCount;
            
            HillshadeStripe aspectStripe = new HillshadeStripe(
                    stripeEffectiveRows, slopeDegsGrid, aspectDegsGrid, offset,
                    zenithDegs, azimuthDegs);
            
            asepectToDos_l.add(aspectStripe);

        }   

        /* Calculate */
        List<Future<DoubleBasicGrid>> aspectFutures_l = execBuilder.getExecutorService().invokeAll(asepectToDos_l);
        List<DoubleBasicGrid> aspectGrids_l = new ArrayList<DoubleBasicGrid>();
        for(Future<DoubleBasicGrid> asepctFuture : aspectFutures_l) {
            aspectGrids_l.add(asepctFuture.get());
        }
        
        return GridDestriper.assembleDoubleStrips(aspectGrids_l);
        
    }
    
    private final DoubleBasicGrid slopeDegsGrid;
    private final DoubleBasicGrid aspectDegsGrid;
    private final double zenithDegs;
    private final double azimuthDegs;
    
}
