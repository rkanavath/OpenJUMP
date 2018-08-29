package com.geomaticaeambiente.openjump.klem.hydrodistance;

import com.geomaticaeambiente.openjump.klem.Log;
import com.geomaticaeambiente.openjump.klem.grid.ByteBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.DoubleBasicGrid;
import com.geomaticaeambiente.openjump.klem.grid.FlowDirBasicGrid;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyCalculator;
import com.geomaticaeambiente.openjump.klem.parallel.DependencyStripe;
import com.geomaticaeambiente.openjump.klem.parallel.ExecutorBuilder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/**
 * Calculated the hydrological distance using parallel computing.
 * @author AdL
 */
public class HydroDistCalculator {

    public DoubleBasicGrid calcD8(FlowDirBasicGrid flowDirsGrid) throws Exception {

        if(flowDirsGrid == null) {
            throw new Exception ("The flow directions grid cannot be null.");
        }    
        
        /* Striping */
        int rowCount = flowDirsGrid.getRowCount();
        ExecutorBuilder execBuilder = new ExecutorBuilder(rowCount);
        int stripeRowCount = execBuilder.getRowPerStripeCount();
        int stripeCount = execBuilder.getStripeCount();
        
        /* Calculate dependency grid */
        if(Log.log) System.out.println("Calc dependency grid...");
        DependencyCalculator depGridCalculator = new DependencyCalculator(
                DependencyStripe.Origin.OUTLET, flowDirsGrid);
        ByteBasicGrid dependencyGrid = depGridCalculator.calculate();

        /* Calculate hydrological distance */
        if(Log.log) System.out.println("Calculate hydrological distance...");
        double noData = -9999;
        double[][] hydrologicalDistanceGrid = new double[flowDirsGrid.getRowCount()][flowDirsGrid.getColumnCount()];
        for(int r=0; r<hydrologicalDistanceGrid.length; r++) {
            for(int c=0; c<hydrologicalDistanceGrid[r].length; c++) {
                if(flowDirsGrid.isNoData(c, r)) {
                    hydrologicalDistanceGrid[r][c] = noData;
                }
            }
        }
        
        /* Striping */
        HydroDistStripe[] stripes = new HydroDistStripe[stripeCount];

        for(int s=0; s<stripes.length; s++) {
            
            int stripeEffectiveRows;
            if(s != stripes.length -1) {
                stripeEffectiveRows = stripeRowCount;
            } else {
                stripeEffectiveRows = rowCount - (s * stripeRowCount);                
            }
        
            byte[][] dependencyStripe = new byte[stripeEffectiveRows][flowDirsGrid.getColumnCount()];
        
            int offset = s * stripeRowCount;
            
            for(int r=0; r<stripeEffectiveRows; r++) {
                dependencyStripe[r] = Arrays.copyOf(dependencyGrid.getData()[r+offset], flowDirsGrid.getColumnCount());
            }
            
            stripes[s] = new HydroDistStripe(
                    s, flowDirsGrid, dependencyStripe,
                    hydrologicalDistanceGrid, offset);
            
        }

        /* Process stripes */
        if(Log.log) System.out.println("Process stripes...");
        boolean terminate = false;        
        while(!terminate) {
            
            // This part can be parallelized
            List<Callable<Boolean>> todos_l = new ArrayList<Callable<Boolean>>();
            
            for(int s=0; s<stripes.length; s++) {
                if(Log.log) System.out.println("Processing stripe: " + (s+1));
                todos_l.add(stripes[s]);
            }
            
            List<Future<Boolean>> futures_l = execBuilder.getExecutorService().invokeAll(todos_l);
            
            /* Swap buffers */
            if(stripes.length == 1) {
                break;
            }
            
            terminate = true;
            for(int s=0; s<stripes.length-1; s++) {
                
                byte[] s2above = stripes[s+1].getAboveBuffer();
                byte[] s1below = stripes[s].getBelowBuffer();                
                
                stripes[s].plugNewBelowBuffer(s2above);
                stripes[s+1].plugNewAboveBuffer(s1below);
                
                if(stripes[s].getQsCount() != 0 || stripes[s+1].getQsCount() != 0) {
                    terminate = false;
                }              
                
            }
            
        }

        return new DoubleBasicGrid(hydrologicalDistanceGrid, flowDirsGrid.getCellSize(),
                noData, flowDirsGrid.getLowerLeftCoord());
        
    }
   
    
    
}
